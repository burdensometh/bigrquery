test_that("publishes a manifest that lists the verified shards", {
  tb <- bq_table("proj", "ds", "table")
  listings <- 0
  written <- list()

  local_mocked_bindings(
    gs_object_list = function(bucket, prefix) {
      listings <<- listings + 1
      if (listings == 1) {
        return(tibble::tibble(
          name = character(),
          uri = character(),
          size = numeric(),
          generation = character()
        ))
      }
      tibble::tibble(
        name = c("part-0", "part-1"),
        uri = c(
          "gs://bkt/exports/table/run-1/part-0",
          "gs://bkt/exports/table/run-1/part-1"
        ),
        size = c(10, 20),
        generation = c("1", "2")
      )
    },
    bq_perform_extract = function(x, destination_uris, ...) {
      testthat::expect_equal(
        destination_uris,
        "gs://bkt/exports/table/run-1/part-*"
      )
      bq_job("proj", "job_1", "US")
    },
    bq_job_wait = function(x, quiet) invisible(x),
    bq_job_meta = function(x, fields) {
      list(statistics = list(extract = list(destinationUriFileCounts = list(2))))
    },
    gs_object_write = function(x, content, if_generation_match = NULL) {
      written[[length(written) + 1]] <<- list(
        object = format(x),
        content = jsonlite::fromJSON(content, simplifyVector = FALSE),
        if_generation_match = if_generation_match
      )
      TRUE
    }
  )

  out <- bq_table_save_manifest(
    tb,
    bucket = "bkt",
    prefix = "exports/table",
    run = "run-1"
  )

  expect_s3_class(out, "bq_export")
  expect_s3_class(out$job, "bq_job")
  expect_equal(out$status, "complete")
  expect_equal(out$objects$name, c("part-0", "part-1"))
  expect_equal(out$manifest_uri, "gs://bkt/exports/table/run-1/manifest.json")

  expect_length(written, 1)
  manifest <- written[[1]]
  expect_equal(manifest$object, out$manifest_uri)
  expect_equal(manifest$if_generation_match, 0)
  expect_equal(manifest$content$status, "complete")
  expect_equal(manifest$content$job$project, "proj")
  expect_equal(manifest$content$job$jobId, "job_1")
  expect_equal(manifest$content$sourceTable, "proj.ds.table")
  expect_equal(manifest$content$shardCount, 2)
  expect_equal(
    vapply(manifest$content$objects, `[[`, character(1), "uri"),
    out$objects$uri
  )
})

test_that("an empty batch is published as its own status", {
  tb <- bq_table("proj", "ds", "table")
  written <- list()

  local_mocked_bindings(
    gs_object_list = function(bucket, prefix) tibble::tibble(name = character(), uri = character(), size = numeric(), generation = character()),
    bq_perform_extract = function(x, destination_uris, ...) {
      bq_job("proj", "job_1", "US")
    },
    bq_job_wait = function(x, quiet) invisible(x),
    bq_job_meta = function(x, fields) {
      list(statistics = list(extract = list(destinationUriFileCounts = list(0))))
    },
    gs_object_write = function(x, content, if_generation_match = NULL) {
      written[[length(written) + 1]] <<- jsonlite::fromJSON(
        content,
        simplifyVector = FALSE
      )
      TRUE
    }
  )

  out <- bq_table_save_manifest(
    tb,
    bucket = "bkt",
    prefix = "exports/table",
    run = "run-empty"
  )

  expect_equal(out$status, "empty")
  expect_equal(nrow(out$objects), 0)
  expect_length(written, 1)
  expect_equal(written[[1]]$status, "empty")
  expect_equal(written[[1]]$objects, list())
})

test_that("an empty table with zero-byte shards is an empty batch", {
  tb <- bq_table("proj", "ds", "table")
  listings <- 0
  written <- 0L

  local_mocked_bindings(
    gs_object_list = function(bucket, prefix) {
      listings <<- listings + 1
      if (listings == 1) {
        return(tibble::tibble(
          name = character(),
          uri = character(),
          size = numeric(),
          generation = character()
        ))
      }
      tibble::tibble(
        name = "part-0",
        uri = "gs://bkt/exports/table/run-1/part-0",
        size = 0,
        generation = "1"
      )
    },
    bq_perform_extract = function(x, destination_uris, ...) {
      bq_job("proj", "job_1", "US")
    },
    bq_job_wait = function(x, quiet) invisible(x),
    bq_job_meta = function(x, fields) {
      list(statistics = list(extract = list(destinationUriFileCounts = list(1))))
    },
    gs_object_write = function(x, content, if_generation_match = NULL) {
      written <<- written + 1L
      TRUE
    }
  )

  out <- bq_table_save_manifest(tb, "bkt", prefix = "exports/table", run = "r")

  expect_equal(out$status, "empty")
  expect_equal(written, 1L)
})

test_that("a failed job publishes no manifest", {
  tb <- bq_table("proj", "ds", "table")
  write_called <- FALSE

  local_mocked_bindings(
    gs_object_list = function(bucket, prefix) tibble::tibble(name = character(), uri = character(), size = numeric(), generation = character()),
    bq_perform_extract = function(x, destination_uris, ...) {
      bq_job("proj", "job_1", "US")
    },
    bq_job_wait = function(x, quiet) {
      cli::cli_abort("Job failed", class = "bigrquery_job_failed")
    },
    gs_object_write = function(x, content, if_generation_match = NULL) {
      write_called <<- TRUE
      TRUE
    }
  )

  expect_snapshot(
    bq_table_save_manifest(tb, "bkt", prefix = "exports/table", run = "run-1"),
    error = TRUE
  )
  expect_false(write_called)
})

test_that("shards that don't match the job report publish no manifest", {
  tb <- bq_table("proj", "ds", "table")
  listings <- 0
  write_called <- FALSE

  local_mocked_bindings(
    gs_object_list = function(bucket, prefix) {
      listings <<- listings + 1
      if (listings == 1) {
        return(tibble::tibble(
          name = character(),
          uri = character(),
          size = numeric(),
          generation = character()
        ))
      }
      tibble::tibble(
        name = "part-0",
        uri = "gs://bkt/exports/table/run-1/part-0",
        size = 10,
        generation = "1"
      )
    },
    bq_perform_extract = function(x, destination_uris, ...) {
      bq_job("proj", "job_1", "US")
    },
    bq_job_wait = function(x, quiet) invisible(x),
    bq_job_meta = function(x, fields) {
      list(statistics = list(extract = list(destinationUriFileCounts = list(2))))
    },
    gs_object_write = function(x, content, if_generation_match = NULL) {
      write_called <<- TRUE
      TRUE
    }
  )

  expect_snapshot(
    bq_table_save_manifest(tb, "bkt", prefix = "exports/table", run = "run-1"),
    error = TRUE
  )
  expect_false(write_called)
})

test_that("a run that already contains objects is refused", {
  tb <- bq_table("proj", "ds", "table")
  performed <- FALSE

  local_mocked_bindings(
    gs_object_list = function(bucket, prefix) {
      tibble::tibble(
        name = "manifest.json",
        uri = "gs://bkt/exports/table/run-1/manifest.json",
        size = 100,
        generation = "1"
      )
    },
    bq_perform_extract = function(x, destination_uris, ...) {
      performed <<- TRUE
      bq_job("proj", "job_1", "US")
    }
  )

  expect_snapshot(
    bq_table_save_manifest(tb, "bkt", prefix = "exports/table", run = "run-1"),
    error = TRUE
  )
  expect_false(performed)
})

test_that("an existing manifest is never overwritten", {
  tb <- bq_table("proj", "ds", "table")
  listings <- 0

  local_mocked_bindings(
    gs_object_list = function(bucket, prefix) {
      listings <<- listings + 1
      if (listings == 1) {
        return(tibble::tibble(
          name = character(),
          uri = character(),
          size = numeric(),
          generation = character()
        ))
      }
      tibble::tibble(
        name = "part-0",
        uri = "gs://bkt/exports/table/run-1/part-0",
        size = 10,
        generation = "1"
      )
    },
    bq_perform_extract = function(x, destination_uris, ...) {
      bq_job("proj", "job_1", "US")
    },
    bq_job_wait = function(x, quiet) invisible(x),
    bq_job_meta = function(x, fields) {
      list(statistics = list(extract = list(destinationUriFileCounts = list(1))))
    },
    gs_object_write = function(x, content, if_generation_match = NULL) {
      cli::cli_abort("Precondition failed", class = "bigrquery_http_412")
    }
  )

  expect_snapshot(
    bq_table_save_manifest(tb, "bkt", prefix = "exports/table", run = "run-1"),
    error = TRUE
  )
})

test_that("re-running writes to a fresh run prefix", {
  tb <- bq_table("proj", "ds", "table")
  seen <- character()

  local_mocked_bindings(
    gs_object_list = function(bucket, prefix) {
      seen <<- c(seen, prefix)
      tibble::tibble(name = character(), uri = character(), size = numeric(), generation = character())
    },
    bq_perform_extract = function(x, destination_uris, ...) {
      bq_job("proj", "job_1", "US")
    },
    bq_job_wait = function(x, quiet) invisible(x),
    bq_job_meta = function(x, fields) {
      list(statistics = list(extract = list(destinationUriFileCounts = list(0))))
    },
    gs_object_write = function(x, content, if_generation_match = NULL) TRUE
  )

  first <- bq_table_save_manifest(tb, "bkt", prefix = "exports")
  second <- bq_table_save_manifest(tb, "bkt", prefix = "exports")

  expect_false(first$run == second$run)
  expect_equal(unique(seen), c(first$object_prefix, second$object_prefix))
})

test_that("manifests can be read back", {
  manifest_json <- jsonlite::toJSON(
    list(
      run = "run-1",
      status = "complete",
      job = list(project = "proj", jobId = "job_1", location = "US"),
      sourceTable = "proj.ds.table",
      bucket = "bkt",
      objectPrefix = "exports/table/run-1/",
      destinationUriPattern = "gs://bkt/exports/table/run-1/part-*",
      shardCount = 1,
      objects = list(list(
        name = "part-0",
        uri = "gs://bkt/exports/table/run-1/part-0",
        size = 10,
        generation = "1"
      )),
      manifestUri = "gs://bkt/exports/table/run-1/manifest.json",
      createdAt = "2026-01-01T00:00:00Z"
    ),
    auto_unbox = TRUE
  )

  local_mocked_bindings(
    gs_object_read = function(x, token) as.character(manifest_json)
  )

  out <- bq_export_read("gs://bkt/exports/table/run-1/manifest.json")

  expect_s3_class(out, "bq_export")
  expect_equal(out$status, "complete")
  expect_equal(as.character(out$job), "proj.job_1.US")
  expect_equal(toString(out$table), "proj.ds.table")
  expect_equal(out$objects$uri, "gs://bkt/exports/table/run-1/part-0")
  expect_equal(out$created_at, "2026-01-01T00:00:00Z")
})

test_that("invalid manifests are rejected", {
  local_mocked_bindings(
    gs_object_read = function(x, token) '{"status": "partial"}'
  )
  expect_snapshot(bq_export_read("gs://bkt/manifest.json"), error = TRUE)

  local_mocked_bindings(
    gs_object_read = function(x, token) '{"status": "complete"}'
  )
  expect_snapshot(bq_export_read("gs://bkt/manifest.json"), error = TRUE)
})

test_that("validates its arguments", {
  tb <- bq_table("proj", "ds", "table")

  expect_snapshot(bq_table_save_manifest(tb, "gs://bkt"), error = TRUE)
  expect_snapshot(bq_table_save_manifest(tb, "bkt", run = "a/b"), error = TRUE)
  expect_snapshot(
    bq_table_save_manifest(tb, "bkt", file_name = "part-0"),
    error = TRUE
  )
  expect_snapshot(
    bq_table_save_manifest(tb, "bkt", file_name = "a/part-*.json"),
    error = TRUE
  )
  expect_snapshot(
    bq_table_save_manifest(tb, "bkt", destination_uris = "gs://bkt/x-*"),
    error = TRUE
  )
})

test_that("has a useful print method", {
  export <- new_bq_export(
    status = "complete",
    job = bq_job("proj", "job_1", "US"),
    table = bq_table("proj", "ds", "table"),
    bucket = "bkt",
    run = "run-1",
    object_prefix = "exports/table/run-1/",
    destination_uri_pattern = "gs://bkt/exports/table/run-1/part-*",
    objects = tibble::tibble(
      name = c("part-0", "part-1"),
      uri = c(
        "gs://bkt/exports/table/run-1/part-0",
        "gs://bkt/exports/table/run-1/part-1"
      ),
      size = c(10, 20),
      generation = c("1", "2")
    ),
    manifest = gs_object("bkt", "exports/table/run-1/manifest.json")
  )
  expect_snapshot(export)
})

test_that("object prefixes are normalised", {
  expect_equal(normalize_object_prefix(NULL), "")
  expect_equal(normalize_object_prefix("exports"), "exports/")
  expect_equal(normalize_object_prefix("/exports/"), "exports/")
  expect_equal(normalize_object_prefix(""), "")
})

test_that("gs_object_list paginates and sorts", {
  pages <- list(
    list(
      items = list(list(
        name = "b",
        uri = "gs://bkt/p/b",
        size = "2",
        generation = "2"
      )),
      nextPageToken = "next"
    ),
    list(
      items = list(list(
        name = "a",
        uri = "gs://bkt/p/a",
        size = "1",
        generation = "1"
      ))
    )
  )
  pages_seen <- 0

  local_mocked_bindings(
    bq_token = function() httr::config(),
    process_request = function(req, raw = FALSE) {
      pages_seen <<- pages_seen + 1
      pages[[pages_seen]]
    }
  )

  out <- gs_object_list("bkt", "p/")
  expect_equal(out$name, c("a", "b"))
  expect_equal(out$size, c(1, 2))
  expect_equal(pages_seen, 2)
})