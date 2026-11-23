catalog_dir <- function() {
  path <- tempfile("bq-catalog-")
  list(path = path, cleanup = function() unlink(path, recursive = TRUE))
}

mock_page <- function(tables, next_token = NULL) {
  list(
    tables = lapply(tables, function(t) {
      pieces <- strsplit(t, ".", fixed = TRUE)[[1]]
      list(
        tableReference = list(
          projectId = pieces[1],
          datasetId = pieces[2],
          tableId = pieces[3]
        ),
        type = "TABLE"
      )
    }),
    nextPageToken = next_token
  )
}

# A mutable log of API queries; environments keep their contents across the
# by-value closure created by local_mocked_bindings().
call_log <- function() {
  env <- new.env(parent = emptyenv())
  env$items <- list()
  env
}

# Returns a mock bq_get() that serves pages and records every query it was
# called with, so tests can assert which pageToken resumed from.
mock_paged_get <- function(pages, log) {
  i <- 0L
  function(url, ..., query = NULL, token = NULL) {
    i <<- i + 1L
    log$items[[length(log$items) + 1L]] <- query
    if (i > length(pages)) {
      stop("test mock exhausted")
    }
    pages[[i]]
  }
}

http_condition <- function(status, reason) {
  cli::cli_abort(
    paste0("server error [", reason, "]"),
    class = c(
      paste0("bigrquery_", reason),
      paste0("bigrquery_http_", status)
    ),
    reason = reason,
    status = status
  )
}

test_that("bq_catalog() creates the on-disk layout", {
  d <- catalog_dir()
  on.exit(d$cleanup())

  catalog <- bq_catalog(d$path)
  expect_s3_class(catalog, "bq_catalog")
  expect_true(dir.exists(file.path(d$path, "state")))
  expect_true(dir.exists(file.path(d$path, "versions")))
  expect_true(file.exists(file.path(d$path, "catalog.json")))

  # Reopening an existing catalog keeps it rather than resetting it.
  expect_identical(bq_catalog(d$path)$path, catalog$path)
  root <- jsonlite::fromJSON(
    file.path(d$path, "catalog.json"),
    simplifyVector = FALSE
  )
  expect_equal(root$version, 1L)
  expect_null(root$current_version)
})

test_that("pagination position is saved per project and dataset identity", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  pages <- list(
    mock_page(c("p.ds.a", "p.ds.b"), next_token = "token-1"),
    mock_page(c("p.ds.c"), next_token = "token-2"),
    mock_page(c("p.ds.d"))
  )
  log <- call_log()
  local_mocked_bindings(bq_get = mock_paged_get(pages, log))

  bq_catalog_dataset_tables(catalog, "p", "ds", page_size = 2)

  state_path <- file.path(d$path, "state", "p__ds.json")
  expect_true(file.exists(state_path))
  state <- jsonlite::fromJSON(state_path, simplifyVector = FALSE)
  expect_equal(state$status, "complete")
  expect_null(state$next_page_token)
  expect_equal(state$pages_read, 3L)
  expect_equal(
    vapply(state$tables, function(x) x$table, character(1)),
    c("a", "b", "c", "d")
  )
  # The first request starts without a token; later pages carry the token
  # returned by the previous response.
  expect_null(log$items[[1]]$pageToken)
  expect_equal(log$items[[2]]$pageToken, "token-1")
  expect_equal(log$items[[3]]$pageToken, "token-2")
})

test_that("an interrupted run resumes from the saved next page token", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  log <- call_log()
  failing_get <- function(url, ..., query = NULL, token = NULL) {
    log$items[[length(log$items) + 1L]] <- query
    switch(length(log$items),
      mock_page(c("p.ds.a", "p.ds.b"), next_token = "token-1"),
      stop("connection reset")
    )
  }
  local_mocked_bindings(bq_get = failing_get)
  expect_error(
    bq_catalog_dataset_tables(catalog, "p", "ds"),
    "connection reset"
  )

  # The checkpoint after page one is on disk even though the run aborted.
  state <- jsonlite::fromJSON(
    file.path(d$path, "state", "p__ds.json"),
    simplifyVector = FALSE
  )
  expect_equal(state$status, "in_progress")
  expect_equal(state$next_page_token, "token-1")
  expect_equal(
    vapply(state$tables, function(x) x$table, character(1)),
    c("a", "b")
  )

  # A fresh process (new catalog object on the same path) must not restart at
  # page one: it asks for token-1 and only sees the remaining tables.
  catalog2 <- bq_catalog(d$path)
  log2 <- call_log()
  local_mocked_bindings(
    bq_get = mock_paged_get(list(mock_page(c("p.ds.c"))), log2)
  )
  bq_catalog_dataset_tables(catalog2, "p", "ds")
  expect_equal(log2$items[[1]]$pageToken, "token-1")

  finished <- jsonlite::fromJSON(
    file.path(d$path, "state", "p__ds.json"),
    simplifyVector = FALSE
  )
  expect_equal(finished$status, "complete")
  expect_equal(
    vapply(finished$tables, function(x) x$table, character(1)),
    c("a", "b", "c")
  )
})

test_that("changing request parameters invalidates the saved position", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  log <- call_log()
  local_mocked_bindings(
    bq_get = mock_paged_get(
      list(mock_page(c("p.ds.a"), next_token = "token-1")),
      log
    )
  )
  expect_error(bq_catalog_dataset_tables(catalog, "p", "ds", page_size = 10))
  expect_null(log$items[[1]]$pageToken)

  # Resuming with a different page size is a different request: the old
  # token must never be reused, so enumeration restarts at page one.
  local_mocked_bindings(
    bq_get = mock_paged_get(list(mock_page(c("p.ds.a2"))), log)
  )
  bq_catalog_dataset_tables(catalog, "p", "ds", page_size = 25)
  second_call <- log$items[[length(log$items)]]
  expect_null(second_call$pageToken)
  expect_equal(second_call$maxResults, 25)

  state <- jsonlite::fromJSON(
    file.path(d$path, "state", "p__ds.json"),
    simplifyVector = FALSE
  )
  expect_equal(
    vapply(state$tables, function(x) x$table, character(1)),
    "a2"
  )
})

test_that("an expired page token restarts the listing once", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  log <- call_log()
  local_mocked_bindings(
    bq_get = mock_paged_get(
      list(mock_page(c("p.ds.old"), next_token = "stale-token")),
      log
    )
  )
  expect_error(bq_catalog_dataset_tables(catalog, "p", "ds"))

  local_mocked_bindings(
    bq_get = function(url, ..., query = NULL, token = NULL) {
      log$items[[length(log$items) + 1L]] <- query
      if (!is.null(query$pageToken)) {
        stop(http_condition(400, "invalid"))
      }
      mock_page(c("p.ds.fresh"))
    }
  )
  expect_snapshot(
    bq_catalog_dataset_tables(catalog, "p", "ds")
  )
  # First resume attempt used the stale token; the restart used no token.
  expect_equal(log$items[[2]]$pageToken, "stale-token")
  expect_equal(log$items[[3]]$pageToken, "stale-token")
  expect_null(log$items[[4]]$pageToken)

  state <- jsonlite::fromJSON(
    file.path(d$path, "state", "p__ds.json"),
    simplifyVector = FALSE
  )
  expect_equal(state$status, "complete")
  expect_equal(
    vapply(state$tables, function(x) x$table, character(1)),
    "fresh"
  )
})

test_that("permission denied is marked, not treated as an empty dataset", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  local_mocked_bindings(
    bq_get = function(url, ..., query = NULL, token = NULL) {
      stop(http_condition(403, "accessDenied"))
    }
  )
  # The 403 is recorded rather than propagated, so a multi-dataset run can
  # keep going.
  expect_no_warning(bq_catalog_dataset_tables(catalog, "p", "locked"))

  state <- jsonlite::fromJSON(
    file.path(d$path, "state", "p__locked.json"),
    simplifyVector = FALSE
  )
  expect_equal(state$status, "access_denied")
  expect_equal(state$error$reason, "accessDenied")
  expect_length(state$tables, 0)

  status <- bq_catalog_status(catalog)
  expect_equal(status$status, "access_denied")
  expect_equal(status$reason, "accessDenied")

  # Re-running does not clear the marker and does not hit the API again.
  hit <- 0L
  local_mocked_bindings(
    bq_get = function(...) {
      hit <<- hit + 1L
      stop(http_condition(403, "accessDenied"))
    }
  )
  bq_catalog_dataset_tables(catalog, "p", "locked")
  expect_equal(hit, 0L)
})

test_that("non-403 errors still propagate", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  local_mocked_bindings(
    bq_get = function(url, ..., query = NULL, token = NULL) {
      stop(http_condition(500, "internalError"))
    }
  )
  expect_error(
    bq_catalog_dataset_tables(catalog, "p", "ds"),
    class = "bigrquery_http_500"
  )
})

test_that("committing builds a version from completed datasets only", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  local_mocked_bindings(
    bq_get = mock_paged_get(
      list(mock_page(c("p.open.a", "p.open.b"))),
      call_log()
    )
  )
  bq_catalog_dataset_tables(catalog, "p", "open")

  local_mocked_bindings(
    bq_get = function(url, ..., query = NULL, token = NULL) {
      stop(http_condition(403, "accessDenied"))
    }
  )
  bq_catalog_dataset_tables(catalog, "p", "locked")

  # A dataset still mid-listing blocks the commit by default.
  half_log <- call_log()
  local_mocked_bindings(
    bq_get = function(url, ..., query = NULL, token = NULL) {
      half_log$items[[length(half_log$items) + 1L]] <- query
      switch(length(half_log$items),
        mock_page(c("p.half.x"), next_token = "t"),
        stop(http_condition(500, "internalError"))
      )
    }
  )
  expect_error(
    bq_catalog_dataset_tables(catalog, "p", "half"),
    class = "bigrquery_http_500"
  )
  expect_snapshot(bq_catalog_commit(catalog), error = TRUE)

  bq_catalog_commit(catalog, only_complete = FALSE)
  root <- jsonlite::fromJSON(
    file.path(d$path, "catalog.json"),
    simplifyVector = FALSE
  )
  expect_equal(root$current_version, 1L)
  expect_true(file.exists(file.path(d$path, "versions", "1.json")))
  # The in-progress dataset is absent; complete and denied ones are present.
  expect_setequal(names(root$datasets), c("p__open", "p__locked"))
  expect_false(root$datasets$p__open$access_denied)
  expect_equal(
    vapply(
      root$datasets$p__open$tables,
      function(x) x$table,
      character(1)
    ),
    c("a", "b")
  )
  expect_true(root$datasets$p__locked$access_denied)

  version <- bq_catalog_version(catalog)
  expect_equal(version$id, 1L)
  expect_null(bq_catalog_version(bq_catalog(tempfile())))
})

test_that("access denied retains tables from the previous version", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  local_mocked_bindings(
    bq_get = mock_paged_get(
      list(mock_page(c("p.ds.hist1", "p.ds.hist2"))),
      call_log()
    )
  )
  bq_catalog_dataset_tables(catalog, "p", "ds")
  bq_catalog_commit(catalog)

  # Permissions are revoked before the next scheduled run.
  local_mocked_bindings(
    bq_get = function(url, ..., query = NULL, token = NULL) {
      stop(http_condition(403, "accessDenied"))
    }
  )
  # Force re-enumeration so the denied state is produced in this run too.
  unlink(file.path(d$path, "state", "p__ds.json"))
  bq_catalog_dataset_tables(catalog, "p", "ds")
  bq_catalog_commit(catalog)

  tables <- bq_catalog_tables(catalog, "p", "ds")
  expect_true(attr(tables, "access_denied"))
  expect_equal(tables$table, c("hist1", "hist2"))
  expect_equal(nrow(tables), 2L)
})

test_that("a genuinely empty dataset is distinct from a denied one", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  local_mocked_bindings(
    bq_get = mock_paged_get(list(mock_page(character())), call_log())
  )
  bq_catalog_dataset_tables(catalog, "p", "empty")
  bq_catalog_commit(catalog)

  tables <- bq_catalog_tables(catalog, "p", "empty")
  expect_false(attr(tables, "access_denied"))
  expect_equal(nrow(tables), 0L)
  expect_s3_class(tables, "data.frame")
})

test_that("duplicate tables across pages are deduplicated", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  pages <- list(
    mock_page(c("p.ds.a", "p.ds.b"), next_token = "t"),
    mock_page(c("p.ds.b", "p.ds.c"))
  )
  local_mocked_bindings(bq_get = mock_paged_get(pages, call_log()))
  bq_catalog_dataset_tables(catalog, "p", "ds")

  state <- jsonlite::fromJSON(
    file.path(d$path, "state", "p__ds.json"),
    simplifyVector = FALSE
  )
  expect_equal(
    vapply(state$tables, function(x) x$table, character(1)),
    c("a", "b", "c")
  )
})

test_that("bq_catalog_status reports every tracked dataset", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  expect_equal(nrow(bq_catalog_status(catalog)), 0L)

  local_mocked_bindings(
    bq_get = mock_paged_get(list(mock_page(c("p.a.t1"))), call_log())
  )
  bq_catalog_dataset_tables(catalog, "p", "a")
  local_mocked_bindings(
    bq_get = function(url, ..., query = NULL, token = NULL) {
      stop(http_condition(403, "accessDenied"))
    }
  )
  bq_catalog_dataset_tables(catalog, "p", "b")

  status <- bq_catalog_status(catalog)
  expect_equal(status$dataset, c("a", "b"))
  expect_equal(status$status, c("complete", "access_denied"))
  expect_equal(status$n_tables, c(1L, 0L))
  expect_equal(status$pages_read, c(1L, 0L))
})

test_that("project dataset enumeration is also resumable", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)
  log <- call_log()
  project_page <- function(ids, next_token = NULL) {
    list(
      datasets = lapply(ids, function(id) {
        list(datasetReference = list(projectId = "p", datasetId = id))
      }),
      nextPageToken = next_token
    )
  }
  local_mocked_bindings(
    bq_get = mock_paged_get(
      list(project_page(c("ds1", "ds2"), next_token = "pt")),
      log
    )
  )
  expect_error(bq_catalog_project_datasets(catalog, "p"))

  local_mocked_bindings(
    bq_get = mock_paged_get(list(project_page(c("ds3"))), log)
  )
  bq_catalog_project_datasets(catalog, "p")
  expect_equal(log$items[[2]]$pageToken, "pt")

  state <- jsonlite::fromJSON(
    file.path(d$path, "state", "p.json"),
    simplifyVector = FALSE
  )
  expect_equal(state$status, "complete")
  expect_equal(
    vapply(state$tables, function(x) x$dataset, character(1)),
    c("ds1", "ds2", "ds3")
  )
})

test_that("print method summarises dataset states", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)
  redact_path <- function(x) gsub(d$path, "<CATALOG>", x, fixed = TRUE)

  expect_snapshot(print(catalog), transform = redact_path)

  local_mocked_bindings(
    bq_get = mock_paged_get(list(mock_page(c("p.a.t1"))), call_log())
  )
  bq_catalog_dataset_tables(catalog, "p", "a")

  expect_snapshot(print(catalog), transform = redact_path)
})

test_that("project-level discovery state is not part of a version", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  local_mocked_bindings(
    bq_get = mock_paged_get(
      list(
        list(
          datasets = list(
            list(datasetReference = list(
              projectId = "p",
              datasetId = "ds"
            ))
          )
        )
      ),
      call_log()
    )
  )
  bq_catalog_project_datasets(catalog, "p")
  local_mocked_bindings(
    bq_get = mock_paged_get(list(mock_page(character())), call_log())
  )
  bq_catalog_dataset_tables(catalog, "p", "ds")
  bq_catalog_commit(catalog)

  root <- jsonlite::fromJSON(
    file.path(d$path, "catalog.json"),
    simplifyVector = FALSE
  )
  expect_equal(names(root$datasets), "p__ds")
  # Project discovery state must not appear as a dataset row.
  expect_equal(bq_catalog_status(catalog)$dataset, "ds")
})

test_that("a stale lock left by a crashed run is taken over", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  # Simulate a crashed process: a lock directory whose owner file is older
  # than the stale threshold.
  lock <- file.path(d$path, "state", "p__ds.json.lock")
  dir.create(lock, recursive = TRUE)
  writeLines("{}", file.path(lock, "owner.json"))
  old <- Sys.time() - bigrquery:::catalog_lock_stale_secs - 60
  Sys.setFileTime(file.path(lock, "owner.json"), old)

  local_mocked_bindings(
    bq_get = mock_paged_get(list(mock_page(c("p.ds.a"))), call_log())
  )
  expect_snapshot(
    bq_catalog_dataset_tables(catalog, "p", "ds")
  )
  expect_false(dir.exists(lock))
})

test_that("unknown and uncommitted datasets give a clear error", {
  d <- catalog_dir()
  on.exit(d$cleanup())
  catalog <- bq_catalog(d$path)

  expect_snapshot(
    bq_catalog_tables(catalog, "p", "missing"),
    error = TRUE
  )
})
