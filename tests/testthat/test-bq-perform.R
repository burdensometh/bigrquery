test_that("bq_perform_upload creates job that succeeds", {
  withr::local_options(cli.progress_show_after = Inf)

  bq_mtcars <- bq_test_table()
  job <- bq_perform_upload(bq_mtcars, mtcars)

  expect_s3_class(job, "bq_job")
  expect_snapshot(
    {
      bq_job_wait(job, quiet = FALSE)
      bq_job_wait(job, quiet = FALSE)
    },
    transform = \(x) gsub("[0-9.]+ kB", "<xxx> kB", x)
  )

  expect_true(bq_table_exists(bq_mtcars))
})

test_that("bq_perform_upload preserves numerical precision", {
  tb <- bq_test_table()
  bq_table_upload(tb, data.frame(dbl = pi))
  df <- bq_table_download(tb)
  expect_identical(df$dbl, pi)
})

test_that("bq_perform_upload preserves microsecond precision", {
  tbl <- data.frame(psx = .POSIXct(0.123456))
  attr(tbl$psx, "tzone") <- "UTC"

  ds <- bq_test_table()
  bq_table_upload(ds, tbl)
  tbl2 <- bq_table_download(ds)

  expect_equal(tbl$psx, tbl2$psx)
})

test_that("bq_perform_upload correclty assigns tzone", {
  # the bug only produced problems when the system TZ has a non-zero
  # offset (i.e., not UTC)
  withr::local_envvar(TZ = "America/New_York")

  ds <- bq_test_table()
  df <- data.frame(
    null = .POSIXct(0, tz = NULL),
    utc = .POSIXct(0, tz = "UTC"),
    ny = .POSIXct(0, tz = "America/New_York"),
    nz = .POSIXct(0, tz = "Pacific/Auckland")
  )
  bq_table_upload(ds, df)
  db <- bq_table_download(ds)

  expect_equal(db$utc, .POSIXct(0, tz = "UTC"))
  expect_equal(db$null, .POSIXct(0, tz = "UTC"))
  expect_equal(db$ny, .POSIXct(0, tz = "UTC"))
  expect_equal(db$nz, .POSIXct(0, tz = "UTC"))
})

# job_id ------------------------------------------------------------------

test_that("bq_perform_upload uses the supplied job_id", {
  tb <- bq_test_table()
  job_id <- random_name()

  job <- bq_perform_upload(tb, mtcars, job_id = job_id)
  expect_equal(job$job, job_id)
  bq_job_wait(job)

  # A retry adopts the existing job instead of creating a new one
  job2 <- bq_perform_upload(tb, mtcars, job_id = job_id)
  expect_equal(job2$job, job_id)
  bq_job_wait(job2)
})

test_that("uploading twice with the same job_id doesn't duplicate rows", {
  tb <- bq_test_table()
  df <- data.frame(x = 1:3)
  job_id <- random_name()

  bq_table_upload(tb, df, job_id = job_id)
  expect_equal(bq_table_nrow(tb), 3)

  # Simulates a retry after the first attempt's response was lost
  bq_table_upload(tb, df, job_id = job_id)
  expect_equal(bq_table_nrow(tb), 3)
})

test_that("a job_id can't be reused for a different table", {
  tb1 <- bq_test_table()
  tb2 <- bq_test_table()
  df <- data.frame(x = 1:3)
  job_id <- random_name()

  bq_table_upload(tb1, df, job_id = job_id)
  expect_error(
    bq_table_upload(tb2, df, job_id = job_id),
    "loads into a different table"
  )
})

test_that("a failed job is not reported as a success", {
  tb <- bq_test_table()
  bq_table_create(tb, fields = list(bq_field("x", "integer")))
  job_id <- random_name()

  # x must be an integer, so this job fails
  expect_error(
    bq_table_upload(tb, data.frame(x = "x"), job_id = job_id),
    "failed"
  )
  # Retrying the batch surfaces the same failure instead of a success
  expect_error(
    bq_table_upload(tb, data.frame(x = "x"), job_id = job_id),
    "failed"
  )
})

# job_id recovery (mocked) ------------------------------------------------

# Simulates a retry after the first attempt's response was lost: the upload
# fails with a duplicate error because the job already exists on the server.
mock_lost_response <- function(load, code, status = list(state = "DONE")) {
  with_mocked_bindings(
    code,
    bq_table_exists = function(x) TRUE,
    bq_upload = function(...) {
      cli::cli_abort("Already Exists: Job", class = "bigrquery_duplicate")
    },
    bq_dataset_meta = function(x, fields = NULL) list(location = "US"),
    bq_job_meta = function(x, fields = NULL) {
      if (identical(fields, "status")) {
        list(status = status)
      } else {
        list(configuration = list(load = load))
      }
    }
  )
}

test_that("a duplicate job_id adopts the existing job", {
  tb <- bq_table("project", "dataset", "table")
  load <- list(
    destinationTable = list(
      projectId = "project",
      datasetId = "dataset",
      tableId = "table"
    ),
    sourceFormat = "NEWLINE_DELIMITED_JSON",
    createDisposition = "CREATE_IF_NEEDED",
    writeDisposition = "WRITE_APPEND"
  )

  mock_lost_response(load, {
    expect_identical(
      bq_table_upload(
        tb,
        data.frame(x = 1),
        write_disposition = "WRITE_APPEND",
        job_id = "job_1",
        quiet = TRUE
      ),
      tb
    )
  })
})

test_that("a failed existing job is not a success", {
  tb <- bq_table("project", "dataset", "table")
  load <- list(
    destinationTable = list(
      projectId = "project",
      datasetId = "dataset",
      tableId = "table"
    ),
    sourceFormat = "NEWLINE_DELIMITED_JSON",
    createDisposition = "CREATE_IF_NEEDED",
    writeDisposition = "WRITE_APPEND"
  )
  status <- list(
    state = "DONE",
    errors = list(list(message = "Could not parse", reason = "invalid"))
  )

  mock_lost_response(load, status = status, {
    expect_error(
      bq_table_upload(
        tb,
        data.frame(x = 1),
        write_disposition = "WRITE_APPEND",
        job_id = "job_1",
        quiet = TRUE
      ),
      "failed"
    )
  })
})

test_that("a job_id pointing at a different table errors informatively", {
  tb <- bq_table("project", "dataset", "table")
  load <- list(
    destinationTable = list(
      projectId = "project",
      datasetId = "dataset",
      tableId = "other_table"
    ),
    sourceFormat = "NEWLINE_DELIMITED_JSON",
    createDisposition = "CREATE_IF_NEEDED",
    writeDisposition = "WRITE_APPEND"
  )

  mock_lost_response(load, {
    expect_error(
      bq_table_upload(
        tb,
        data.frame(x = 1),
        write_disposition = "WRITE_APPEND",
        job_id = "job_1",
        quiet = TRUE
      ),
      "loads into a different table"
    )
  })
})

# check_recovered_load_job ------------------------------------------------

test_that("check_recovered_load_job accepts a matching job", {
  x <- bq_table("project", "dataset", "table")
  load <- list(
    destinationTable = list(
      projectId = "project",
      datasetId = "dataset",
      tableId = "table"
    ),
    sourceFormat = "NEWLINE_DELIMITED_JSON",
    createDisposition = "CREATE_IF_NEEDED",
    writeDisposition = "WRITE_APPEND"
  )

  expect_true(
    check_recovered_load_job(
      load,
      job_id = "job_1",
      x = x,
      source_format = "NEWLINE_DELIMITED_JSON",
      create_disposition = "CREATE_IF_NEEDED",
      write_disposition = "WRITE_APPEND"
    )
  )
})

test_that("check_recovered_load_job rejects a non-load job", {
  x <- bq_table("project", "dataset", "table")
  expect_snapshot(
    check_recovered_load_job(
      NULL,
      job_id = "job_1",
      x = x,
      source_format = "NEWLINE_DELIMITED_JSON",
      create_disposition = "CREATE_IF_NEEDED",
      write_disposition = "WRITE_APPEND"
    ),
    error = TRUE
  )
})

test_that("check_recovered_load_job rejects a different destination", {
  x <- bq_table("project", "dataset", "table")
  load <- list(
    destinationTable = list(
      projectId = "project",
      datasetId = "dataset",
      tableId = "other_table"
    ),
    sourceFormat = "NEWLINE_DELIMITED_JSON",
    createDisposition = "CREATE_IF_NEEDED",
    writeDisposition = "WRITE_APPEND"
  )

  expect_snapshot(
    check_recovered_load_job(
      load,
      job_id = "job_1",
      x = x,
      source_format = "NEWLINE_DELIMITED_JSON",
      create_disposition = "CREATE_IF_NEEDED",
      write_disposition = "WRITE_APPEND"
    ),
    error = TRUE
  )
})

test_that("check_recovered_load_job rejects a different load configuration", {
  x <- bq_table("project", "dataset", "table")
  load <- list(
    destinationTable = list(
      projectId = "project",
      datasetId = "dataset",
      tableId = "table"
    ),
    sourceFormat = "PARQUET",
    createDisposition = "CREATE_IF_NEEDED",
    writeDisposition = "WRITE_TRUNCATE"
  )

  expect_snapshot(
    check_recovered_load_job(
      load,
      job_id = "job_1",
      x = x,
      source_format = "NEWLINE_DELIMITED_JSON",
      create_disposition = "CREATE_IF_NEEDED",
      write_disposition = "WRITE_APPEND"
    ),
    error = TRUE
  )
})

test_that("bq_perform_copy creates job that succeeds", {
  withr::local_options(cli.progress_show_after = Inf)

  src <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  dst <- bq_test_table()

  job <- bq_perform_copy(src, dst)
  expect_s3_class(job, "bq_job")

  # Doesn't return any statistics to show
  expect_snapshot({
    bq_job_wait(job, quiet = FALSE)
  })

  expect_true(bq_table_exists(dst))
})


# Load / extract ----------------------------------------------------------

test_that("can round trip extract + load", {
  ds_public <- bq_dataset("bigquery-public-data", "moon_phases")

  tb <- bq_dataset_query(
    ds_public,
    query = "SELECT COUNT(*) as count FROM moon_phases",
    billing = bq_test_project()
  )

  tmp <- gs_test_object()
  # on.exit(gs_object_delete(tmp))

  job <- bq_perform_extract(tb, tmp)
  bq_job_wait(job)

  tb_ks <- bq_test_table()
  job <- bq_perform_load(tb_ks, tmp)
  bq_job_wait(job)

  df <- bq_table_download(tb_ks)
  expect_equal(nrow(df), 1)
  expect_named(df, "count")
})

# Queries -----------------------------------------------------------------

test_that("bq_perform_query creates job that succeeds", {
  ds <- as_bq_dataset("bigquery-public-data.moon_phases")
  job <- bq_perform_query(
    "SELECT count(*) FROM moon_phases",
    billing = bq_test_project(),
    default_dataset = ds
  )

  expect_s3_class(job, "bq_job")
  expect_snapshot({
    bq_job_wait(job, quiet = FALSE)
  })

  job_tb <- bq_job_table(job)
  expect_true(bq_table_exists(job_tb))
})

test_that("can supply scalar parameters", {
  job <- bq_project_query(
    bq_test_project(),
    "SELECT 1 + @x",
    parameters = list(x = bq_param_scalar(1))
  )
  df <- bq_table_download(job)
  expect_setequal(df[[1]], 2)
})

test_that("can supply array parameters", {
  job <- bq_project_query(
    bq_test_project(),
    "SELECT values FROM UNNEST(@x) values",
    parameters = list(x = bq_param_array(c("a", "b")))
  )
  df <- bq_table_download(job)
  expect_setequal(df$values, c("a", "b"))
})

test_that("can estimate cost and get schema", {
  cost <- bq_perform_query_dry_run(
    "SELECT count(*) FROM bigquery-public-data.moon_phases.moon_phases",
    billing = bq_test_project()
  )
  expect_equal(cost, structure(0, class = "bq_bytes"))

  schema <- bq_perform_query_schema(
    "SELECT * FROM bigquery-public-data.moon_phases.moon_phases",
    billing = bq_test_project()
  )
  names <- vapply(schema, function(x) x$name, character(1))
  expect_equal(names, c("phase", "phase_emoji", "peak_datetime"))

  types <- vapply(schema, function(x) x$type, character(1))
  expect_equal(types, c("STRING", "STRING", "DATETIME"))
})
