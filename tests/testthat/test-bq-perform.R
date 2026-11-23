test_that("export_json keeps DATETIME local while TIMESTAMP stays an instant", {
  library(clock)
  df <- data.frame(
    business_dt = naive_time_parse(
      c("2025-03-09T23:30:00", "2025-03-10T00:30:00.123456"),
      precision = "microsecond"
    ),
    event_ts = as.POSIXct(
      c("2025-03-09 23:30:00", "2025-03-10 00:30:00"),
      tz = "UTC"
    )
  )

  lines <- strsplit(export_json(df), "\n")[[1]]

  expect_match(lines[[1]], '"business_dt":"2025-03-09 23:30:00.000000"')
  expect_match(lines[[1]], '"event_ts":"2025-03-09 23:30:00.000000"')
  expect_match(lines[[2]], '"business_dt":"2025-03-10 00:30:00.123456"')
})

test_that("export_json writes DATETIME NAs as JSON null", {
  library(clock)
  df <- data.frame(
    business_dt = naive_time_parse(
      c("2025-03-09T23:30:00", NA),
      precision = "microsecond"
    )
  )

  lines <- strsplit(export_json(df), "\n")[[1]]
  expect_match(lines[[1]], '"business_dt":"2025-03-09 23:30:00.000000"')
  expect_match(lines[[2]], '"business_dt":null')
})

test_that("DATETIME parameters serialise with their local value and type", {
  library(clock)
  nt <- naive_time_parse(
    c("2025-03-09T23:30:00", "2025-03-10T00:30:00.123456"),
    precision = "microsecond"
  )

  scalar <- bq_param_scalar(nt[1], name = "x")
  expect_equal(scalar$type, "DATETIME")
  scalar_json <- jsonlite::toJSON(as_json(scalar), auto_unbox = TRUE)
  expect_match(
    scalar_json,
    '"parameterType":{"type":"DATETIME"}',
    fixed = TRUE
  )
  expect_match(scalar_json, '"value":"2025-03-09 23:30:00.000000"', fixed = TRUE)

  array <- bq_param_array(nt, name = "x")
  expect_equal(array$type, "DATETIME")
  json <- jsonlite::toJSON(as_json(array), auto_unbox = TRUE)
  expect_match(json, '"type":"ARRAY"')
  expect_match(json, '"2025-03-09 23:30:00.000000"')
  expect_match(json, '"2025-03-10 00:30:00.123456"')
})

test_that("data_type distinguishes DATETIME from TIMESTAMP", {
  library(clock)
  expect_equal(
    data_type(naive_time_parse("2025-03-09T23:30:00", precision = "microsecond")),
    "DATETIME"
  )
  expect_equal(data_type(as.POSIXct("2025-03-09 23:30:00", tz = "UTC")), "TIMESTAMP")

  df <- data.frame(
    x = naive_time_parse("2025-03-09T23:30:00", precision = "microsecond"),
    y = as.POSIXct("2025-03-09 23:30:00", tz = "UTC")
  )
  fields <- as_bq_fields(df)
  expect_equal(fields[[1]]$type, "DATETIME")
  expect_equal(fields[[2]]$type, "TIMESTAMP")
})

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
