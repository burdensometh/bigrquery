test_that("as_of is coerced to a single UTC instant", {
  utc <- as_bq_as_of("2024-01-01 12:00:00")
  expect_s3_class(utc, "POSIXct")
  expect_equal(attr(utc, "tzone"), "UTC")
  expect_equal(
    format(utc, "%Y-%m-%d %H:%M:%OS6", tz = "UTC"),
    "2024-01-01 12:00:00.000000"
  )

  expect_equal(
    as_bq_as_of("2024-01-01T12:00:00Z"),
    utc
  )
  expect_equal(
    as_bq_as_of("2024-01-01T14:00:00+02:00"),
    utc
  )
  expect_equal(
    format(
      as_bq_as_of("2024-01-01T12:00:00.123456Z"),
      "%Y-%m-%d %H:%M:%OS6",
      tz = "UTC"
    ),
    "2024-01-01 12:00:00.123456"
  )
  expect_equal(
    as_bq_as_of(as.POSIXct("2024-01-01 07:00:00", tz = "America/New_York")),
    utc
  )
  expect_equal(as_bq_as_of(as.Date("2024-01-01")), as_bq_as_of("2024-01-01"))
})

test_that("as_of rejects invalid inputs", {
  expect_snapshot(error = TRUE, {
    as_bq_as_of("not a time")
    as_bq_as_of("2024-01-01junk")
    as_bq_as_of("2024-02-30 10:00:00")
    as_bq_as_of(c("2024-01-01", "2024-01-02"))
    as_bq_as_of(NA_character_)
    as_bq_as_of(1)
  })
})

test_that("query reads the source table as of the instant", {
  x <- bq_table("my-project", "my_dataset", "my_table")
  as_of <- as_bq_as_of("2024-01-01 12:00:00.123456")

  expect_equal(
    bq_as_of_query(x, as_of),
    paste0(
      "SELECT * FROM `my-project.my_dataset.my_table`",
      " FOR SYSTEM_TIME AS OF TIMESTAMP '2024-01-01 12:00:00.123456+00:00'"
    )
  )
})

test_that("bq_table_as_of stores source and read time and reports them", {
  source <- bq_table("my-project", "my_dataset", "my_table")
  result <- bq_table("billing", "_anon", "result")

  local_mocked_bindings(
    bq_perform_query = function(query, ...) {
      expect_equal(
        query,
        bq_as_of_query(source, as_bq_as_of("2024-01-01 12:00:00"))
      )
      bq_job("billing", "job", "US")
    },
    bq_job_wait = function(x, ...) invisible(x),
    bq_job_table = function(x) result
  )

  out <- bq_table_as_of(source, as_of = "2024-01-01 12:00:00")

  expect_s3_class(out, "bq_table_as_of")
  expect_s3_class(out, "bq_table")
  expect_equal(bq_table_as_of_source(out), source)
  expect_equal(
    bq_table_as_of_time(out),
    as_bq_as_of("2024-01-01 12:00:00")
  )
  expect_snapshot(print(out))
})

test_that("bq_table_as_of only accepts its own tables' metadata", {
  expect_snapshot(error = TRUE, {
    bq_table_as_of_source(bq_table("a", "b", "c"))
    bq_table_as_of_time(bq_table("a", "b", "c"))
  })
})

test_that("out-of-window errors are distinguishable from empty results", {
  source <- bq_table("my-project", "my_dataset", "my_table")

  local_mocked_bindings(
    bq_perform_query = function(query, ...) bq_job("billing", "job", "US"),
    bq_job_wait = function(x, ...) {
      cli::cli_abort(c(
        "Job <bq_job> failed",
        x = paste0(
          "Invalid snapshot time 1601168925462 for table ",
          "my-project:my_dataset.my_table@1601168925462. ",
          "Cannot read before 1601573410026. [invalidQuery]"
        )
      ))
    }
  )

  err <- expect_error(
    bq_table_as_of(source, as_of = "2020-09-26 00:00:00"),
    class = "bigrquery_as_of_out_of_window"
  )
  expect_equal(err$source_table, source)
  expect_equal(err$as_of, as_bq_as_of("2020-09-26 00:00:00"))
  expect_equal(
    err$oldest_readable,
    .POSIXct(1601573410.026, tz = "UTC")
  )
  expect_snapshot(error = TRUE, {
    bq_table_as_of(source, as_of = "2020-09-26 00:00:00")
  })
})

test_that("a successful read with no rows is not an out-of-window error", {
  source <- bq_table("my-project", "my_dataset", "my_table")
  result <- bq_table("billing", "_anon", "empty")

  local_mocked_bindings(
    bq_perform_query = function(query, ...) bq_job("billing", "job", "US"),
    bq_job_wait = function(x, ...) invisible(x),
    bq_job_table = function(x) result
  )

  out <- bq_table_as_of(source, as_of = "2024-01-01 00:00:00")
  expect_s3_class(out, "bq_table_as_of")
})

test_that("billing defaults to the source project but can be overridden", {
  source <- bq_table("my-project", "my_dataset", "my_table")
  seen <- NULL

  local_mocked_bindings(
    bq_perform_query = function(query, billing, ...) {
      seen <<- billing
      bq_job("billing", "job", "US")
    },
    bq_job_wait = function(x, ...) invisible(x),
    bq_job_table = function(x) bq_table("billing", "_anon", "result")
  )

  bq_table_as_of(source, as_of = "2024-01-01 00:00:00")
  expect_equal(seen, "my-project")

  bq_table_as_of(source, as_of = "2024-01-01 00:00:00", billing = "other")
  expect_equal(seen, "other")
})

test_that("unrelated job errors are not reclassified", {
  source <- bq_table("my-project", "my_dataset", "my_table")

  local_mocked_bindings(
    bq_perform_query = function(query, ...) bq_job("billing", "job", "US"),
    bq_job_wait = function(x, ...) {
      cli::cli_abort("Job <bq_job> failed", class = "bigrquery_invalidQuery")
    }
  )

  expect_snapshot(
    bq_table_as_of(source, as_of = "2024-01-01 00:00:00"),
    error = TRUE
  )
})
