test_that("recalc_problems flags bad staging data", {
  date <- as.Date("2026-11-22")
  na_date <- as.Date(NA_character_)

  ok <- list(n = 5, n_null = 0, min_date = date, max_date = date)
  expect_equal(recalc_problems(ok, date, 1, "trade_date"), character())

  empty <- list(n = 0, n_null = 0, min_date = na_date, max_date = na_date)
  expect_length(recalc_problems(empty, date, 1, "trade_date"), 1)
  expect_length(recalc_problems(empty, date, 0, "trade_date"), 0)

  wrong_day <- list(n = 3, n_null = 0, min_date = date - 1, max_date = date)
  expect_length(recalc_problems(wrong_day, date, 1, "trade_date"), 1)

  missing_day <- list(n = 3, n_null = 1, min_date = date, max_date = date)
  expect_length(recalc_problems(missing_day, date, 1, "trade_date"), 1)

  all_missing <- list(n = 2, n_null = 2, min_date = na_date, max_date = na_date)
  expect_length(recalc_problems(all_missing, date, 1, "trade_date"), 1)

  bad <- list(n = 2, n_null = 1, min_date = date - 1, max_date = date + 1)
  expect_length(recalc_problems(bad, date, 5, "trade_date"), 3)
})

test_that("as_recalc_date coerces and validates", {
  expect_equal(as_recalc_date("2026-11-22"), as.Date("2026-11-22"))
  expect_equal(as_recalc_date(as.Date("2026-11-22")), as.Date("2026-11-22"))

  expect_snapshot(as_recalc_date("not a date"), error = TRUE)
  expect_snapshot(as_recalc_date(as.Date("2026-11-22") + 0:1), error = TRUE)
})

test_that("staging and partition tables are derived from the date", {
  x <- bq_table("proj", "ds", "trades")
  date <- as.Date("2026-11-22")

  expect_equal(
    recalc_staging_table(x, date),
    bq_table("proj", "ds", "trades_staging_20261122")
  )
  expect_equal(recalc_partition_table(x, date)$table, "trades$20261122")
})

test_that("recalc_stats inspects the staging table", {
  track <- new.env()
  local_mocked_bindings(
    bq_project_query = function(x, query, ...) {
      track$query <- query
      bq_table("proj", "ds", "anon")
    },
    bq_table_download = function(x, ...) {
      data.frame(
        n = 3,
        n_null = 0,
        min_date = as.Date("2026-11-22"),
        max_date = as.Date("2026-11-22")
      )
    }
  )

  stats <- recalc_stats(
    bq_table("proj", "ds", "trades_staging_20261122"),
    "trade_date",
    billing = "proj",
    quiet = TRUE
  )

  expect_match(track$query, "COUNTIF(`trade_date` IS NULL)", fixed = TRUE)
  expect_match(
    track$query,
    "FROM `proj.ds.trades_staging_20261122`",
    fixed = TRUE
  )
  expect_equal(stats$n, 3)
  expect_equal(stats$min_date, as.Date("2026-11-22"))
})

test_that("successful run loads, validates, publishes, and cleans up", {
  track <- new.env()
  local_mocked_bindings(
    bq_table_exists = function(x) TRUE,
    bq_perform_query = function(
      query,
      billing,
      ...,
      destination_table,
      write_disposition,
      parameters
    ) {
      track$load_dest <- destination_table
      track$load_write <- write_disposition
      track$load_parameters <- parameters
      bq_job(billing, "job_load", "US")
    },
    bq_job_wait = function(x, ...) invisible(x),
    recalc_stats = function(...) {
      list(
        n = 10,
        n_null = 0,
        min_date = as.Date("2026-11-22"),
        max_date = as.Date("2026-11-22")
      )
    },
    bq_perform_copy = function(src, dest, ..., write_disposition) {
      track$copy_src <- src
      track$copy_dest <- dest
      track$copy_write <- write_disposition
      bq_job(dest$project, "job_copy", "US")
    },
    bq_table_delete = function(x) track$deleted <- toString(x)
  )

  target <- bq_table("proj", "ds", "trades")
  res <- bq_table_recalc(
    target,
    "SELECT @date AS trade_date, 1 AS qty",
    "2026-11-22",
    date_column = "trade_date",
    quiet = TRUE
  )

  # load truncates the deterministic per-date staging table
  expect_equal(
    track$load_dest,
    bq_table("proj", "ds", "trades_staging_20261122")
  )
  expect_equal(track$load_write, "WRITE_TRUNCATE")
  expect_equal(track$load_parameters$date, as.Date("2026-11-22"))

  # publish atomically replaces only the day's partition
  expect_equal(track$copy_src, track$load_dest)
  expect_equal(track$copy_dest$table, "trades$20261122")
  expect_equal(track$copy_write, "WRITE_TRUNCATE")

  # staging is cleaned up on success
  expect_equal(track$deleted, "proj.ds.trades_staging_20261122")

  expect_equal(res$table, target)
  expect_equal(res$date, as.Date("2026-11-22"))
  expect_equal(res$rows, 10)
  expect_s3_class(res$load_job, "bq_job")
  expect_s3_class(res$publish_job, "bq_job")
})

test_that("keep_staging = TRUE preserves the staging table on success", {
  track <- new.env()
  local_mocked_bindings(
    bq_table_exists = function(x) TRUE,
    bq_perform_query = function(...) bq_job("proj", "job_load", "US"),
    bq_job_wait = function(x, ...) invisible(x),
    recalc_stats = function(...) {
      list(
        n = 1,
        n_null = 0,
        min_date = as.Date("2026-11-22"),
        max_date = as.Date("2026-11-22")
      )
    },
    bq_perform_copy = function(...) bq_job("proj", "job_copy", "US"),
    bq_table_delete = function(x) track$deleted <- toString(x)
  )

  bq_table_recalc(
    bq_table("proj", "ds", "trades"),
    "SELECT 1",
    "2026-11-22",
    keep_staging = TRUE,
    quiet = TRUE
  )
  expect_null(track$deleted)
})

test_that("validation failure keeps staging and does not publish", {
  track <- new.env()
  local_mocked_bindings(
    bq_table_exists = function(x) TRUE,
    bq_perform_query = function(...) bq_job("proj", "job_load", "US"),
    bq_job_wait = function(x, ...) invisible(x),
    recalc_stats = function(...) {
      list(
        n = 2,
        n_null = 0,
        min_date = as.Date("2026-11-21"),
        max_date = as.Date("2026-11-22")
      )
    },
    bq_perform_copy = function(...) track$published <- TRUE,
    bq_table_delete = function(x) track$deleted <- toString(x)
  )

  expect_snapshot(
    bq_table_recalc(
      bq_table("proj", "ds", "trades"),
      "SELECT 1",
      "2026-11-22",
      date_column = "trade_date",
      quiet = TRUE
    ),
    error = TRUE
  )

  expect_error(
    bq_table_recalc(
      bq_table("proj", "ds", "trades"),
      "SELECT 1",
      "2026-11-22",
      date_column = "trade_date",
      quiet = TRUE
    ),
    class = "bigrquery_recalc_validation"
  )

  # nothing published, staging kept for inspection
  expect_null(track$published)
  expect_null(track$deleted)
})

test_that("missing target table fails before loading", {
  local_mocked_bindings(
    bq_table_exists = function(x) FALSE,
    bq_perform_query = function(...) stop("must not be called")
  )

  expect_snapshot(
    bq_table_recalc(bq_table("proj", "ds", "trades"), "SELECT 1", "2026-11-22"),
    error = TRUE
  )
})

test_that("recalc replaces one partition and preserves others", {
  ds <- bq_test_dataset()
  target <- bq_table(ds, "trades")
  day1 <- as.Date("2026-11-20")
  day2 <- as.Date("2026-11-21")

  df <- data.frame(
    trade_date = c(day1, day1, day2),
    trade_id = c("a", "b", "c"),
    qty = c(1, 2, 3)
  )
  bq_table_create(
    target,
    fields = df,
    time_partitioning = list(type = "DAY", field = "trade_date")
  )
  bq_table_upload(target, df)

  query <- "SELECT @date AS trade_date, 'a2' AS trade_id, 10 AS qty"
  res <- bq_table_recalc(
    target,
    query,
    day1,
    date_column = "trade_date",
    quiet = TRUE
  )
  expect_equal(res$rows, 1)

  # re-running the same date is safe
  bq_table_recalc(target, query, day1, date_column = "trade_date", quiet = TRUE)

  out <- bq_table_download(target, quiet = TRUE)
  out <- out[order(out$trade_id), ]
  expect_equal(out$trade_id, c("a2", "c"))
  expect_equal(out$trade_date, as.Date(c("2026-11-20", "2026-11-21")))
  expect_equal(out$qty, c(10, 3))

  # staging was cleaned up
  expect_false(bq_table_exists(bq_table(ds, "trades_staging_20261120")))
})

test_that("live validation failure keeps staging and leaves target untouched", {
  ds <- bq_test_dataset()
  target <- bq_table(ds, "trades")

  df <- data.frame(
    trade_date = as.Date("2026-11-20"),
    trade_id = "a",
    qty = 1
  )
  bq_table_create(
    target,
    fields = df,
    time_partitioning = list(type = "DAY", field = "trade_date")
  )
  bq_table_upload(target, df)

  scrub <- function(x) {
    x <- gsub(bq_test_project(), "<project>", x, fixed = TRUE)
    x <- gsub("TESTING_[a-z]+", "<dataset>", x)
    x <- gsub('load job "[^"]+"', 'load job "<job>"', x)
    x
  }
  expect_snapshot(
    bq_table_recalc(
      target,
      "SELECT DATE '2026-11-19' AS trade_date, 'x' AS trade_id, 1 AS qty",
      "2026-11-20",
      date_column = "trade_date",
      quiet = TRUE
    ),
    error = TRUE,
    transform = scrub
  )

  # staging kept for inspection
  staging <- bq_table(ds, "trades_staging_20261120")
  expect_true(bq_table_exists(staging))
  defer(bq_table_delete(staging))

  # target untouched
  out <- bq_table_download(target, quiet = TRUE)
  expect_equal(out$trade_id, "a")
})
