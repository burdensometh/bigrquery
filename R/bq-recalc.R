#' Recalculate one day's partition of a table
#'
#' @description
#' `bq_table_recalc()` repairs a single day's partition of a date-partitioned
#' table, e.g. when late-arriving facts require recomputing the previous
#' business day. It connects three steps into a single entry point:
#'
#' 1. **Load**: `query` is run and its results are written to a staging
#'    table, overwriting any data staged by a previous run for the same day.
#' 1. **Validate**: the staged rows are checked to contain at least
#'    `min_rows` rows, and every `date_column` value must equal `date`.
#' 1. **Publish**: the staging table is copied over the `date` partition of
#'    `x` in a single atomic job, leaving all other partitions untouched.
#'
#' If validation fails, `x` is left untouched and the staging table is kept;
#' the error message tells you where to find it. Re-running
#' `bq_table_recalc()` for the same `date` is safe and produces the same
#' result.
#'
#' @param x A [bq_table], or an object coercible to one: the target table,
#'   partitioned by `date_column`.
#' @param query A SQL query string that recomputes one day of data. Refer to
#'   the day being recalculated with the `@date` parameter, which is
#'   automatically bound to the `date` argument.
#' @param date A `Date`, or a string like `"2026-11-22"`: the business day to
#'   recalculate.
#' @param date_column Name of the date column in the query result; used to
#'   check that the staged rows all belong to `date`.
#' @param staging A [bq_table] used to stage the recalculated data. By
#'   default a date-specific table named after the target table and date
#'   (e.g. `trades_staging_20261122`) in the same dataset is used, so that a
#'   failed run can be inspected and re-runs overwrite the same table.
#' @param min_rows Minimum number of staged rows needed for validation to
#'   pass.
#' @param parameters A named list of additional query parameters, matched to
#'   `@name` placeholders in `query`.
#' @param keep_staging Keep the staging table after a successful publish?
#'   Defaults to deleting it.
#' @param billing Project to bill for the query and copy jobs.
#' @inheritParams api-job
#' @param ... Additional arguments passed on to [bq_perform_query()].
#' @return Invisibly, a list describing the run, with components `table`,
#'   `date`, `staging`, `rows`, `load_job`, and `publish_job`.
#' @export
#' @examplesIf bq_testable()
#' ds <- bq_test_dataset()
#' trades <- bq_table(ds, "trades")
#' fields <- data.frame(
#'   trade_date = as.Date("2026-11-21"),
#'   trade_id = "t1",
#'   qty = 1
#' )
#' bq_table_create(
#'   trades,
#'   fields = fields,
#'   time_partitioning = list(type = "DAY", field = "trade_date")
#' )
#'
#' # Late trades arrived for 2026-11-21: recompute the day into staging,
#' # validate it, then atomically replace only that day's partition
#' bq_table_recalc(
#'   trades,
#'   "SELECT @date AS trade_date, 't2' AS trade_id, 5 AS qty",
#'   "2026-11-21",
#'   date_column = "trade_date"
#' )
#'
#' bq_table_download(trades)
bq_table_recalc <- function(
  x,
  query,
  date,
  date_column = "date",
  staging = NULL,
  min_rows = 1,
  parameters = list(),
  keep_staging = FALSE,
  billing = x$project,
  quiet = getOption("bigrquery.quiet", NA),
  ...
) {
  x <- as_bq_table(x)
  query <- as_query(query)
  date <- as_recalc_date(date)
  check_string(date_column)
  if (is.null(staging)) {
    staging <- recalc_staging_table(x, date)
  } else {
    staging <- as_bq_table(staging)
  }
  check_number_whole(min_rows, min = 0)
  check_bool(keep_staging)
  check_string(billing)
  quiet <- check_quiet(quiet)

  if (!bq_table_exists(x)) {
    cli::cli_abort("Target table {.val {toString(x)}} does not exist.")
  }

  # 1. Load: (re)compute the day into the staging table
  parameters <- utils::modifyList(list(date = date), parameters)
  load_job <- bq_perform_query(
    query,
    billing = billing,
    destination_table = staging,
    write_disposition = "WRITE_TRUNCATE",
    parameters = parameters,
    ...
  )
  bq_job_wait(load_job, quiet = quiet)

  # 2. Validate: staging must hold exactly this day's rows
  stats <- recalc_stats(staging, date_column, billing = billing, quiet = quiet)
  problems <- recalc_problems(stats, date, min_rows, date_column)
  if (length(problems) > 0) {
    cli::cli_abort(
      c(
        "Recalculation of {.val {toString(x)}} for {date} failed validation:",
        set_names(problems, "x"),
        "i" = "Target table was not modified.",
        "i" = paste0(
          "Staging table {.val {toString(staging)}} kept for inspection ",
          "(load job {.val {as.character(load_job)}})."
        )
      ),
      class = "bigrquery_recalc_validation"
    )
  }

  # 3. Publish: atomically replace only this day's partition
  partition <- recalc_partition_table(x, date)
  publish_job <- bq_perform_copy(
    staging,
    partition,
    write_disposition = "WRITE_TRUNCATE",
    billing = billing
  )
  bq_job_wait(publish_job, quiet = quiet)

  # 4. Clean up staging; the partition now holds the validated data
  if (!keep_staging) {
    tryCatch(
      bq_table_delete(staging),
      error = function(cnd) {
        cli::cli_warn(
          "Failed to delete staging table {.val {toString(staging)}}."
        )
      }
    )
  }

  invisible(list(
    table = x,
    date = date,
    staging = staging,
    rows = stats$n,
    load_job = load_job,
    publish_job = publish_job
  ))
}

recalc_staging_table <- function(x, date) {
  bq_table(
    x$project,
    x$dataset,
    paste0(x$table, "_staging_", format(date, "%Y%m%d"))
  )
}

recalc_partition_table <- function(x, date) {
  bq_table(
    x$project,
    x$dataset,
    paste0(x$table, "$", format(date, "%Y%m%d"))
  )
}

recalc_stats <- function(staging, date_column, billing, quiet) {
  sql <- paste0(
    "SELECT COUNT(*) AS n,",
    " COUNTIF(`", date_column, "` IS NULL) AS n_null,",
    " MIN(`", date_column, "`) AS min_date,",
    " MAX(`", date_column, "`) AS max_date",
    " FROM `", toString(staging), "`"
  )
  tb <- bq_project_query(billing, sql, quiet = quiet)
  df <- bq_table_download(tb, bigint = "numeric", quiet = quiet)

  list(
    n = df$n,
    n_null = df$n_null,
    min_date = df$min_date,
    max_date = df$max_date
  )
}

recalc_problems <- function(stats, date, min_rows, date_column) {
  problems <- character()

  if (stats$n < min_rows) {
    problems <- c(problems, sprintf(
      "Staging table contains %s row%s, but `min_rows` is %s.",
      stats$n,
      if (stats$n == 1) "" else "s",
      min_rows
    ))
  }
  if (stats$n_null > 0) {
    problems <- c(problems, sprintf(
      "%s row%s have a missing `%s` value.",
      stats$n_null,
      if (stats$n_null == 1) "" else "s",
      date_column
    ))
  }
  if (stats$n > stats$n_null) {
    observed <- c(stats$min_date, stats$max_date)
    in_range <- !any(is.na(observed)) && all(observed == date)
    if (!in_range) {
      problems <- c(problems, sprintf(
        "`%s` ranges from %s to %s, but every row must be %s.",
        date_column,
        format(observed[[1]]),
        format(observed[[2]]),
        format(date)
      ))
    }
  }

  problems
}

as_recalc_date <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!inherits(x, "Date")) {
    x <- tryCatch(
      as.Date(x),
      error = function(cnd) {
        cli::cli_abort(
          c(
            "{.arg {arg}} must be a {.cls Date} or a date string like {.val 2026-11-22}.",
            "x" = conditionMessage(cnd)
          ),
          call = call
        )
      }
    )
  }
  if (length(x) != 1 || is.na(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single, non-missing date.",
      call = call
    )
  }
  x
}
