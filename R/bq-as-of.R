#' Read a table as of a point in time
#'
#' @description
#' `bq_table_as_of()` materializes the state of a table at a specific UTC
#' instant, using BigQuery
#' [time travel](https://cloud.google.com/bigquery/docs/access-historical-data).
#' This is useful for reproducing an analysis when rows have since been added,
#' updated, or deleted: the returned table is an independent query result that
#' does not change as the source table changes.
#'
#' The returned object is a [bq_table] (with class `bq_table_as_of`), so you can
#' download it with [bq_table_download()] or pass it to other `bq_table_*()`
#' functions. It records the source table and the instant it was read at; see
#' `bq_table_as_of_source()` and `bq_table_as_of_time()`.
#'
#' If `destination_table` is `NULL`, the results are stored in the job's
#' anonymous results table, which BigQuery keeps for roughly 24 hours. Supply
#' `destination_table` to keep a durable copy.
#'
#' @param x A [bq_table], or an object coercible to a `bq_table`.
#' @param as_of The UTC instant to read at. A `POSIXct`, `Date`, or a string
#'   such as `"2024-01-01 12:00:00"` (interpreted as UTC) or
#'   `"2024-01-01T12:00:00Z"`. Must fall within the table's time travel
#'   window.
#' @param destination_table An optional [bq_table] where the results should be
#'   stored.
#' @param billing Project to bill. Defaults to the project of `x`; supply a
#'   billing project when reading a public dataset.#' @inheritParams api-perform
#' @inheritParams api-job
#' @return
#' * `bq_table_as_of()`: a `bq_table` of class `bq_table_as_of`, with the
#'   source table and read instant stored as attributes.
#' * `bq_table_as_of_source()`: the source [bq_table].
#' * `bq_table_as_of_time()`: the read instant, a `POSIXct` in UTC.
#' @section Errors:
#' If BigQuery can't read `as_of`, for example because it falls before the
#' start of the table's time travel window or before the table was created,
#' `bq_table_as_of()` throws an error of class
#' `bigrquery_as_of_out_of_window`. This is distinct from a successful read
#' that happens to contain no rows: in that case `bq_table_as_of()` succeeds
#' and [bq_table_download()] returns a zero-row table.
#' @seealso [bq_table_download()] to retrieve the rows.
#' @examplesIf bq_testable()
#' tb <- bq_table(bq_test_project(), "basedata", "mtcars")
#' snapshot <- bq_table_as_of(tb, as_of = Sys.time() - 60 * 60)
#' bq_table_as_of_source(snapshot)
#' bq_table_as_of_time(snapshot)
#' bq_table_download(snapshot)
#' @export
bq_table_as_of <- function(
  x,
  as_of,
  destination_table = NULL,
  ...,
  billing = NULL,
  quiet = getOption("bigrquery.quiet", NA)
) {
  check_required(as_of)
  x <- as_bq_table(x)
  as_of <- as_bq_as_of(as_of)
  check_string(billing, allow_null = TRUE)
  billing <- billing %||% x$project
  quiet <- check_quiet(quiet)
  if (!is.null(destination_table)) {
    destination_table <- as_bq_table(destination_table)
  }

  query <- bq_as_of_query(x, as_of)
  call <- caller_env()

  job <- as_of_guard(
    bq_perform_query(
      query,
      billing = billing,
      destination_table = destination_table,
      ...
    ),
    source = x,
    as_of = as_of,
    call = call
  )
  as_of_guard(
    bq_job_wait(job, quiet = quiet),
    source = x,
    as_of = as_of,
    call = call
  )

  out <- bq_job_table(job)
  attr(out, "source_table") <- x
  attr(out, "as_of") <- as_of
  class(out) <- c("bq_table_as_of", class(out))
  out
}

#' @export
#' @rdname bq_table_as_of
bq_table_as_of_source <- function(x) {
  check_as_of_table(x)
  attr(x, "source_table")
}

#' @export
#' @rdname bq_table_as_of
bq_table_as_of_time <- function(x) {
  check_as_of_table(x)
  attr(x, "as_of")
}

#' @export
print.bq_table_as_of <- function(x, ...) {
  cat_line("<bq_table_as_of> ", toString(x))
  cat_line("Source: ", toString(attr(x, "source_table")))
  cat_line("As of:  ", format_bq_as_of(attr(x, "as_of")))
  invisible(x)
}

check_as_of_table <- function(x, call = caller_env()) {
  if (!inherits(x, "bq_table_as_of")) {
    cli::cli_abort(
      "{.arg x} must be a table created by {.fun bq_table_as_of}.",
      call = call
    )
  }
  invisible()
}

# Build the query that reads `x` at `as_of` -------------------------------

bq_as_of_query <- function(x, as_of) {
  paste0(
    "SELECT * FROM ",
    bq_table_sql(x),
    " FOR SYSTEM_TIME AS OF TIMESTAMP '",
    format_bq_as_of_sql(as_of),
    "'"
  )
}

bq_table_sql <- function(x) {
  encodeString(toString(as_bq_table(x)), quote = "`")
}

# SQL TIMESTAMP literal, always in UTC with microsecond precision.
format_bq_as_of_sql <- function(x) {
  paste0(format_bq_as_of_core(x), "+00:00")
}

format_bq_as_of <- function(x) {
  out <- format_bq_as_of_core(x)
  out <- sub("0+$", "", out)
  out <- sub("\\.$", "", out)
  paste0(out, " UTC")
}

# Build the timestamp by hand so the decimal separator doesn't follow
# `LC_NUMERIC` (a comma in many locales).
format_bq_as_of_core <- function(x) {
  seconds <- as.numeric(x)
  whole <- floor(seconds)
  micros <- round((seconds - whole) * 1e6)
  if (micros >= 1e6) {
    whole <- whole + 1
    micros <- 0
  }
  date_time <- format(.POSIXct(whole, tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  sprintf("%s.%06d", date_time, as.integer(micros))
}

# Coerce `as_of` to a length-1 POSIXct in UTC -----------------------------

as_bq_as_of <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (inherits(x, "POSIXt") || inherits(x, "Date")) {
    if (length(x) != 1 || is.na(x)) {
      cli::cli_abort(
        "{.arg {arg}} must be a single, non-missing time.",
        call = call
      )
    }
    if (inherits(x, "POSIXt")) {
      out <- .POSIXct(as.numeric(x), tz = "UTC")
    } else {
      out <- .POSIXct(as.numeric(as.POSIXct(x, tz = "UTC")), tz = "UTC")
    }
    return(out)
  }

  if (!is.character(x)) {
    stop_input_type(
      x,
      c("a POSIXct", "a Date", "a string"),
      arg = arg,
      call = call
    )
  }
  if (length(x) != 1 || is.na(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single, non-missing time.",
      call = call
    )
  }

  parse_bq_as_of(x, arg = arg, call = call)
}

parse_bq_as_of <- function(x, arg = caller_arg(x), call = caller_env()) {
  pattern <- paste0(
    "^(\\d{4}-\\d{2}-\\d{2})",
    "(?:[T ](\\d{2}:\\d{2})(?::(\\d{2})(?:\\.(\\d{1,9}))?)?)?",
    "(Z|[+-]\\d{2}:?\\d{2})?$"
  )
  x <- trimws(x)
  matches <- regmatches(x, regexec(pattern, x, perl = TRUE))
  if (length(matches[[1]]) == 0) {
    as_of_parse_abort(x, arg = arg, call = call)
  }
  pieces <- matches[[1]]

  date <- pieces[2]
  hm <- pieces[3]
  sec <- pieces[4]
  frac <- pieces[5]
  offset <- pieces[6]

  value <- date
  fmt <- "%Y-%m-%d"
  if (nzchar(hm)) {
    value <- paste0(value, " ", hm)
    fmt <- paste0(fmt, " %H:%M")
  }
  if (nzchar(sec)) {
    value <- paste0(value, ":", sec)
    if (nzchar(frac)) {
      value <- paste0(value, ".", frac)
    }
    fmt <- paste0(fmt, ":%OS")
  }
  if (nzchar(offset)) {
    offset <- if (offset == "Z") "+0000" else sub(":", "", offset)
    value <- paste0(value, offset)
    fmt <- paste0(fmt, "%z")
  }

  lt <- strptime(value, fmt, tz = "UTC")
  if (is.na(lt)) {
    as_of_parse_abort(x, arg = arg, call = call)
  }
  .POSIXct(as.numeric(as.POSIXct(lt, tz = "UTC")), tz = "UTC")
}

as_of_parse_abort <- function(x, arg, call) {
  cli::cli_abort(
    c(
      "{.arg {arg}} must be a valid UTC date-time, not {.val {cli_escape(x)}}.",
      i = 'Use e.g. "2024-01-01 12:00:00" or "2024-01-01T12:00:00Z".'
    ),
    call = call
  )
}

# Translate "outside the time travel window" server errors -----------------

# Only rethrows (with a clearer message) for out-of-window errors; any other
# error propagates untouched.
as_of_guard <- function(expr, source, as_of, call) {
  withCallingHandlers(
    expr,
    error = function(err) {
      as_of_abort_if_out_of_window(err, source, as_of, call = call)
    }
  )
}

as_of_abort_if_out_of_window <- function(err, source, as_of, call) {
  message <- conditionMessage(err)
  if (!is_time_travel_error(message)) {
    return(invisible())
  }

  oldest <- time_travel_oldest(message)
  bullets <- c(
    x = paste0(
      "That instant is outside the time travel window for ",
      cli_escape(toString(source)),
      "."
    ),
    i = paste0("BigQuery said: ", cli_escape(time_travel_reason(message)))
  )
  if (!is.null(oldest)) {
    bullets <- append(
      bullets,
      c("i" = paste0(
        "The oldest readable data starts at ",
        cli_escape(format_bq_as_of(oldest)),
        "."
      )),
      after = 1
    )
  }

  cli::cli_abort(
    c(
      paste0(
        "Can't read ",
        cli_escape(toString(source)),
        " as of ",
        cli_escape(format_bq_as_of(as_of)),
        "."
      ),
      bullets
    ),
    class = c(
      "bigrquery_as_of_out_of_window",
      "bigrquery_as_of_error",
      "bigrquery_error"
    ),
    source_table = source,
    as_of = as_of,
    oldest_readable = oldest,
    call = call
  )
}

is_time_travel_error <- function(message) {
  message <- tolower(message)
  grepl("invalid snapshot time", message, fixed = TRUE) ||
    grepl("invalid time travel timestamp", message, fixed = TRUE) ||
    grepl("cannot read before", message, fixed = TRUE) ||
    grepl("cannot read after", message, fixed = TRUE)
}

# Keep only the server's own sentence, dropping the job wrapper prefix.
time_travel_reason <- function(message) {
  lines <- trimws(strsplit(message, "\n", fixed = TRUE)[[1]])
  hits <- grep(
    "snapshot time|time travel|read before|read after",
    lines,
    ignore.case = TRUE,
    value = TRUE
  )
  if (length(hits) == 0) {
    return(message)
  }
  reason <- hits[[1]]
  sub("^[x!]\\s*", "", reason)
}

# The server reports the start of the window as milliseconds since the epoch.
time_travel_oldest <- function(message) {
  match <- regexpr("(?i)cannot read before [0-9]+", message, perl = TRUE)
  if (match < 0) {
    return(NULL)
  }
  text <- regmatches(message, match)
  ms <- as.numeric(sub("(?i)cannot read before ", "", text, perl = TRUE))
  .POSIXct(ms / 1000, tz = "UTC")
}
