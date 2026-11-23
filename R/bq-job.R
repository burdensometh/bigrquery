#' BigQuery job: retrieve metadata
#'
#' To perform a job, see [api-perform]. These functions all retrieve metadata
#' (in various forms) about an existing job.
#'
#' @section Google BigQuery API documentation:
#' * [get](https://docs.cloud.google.com/bigquery/docs/reference/rest/v2/jobs/get)
#'
#' @examplesIf bq_testable()
#' jobs <- bq_project_jobs(bq_test_project())
#' jobs[[1]]
#'
#' # Show statistics about job
#' bq_job_show_statistics(jobs[[1]])
#'
#' # Wait for job to complete
#' bq_job_wait(jobs[[1]])
#' @name api-job
NULL

#' @export
#' @name api-job
#' @param x A [bq_job]
#' @param fields An optional field specification for
#'   [partial response](https://docs.cloud.google.com/bigquery/docs/api-performance#partial-response)
bq_job_meta <- function(x, fields = NULL) {
  x <- as_bq_job(x)
  bq_get(
    bq_path(x$project, jobs = x$job),
    query = list(
      location = x$location,
      fields = fields
    )
  )
}

bq_job_table <- function(x) {
  meta <- bq_job_meta(x, "configuration(query(destinationTable))")
  as_bq_table(meta$configuration$query$destinationTable)
}

#' @export
#' @name api-job
bq_job_status <- function(x) {
  bq_job_meta(x, "status")$status
}

#' @export
#' @name api-job
bq_job_show_statistics <- function(x) {
  stats <- bq_job_meta(x, "statistics")$statistics

  if ("load" %in% names(stats)) {
    in_bytes <- as.numeric(stats$load$inputFileBytes)
    out_bytes <- as.numeric(stats$load$outputBytes)
    cli::cli_inform("Input: {prettyunits::pretty_bytes(in_bytes)}")
    cli::cli_inform("Output: {prettyunits::pretty_bytes(out_bytes)}")
    bad_records <- as.numeric(stats$load$badRecords %||% 0)
    if (bad_records > 0) {
      cli::cli_inform("Discarded: {bad_records} bad record{?s}")
    }
  }

  if ("query" %in% names(stats)) {
    bytes <- as.numeric(stats$query$totalBytesBilled)
    cli::cli_inform("Billed: {prettyunits::pretty_bytes(bytes)}")
  }

  invisible(x)
}

#' Retrieve load statistics for a job
#'
#' Returns the load statistics for a load job, including the number of rows
#' that BigQuery discarded as bad records. A load job can succeed overall
#' while silently discarding rows (see the `max_bad_records` argument of
#' [bq_perform_load()]), so check these statistics before relying on the
#' loaded data. [bq_table_load_staged()] automates this check.
#'
#' @param x A [bq_job] created by [bq_perform_load()].
#' @return A list with components:
#'   * `input_files`: number of source files read.
#'   * `input_bytes`: size of the source files in bytes.
#'   * `output_rows`: number of rows loaded into the destination table.
#'   * `output_bytes`: size of the loaded data in bytes.
#'   * `bad_records`: number of rows discarded because they were invalid.
#' @export
#' @examplesIf bq_testable()
#' ds <- bq_test_dataset()
#' job <- bq_perform_upload(bq_table(ds, "mtcars"), mtcars)
#' bq_job_wait(job)
#' bq_job_load_stats(job)
bq_job_load_stats <- function(x) {
  x <- as_bq_job(x)
  stats <- bq_job_meta(x, "statistics")$statistics$load
  if (is.null(stats)) {
    cli::cli_abort("Job {x} does not have load statistics.")
  }

  list(
    input_files = as.numeric(stats$inputFiles %||% 0),
    input_bytes = as.numeric(stats$inputFileBytes %||% 0),
    output_rows = as.numeric(stats$outputRows %||% 0),
    output_bytes = as.numeric(stats$outputBytes %||% 0),
    bad_records = as.numeric(stats$badRecords %||% 0)
  )
}

#' @param quiet If `FALSE`, displays progress bar; if `TRUE` is silent;
#'   if `NA` picks based on whether or not you're in an interactive context.
#' @param pause amount of time to wait between status requests
#' @export
#' @name api-job
#' @inheritParams rlang::args_error_context
bq_job_wait <- function(
  x,
  quiet = getOption("bigrquery.quiet"),
  pause = 0.5,
  call = caller_env()
) {
  x <- as_bq_job(x)
  quiet <- check_quiet(quiet)
  check_number_decimal(pause)

  if (!quiet) {
    cli::cli_progress_bar(
      format = "Running job {x} {cli::pb_spin} {cli::pb_elapsed}",
      total = NA,
      clear = FALSE
    )
  }

  repeat {
    if (!quiet) {
      cli::cli_progress_update()
    }
    # https://docs.cloud.google.com/bigquery/docs/error-messages
    # Switch to req_retry() when we move to httr2
    status <- tryCatch(
      bq_job_status(x),
      bigrquery_http_503 = function(err) NULL
    )
    if (!quiet) {
      cli::cli_progress_update()
    }

    if (!is.null(status) && status$state == "DONE") {
      break
    }
    Sys.sleep(pause)
  }
  if (!quiet) {
    cli::cli_progress_done()
  }

  errors <- status$errors
  if (length(errors) > 0) {
    if (length(errors) > 1) {
      # First error says to look in errors[]
      errors <- errors[-1]
    }

    bullets <- map_chr(errors, function(x) {
      cli_escape(paste0(x$message, " [", x$reason, "]"))
    })
    bullets <- set_names(bullets, "x")
    cli::cli_abort(c("Job {x} failed", bullets), call = call)
  }

  if (!quiet) {
    cli::cli_inform("Job complete")
    bq_job_show_statistics(x)
  }

  invisible(x)
}
