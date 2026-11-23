#' BigQuery tables
#'
#' Basic create-read-update-delete verbs for tables, as well as functions
#' uploading data (`bq_table_upload()`), saving to/loading from Google
#' Cloud Storage (`bq_table_load()`, `bq_table_save()`), and getting
#' various values from the metadata.
#'
#' @param x A [bq_table], or an object coercible to a `bq_table`.
#' @inheritParams api-job
#' @inheritParams api-perform
#' @inheritParams bq_projects
#' @section Google BigQuery API documentation:
#' * [insert](https://docs.cloud.google.com/bigquery/docs/reference/rest/v2/tables/insert)
#' * [get](https://docs.cloud.google.com/bigquery/docs/reference/rest/v2/tables/get)
#' * [delete](https://docs.cloud.google.com/bigquery/docs/reference/rest/v2/tables/delete)
#' @return
#' * `bq_table_copy()`, `bq_table_create()`, `bq_table_delete()`, `bq_table_upload()`:
#'   an invisible [bq_table]
#' * `bq_table_exists()`: either `TRUE` or `FALSE`.
#' * `bq_table_size()`: the size of the table in bytes
#' * `bq_table_fields()`: a [bq_fields].
#'
#' @examplesIf bq_testable()
#' ds <- bq_test_dataset()
#'
#' bq_mtcars <- bq_table(ds, "mtcars")
#' bq_table_exists(bq_mtcars)
#'
#' bq_table_create(
#'   bq_mtcars,
#'   fields = mtcars,
#'   friendly_name = "Motor Trend Car Road Tests",
#'   description = "The data was extracted from the 1974 Motor Trend US magazine",
#'   labels = list(category = "example")
#' )
#' bq_table_exists(bq_mtcars)
#'
#' bq_table_upload(bq_mtcars, mtcars)
#'
#' bq_table_fields(bq_mtcars)
#' bq_table_size(bq_mtcars)
#' str(bq_table_meta(bq_mtcars))
#'
#' bq_table_delete(bq_mtcars)
#' bq_table_exists(bq_mtcars)
#'
#' my_natality <- bq_table(ds, "mynatality")
#' bq_table_copy("publicdata.samples.natality", my_natality)
#' @name api-table
NULL

#' @export
#' @rdname api-table
#' @param fields A [bq_fields] specification, or something coercible to it
#'   (like a data frame).
bq_table_create <- function(x, fields = NULL, ...) {
  x <- as_bq_table(x)

  url <- bq_path(x$project, x$dataset, "")
  body <- list(
    tableReference = tableReference(x)
  )
  if (!is.null(fields)) {
    fields <- as_bq_fields(fields)
    body$schema <- list(fields = as_json(fields))
  }

  bq_post(url, body = bq_body(body, ...))

  x
}

#' @export
#' @rdname api-table
#' @inheritParams api-job
bq_table_meta <- function(x, fields = NULL) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  bq_get(url, query = list(fields = fields))
}

#' @export
#' @rdname api-table
bq_table_fields <- function(x) {
  meta <- bq_table_meta(x, fields = "schema")
  fields <- meta$schema$fields

  bq_fields(lapply(fields, as_bq_field))
}

#' @export
#' @rdname api-table
bq_table_size <- function(x) {
  meta <- bq_table_meta(x, fields = "numBytes")
  bytes <- as.numeric(meta$numBytes)
  structure(bytes, class = "bq_bytes")
}

#' @export
#' @rdname api-table
bq_table_nrow <- function(x) {
  meta <- bq_table_meta(x, fields = "numRows")
  as.numeric(meta$numRows)
}

#' @export
#' @rdname api-table
bq_table_exists <- function(x) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  bq_exists(url)
}

#' @export
#' @rdname api-table
bq_table_delete <- function(x) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  invisible(bq_delete(url))
}

#' @export
#' @rdname api-table
#' @inheritParams bq_perform_copy
#' @param dest Source and destination [bq_table]s.
bq_table_copy <- function(
  x,
  dest,
  ...,
  quiet = getOption("bigrquery.quiet", NA)
) {
  x <- as_bq_table(x)
  dest <- as_bq_table(dest)

  job <- bq_perform_copy(x, dest, ...)
  bq_job_wait(job, quiet = quiet)

  dest
}

#' @export
#' @rdname api-table
#' @inheritParams api-perform
bq_table_upload <- function(
  x,
  values,
  ...,
  quiet = getOption("bigrquery.quiet", NA),
  json_digits = NULL
) {
  x <- as_bq_table(x)
  json_digits <- check_digits(json_digits)

  job <- bq_perform_upload(x, values, ..., json_digits = json_digits)
  bq_job_wait(job, quiet = quiet)

  invisible(x)
}

#' @export
#' @rdname api-table
bq_table_save <- function(
  x,
  destination_uris,
  ...,
  quiet = getOption("bigrquery.quiet", NA)
) {
  x <- as_bq_table(x)

  job <- bq_perform_extract(x, destination_uris = destination_uris, ...)
  bq_job_wait(job, quiet = quiet)

  invisible(x)
}

#' @export
#' @rdname api-table
bq_table_load <- function(
  x,
  source_uris,
  ...,
  quiet = getOption("bigrquery.quiet", NA)
) {
  x <- as_bq_table(x)

  job <- bq_perform_load(x, source_uris = source_uris, ...)
  bq_job_wait(job, quiet = quiet)

  invisible(x)
}

#' Load data from Cloud Storage with a quality gate
#'
#' `bq_table_load_staged()` loads data into a staging table, then uses the
#' load statistics to decide whether the batch is good enough to publish to
#' the target table `x`. A load job can succeed overall while silently
#' discarding rows with errors, so this function checks the number of
#' discarded rows against `max_bad_records` before copying the staged
#' results to `x`:
#'
#' * If the number of discarded rows is within `max_bad_records`, the staged
#'   results are published to `x` and the report gives the total number of
#'   rows in the batch and the number that were discarded.
#' * If the number of discarded rows exceeds `max_bad_records`, `x` is left
#'   untouched. The staged results remain in `staging` and the report
#'   includes the original load `job`, so you can inspect the job record to
#'   find out what went wrong.
#'
#' The staging load always replaces the contents of `staging` and allows
#' BigQuery to discard (and count) any number of bad records, so that the
#' quality gate always has load statistics to work with.
#'
#' @param x A [bq_table], or an object coercible to a `bq_table`, that the
#'   batch should be published to.
#' @param source_uris The fully-qualified URIs that point to your data in
#'   Google Cloud Storage, as described in [bq_perform_load()].
#' @param staging A [bq_table], or an object coercible to a `bq_table`, used
#'   to stage the loaded data before it is published to `x`.
#' @param max_bad_records The maximum number of discarded rows to tolerate
#'   before the batch is considered unfit to publish.
#' @param write_disposition Specifies what happens to the data in `x` when a
#'   batch is published. One of "WRITE_APPEND" (the default, add the batch
#'   to the existing rows), "WRITE_TRUNCATE" (replace the existing rows), or
#'   "WRITE_EMPTY" (fail if `x` already contains data).
#' @param ... Additional arguments passed on to [bq_perform_load()].
#' @inheritParams api-job
#' @return A list describing the outcome of the load, with components:
#'   * `published`: `TRUE` if the batch was published to `x`.
#'   * `rows_total`: total number of rows in the batch.
#'   * `rows_loaded`: number of rows loaded into the staging table.
#'   * `rows_discarded`: number of rows discarded because they were invalid.
#'   * `max_bad_records`: the tolerance the batch was checked against.
#'   * `job`: the [bq_job] that loaded the data into the staging table.
#'   * `staging`: the [bq_table] holding the staged results.
#' @export
#' @examplesIf bq_testable()
#' ds <- bq_test_dataset()
#' bq_mtcars <- bq_table(ds, "mtcars")
#' bq_table_upload(bq_mtcars, mtcars)
#'
#' gs <- gs_test_object()
#' bq_table_save(bq_mtcars, gs)
#'
#' report <- bq_table_load_staged(
#'   bq_table(ds, "mtcars_copy"),
#'   gs,
#'   staging = bq_table(ds, "mtcars_staging"),
#'   max_bad_records = 10
#' )
#' report$published
#' report$rows_total
bq_table_load_staged <- function(
  x,
  source_uris,
  staging,
  max_bad_records = 0,
  write_disposition = "WRITE_APPEND",
  ...,
  quiet = getOption("bigrquery.quiet", NA)
) {
  x <- as_bq_table(x)
  staging <- as_bq_table(staging)
  check_number_whole(max_bad_records, min = 0)
  check_string(write_disposition)
  quiet <- check_quiet(quiet)

  # Load into the staging table, allowing BigQuery to discard (and count)
  # any number of bad records, so that load statistics are always available
  # to check against `max_bad_records`.
  job <- bq_perform_load(
    staging,
    source_uris,
    write_disposition = "WRITE_TRUNCATE",
    max_bad_records = .Machine$integer.max,
    ...
  )
  bq_job_wait(job, quiet = quiet)

  stats <- bq_job_load_stats(job)
  loaded <- stats$output_rows
  discarded <- stats$bad_records
  report <- list(
    published = FALSE,
    rows_total = loaded + discarded,
    rows_loaded = loaded,
    rows_discarded = discarded,
    max_bad_records = max_bad_records,
    job = job,
    staging = staging
  )

  if (discarded > max_bad_records) {
    if (!quiet) {
      job_id <- as.character(job)
      staging_id <- toString(staging)
      cli::cli_inform(c(
        "!" = "Load rejected: {discarded} bad record{?s}.",
        "i" = "Limit is {max_bad_records} bad record{?s}.",
        "i" = "Staged results left in {staging_id}; see job {job_id}."
      ))
    }
    return(report)
  }

  bq_table_copy(
    staging,
    x,
    write_disposition = write_disposition,
    quiet = quiet
  )
  report$published <- TRUE
  if (!quiet) {
    target_id <- toString(x)
    cli::cli_inform(
      "Published {loaded} row{?s} to {target_id} ({discarded} discarded)."
    )
  }

  report
}

#' @export
#' @rdname api-table
bq_table_patch <- function(x, fields) {
  x <- as_bq_table(x)

  url <- bq_path(x$project, x$dataset, x$table)
  body <- list(
    tableReference = tableReference(x)
  )
  fields <- as_bq_fields(fields)
  body$schema <- list(fields = as_json(fields))
  bq_patch(url, body)
}
