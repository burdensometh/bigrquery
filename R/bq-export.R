#' Export a table together with a manifest
#'
#' @description
#' `bq_table_save_manifest()` is like [bq_table_save()], but it is designed for
#' scheduled exports that are consumed by another process. In addition to
#' writing the data to Google Cloud Storage, it records the identity of the
#' extract job and the exact list of objects produced by that job in a JSON
#' manifest. The manifest is published only after the job has succeeded and the
#' shards found in the bucket have been verified against the file counts
#' reported by the job, so a consumer can never observe a partial batch.
#'
#' Each call writes its shards to a fresh, run-specific prefix inside `prefix`,
#' so a re-run can never mix objects from two batches. Supplying a `run` that
#' already contains objects is an error.
#'
#' An export that produces no data is a success, not a failure: the manifest is
#' still published, with `status = "empty"` and an empty `objects` list. If the
#' extract job fails, or the shards cannot be verified, no manifest is
#' published and an error is thrown. These are deliberately independent states,
#' so a consumer can tell an empty batch apart from a broken one.
#'
#' Consumers can discover batches by listing `prefix` for objects named
#' `manifest.json` and reading them with [bq_export_read()], or by reading the
#' JSON manifest directly.
#'
#' @param x A [bq_table].
#' @param bucket Name of the Google Cloud Storage bucket to write to, without
#'   the `gs://` prefix.
#' @param prefix An optional object name prefix (a "folder") below which the
#'   run-specific prefix is created. A trailing slash is optional.
#' @param ... Passed on to [bq_perform_extract()], e.g. `destination_format`,
#'   `compression`, `print_header`, `labels`, or `billing`.
#' @param run A string identifying this run, used as the final component of the
#'   run-specific prefix. Must be a single path segment that does not contain
#'   `/`. Defaults to a unique value derived from the current time.
#' @param file_name The name of the exported files. Must contain exactly one
#'   `*`, which BigQuery replaces with a shard number, e.g. `"part-*.parquet"`.
#' @param quiet If `FALSE`, displays progress bar; if `TRUE` is silent;
#'   if `NA` picks based on whether or not you're in an interactive context.
#'
#' @return
#' A `bq_export` object, a list with fields:
#' * `status`: either `"complete"` (shards were written and verified) or
#'   `"empty"` (the job succeeded but exported no data).
#' * `job`: the [bq_job] that performed the export.
#' * `table`: the exported [bq_table].
#' * `bucket`, `run`, `object_prefix`: the bucket and the run-specific object
#'   prefix, e.g. `"exports/mtcars/20260101T000000Z-1a2b3c4d/"`.
#' * `destination_uri_pattern`: the wildcard URI that BigQuery exported to.
#' * `objects`: a tibble with one row per shard and columns `name`, `uri`,
#'   `size`, and `generation`, as reported by Cloud Storage.
#' * `manifest`: a `gs_object` that points at the published manifest,
#'   `manifest.json` in the run-specific prefix.
#' * `manifest_uri`: the same object as a `gs://` URI.
#' * `created_at`: the time the manifest was published, in UTC.
#'
#' @seealso [bq_table_save()], [bq_export_read()]
#' @examplesIf bq_testable() && nzchar(Sys.getenv("BIGQUERY_TEST_BUCKET"))
#' ds <- bq_test_dataset()
#' tb <- bq_table_upload(bq_table(ds, "mtcars"), mtcars)
#'
#' export <- bq_table_save_manifest(
#'   tb,
#'   bucket = gs_test_bucket(),
#'   prefix = "exports/mtcars"
#' )
#' export
#'
#' # Another process can consume the published manifest
#' bq_export_read(export$manifest)
#' @export
bq_table_save_manifest <- function(
  x,
  bucket,
  prefix = NULL,
  ...,
  run = NULL,
  file_name = "part-*",
  quiet = getOption("bigrquery.quiet", NA)
) {
  x <- as_bq_table(x)
  check_bucket(bucket)
  prefix <- normalize_object_prefix(prefix)
  run <- run %||% new_run_id()
  check_run_id(run)
  check_file_name(file_name)
  check_export_dots(list(...))

  object_prefix <- paste0(prefix, run, "/")
  manifest <- gs_object(bucket, paste0(object_prefix, "manifest.json"))

  # The run prefix must be empty. Otherwise a re-run would mix objects from two
  # batches and the manifest would describe files that this job did not write.
  existing <- gs_object_list(bucket, object_prefix)
  if (nrow(existing) > 0) {
    cli::cli_abort(
      c(
        "Export run {.val {run}} already contains {nrow(existing)}
         object{?s}.",
        i = "Each run must write to an empty prefix, so the objects of two
             batches are never mixed.",
        i = "Use a new {.arg run}, or delete the objects below
             {.val {paste0('gs://', bucket, '/', object_prefix)}} first."
      ),
      class = "bigrquery_export_not_empty"
    )
  }

  destination_uris <- paste0("gs://", bucket, "/", object_prefix, file_name)
  job <- bq_perform_extract(x, destination_uris = destination_uris, ...)

  tryCatch(
    bq_job_wait(job, quiet = quiet),
    error = function(err) {
      cli::cli_abort(
        c(
          "Export job {.val {as.character(job)}} failed.",
          x = "No manifest was published.",
          i = "Use a new {.arg run} to try again; any partial shards are left
               in place."
        ),
        class = "bigrquery_export_failed",
        parent = err
      )
    }
  )

  meta <- bq_job_meta(
    job,
    fields = "statistics(extract(destinationUriFileCounts))"
  )
  counts <- meta$statistics$extract$destinationUriFileCounts
  if (is.null(counts)) {
    abort_export_incomplete(
      "BigQuery did not report destinationUriFileCounts."
    )
  }
  expected <- sum(as.numeric(unlist(counts, use.names = FALSE)))
  if (!is.finite(expected)) {
    abort_export_incomplete(
      "BigQuery returned invalid extract file counts."
    )
  }

  objects <- gs_object_list(bucket, object_prefix)
  problem <- export_shard_problem(objects, expected, object_prefix)
  if (is.null(problem)) {
    status <- if (nrow(objects) == 0 || all(objects$size == 0)) {
      "empty"
    } else {
      "complete"
    }
  } else {
    abort_export_incomplete(problem)
  }

  out <- new_bq_export(
    status = status,
    job = job,
    table = x,
    bucket = bucket,
    run = run,
    object_prefix = object_prefix,
    destination_uri_pattern = destination_uris,
    objects = objects,
    manifest = manifest
  )

  # Publish the manifest last, and never overwrite an existing one: consumers
  # only ever see a manifest that describes a fully verified batch.
  tryCatch(
    gs_object_write(
      manifest,
      bq_export_manifest_json(out),
      if_generation_match = 0
    ),
    bigrquery_http_412 = function(err) {
      cli::cli_abort(
        c(
          "The manifest {.val {out$manifest_uri}} already exists.",
          i = "Manifests are never overwritten; use a new {.arg run}."
        ),
        class = "bigrquery_export_manifest_exists",
        parent = err
      )
    }
  )

  out
}

#' Read an export manifest
#'
#' Reads a manifest published by [bq_table_save_manifest()] back into R. This
#' is mostly useful for testing and for R consumers of the export; other
#' processes can read the JSON object directly.
#'
#' @param x A `gs_object`, or a `gs://` URI, that points at a manifest.
#' @return A `bq_export` object, as described in [bq_table_save_manifest()].
#' @seealso [bq_table_save_manifest()]
#' @export
bq_export_read <- function(x) {
  manifest <- as_gs_object(x)
  json <- jsonlite::fromJSON(
    gs_object_read(manifest),
    simplifyVector = FALSE
  )

  if (!is.list(json) || is.null(json$status)) {
    cli::cli_abort(
      "{.arg x} does not contain an export manifest.",
      class = "bigrquery_export_invalid"
    )
  }
  if (!json$status %in% c("complete", "empty")) {
    cli::cli_abort(
      "{.arg x} contains an unknown export status {.val {json$status}}.",
      class = "bigrquery_export_invalid"
    )
  }
  if (is.null(json$job$project) || is.null(json$job$jobId)) {
    cli::cli_abort(
      "{.arg x} does not record the identity of the export job.",
      class = "bigrquery_export_invalid"
    )
  }
  if (is.null(json$sourceTable)) {
    cli::cli_abort(
      "{.arg x} does not record the exported table.",
      class = "bigrquery_export_invalid"
    )
  }

  new_bq_export(
    status = json$status,
    job = bq_job(
      json$job$project,
      json$job$jobId,
      json$job$location %||% "US"
    ),
    table = as_bq_table(json$sourceTable),
    bucket = json$bucket,
    run = json$run,
    object_prefix = json$objectPrefix,
    destination_uri_pattern = json$destinationUriPattern,
    objects = manifest_objects_tibble(json$objects),
    manifest = manifest,
    created_at = json$createdAt
  )
}

#' @export
print.bq_export <- function(x, ...) {
  cat_line("<bq_export> ", x$status)
  cat_line("  Job:      ", as.character(x$job))
  cat_line("  Table:    ", toString(x$table))
  cat_line("  Prefix:   ", x$object_prefix)
  cat_line("  Shards:   ", nrow(x$objects))
  cat_line("  Manifest: ", x$manifest_uri)
  invisible(x)
}

new_bq_export <- function(
  status,
  job,
  table,
  bucket,
  run,
  object_prefix,
  destination_uri_pattern,
  objects,
  manifest,
  created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
) {
  structure(
    list(
      status = status,
      job = job,
      table = table,
      bucket = bucket,
      run = run,
      object_prefix = object_prefix,
      destination_uri_pattern = destination_uri_pattern,
      objects = objects,
      manifest = manifest,
      manifest_uri = format(manifest),
      created_at = created_at
    ),
    class = "bq_export"
  )
}

bq_export_manifest_json <- function(x) {
  job <- x$job
  body <- list(
    run = x$run,
    status = x$status,
    job = list(
      project = job$project,
      jobId = job$job,
      location = job$location
    ),
    sourceTable = toString(x$table),
    bucket = x$bucket,
    objectPrefix = x$object_prefix,
    destinationUriPattern = x$destination_uri_pattern,
    shardCount = nrow(x$objects),
    objects = manifest_objects_json(x$objects),
    manifestUri = x$manifest_uri,
    createdAt = x$created_at
  )
  jsonlite::toJSON(
    body,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    digits = NA
  )
}

manifest_objects_json <- function(objects) {
  lapply(seq_len(nrow(objects)), function(i) {
    list(
      name = objects$name[[i]],
      uri = objects$uri[[i]],
      size = objects$size[[i]],
      generation = objects$generation[[i]]
    )
  })
}

manifest_objects_tibble <- function(objects) {
  objects <- objects %||% list()
  tibble::tibble(
    name = map_chr(objects, function(x) as.character(x$name %||% NA)),
    uri = map_chr(objects, function(x) as.character(x$uri %||% NA)),
    size = if (length(objects) == 0) {
      numeric()
    } else {
      vapply(objects, function(x) as.numeric(x$size), numeric(1))
    },
    generation = map_chr(
      objects,
      function(x) as.character(x$generation %||% NA)
    )
  )
}

# Returns `NULL` if the listing contains exactly the shards that the job says
# it wrote, or a string explaining the problem. The job succeeding is not
# enough: the listing itself must be verified before a manifest is published.
export_shard_problem <- function(objects, expected, object_prefix) {
  if (nrow(objects) != expected) {
    return(cli::format_inline(
      "The job reported {expected} shard{?s}, but {nrow(objects)}
       object{?s} {?was/were} found below {.val {object_prefix}}."
    ))
  }
  if (nrow(objects) == 0) {
    return(NULL)
  }
  if (anyNA(objects$size)) {
    return("Cloud Storage did not report a size for every shard.")
  }
  if (!all(grepl("^gs://", objects$uri))) {
    return("Cloud Storage did not report a URI for every shard.")
  }
  if (!all(objects$size == 0) && any(objects$size == 0)) {
    return("At least one shard is empty (0 bytes), so the export is incomplete.")
  }
  NULL
}

abort_export_incomplete <- function(message, call = caller_env()) {
  cli::cli_abort(
    c(
      "Export shards could not be verified, so no manifest was published.",
      x = message
    ),
    class = "bigrquery_export_incomplete",
    call = call
  )
}

# Input helpers -----------------------------------------------------------

check_bucket <- function(bucket, arg = caller_arg(bucket), call = caller_env()) {
  check_string(bucket, allow_empty = FALSE, arg = arg, call = call)
  if (grepl("^gs://", bucket)) {
    cli::cli_abort(
      "{.arg {arg}} must be a bucket name, not a {.val gs://} URI.",
      call = call
    )
  }
}

check_run_id <- function(run, arg = caller_arg(run), call = caller_env()) {
  check_string(run, allow_empty = FALSE, arg = arg, call = call)
  if (!grepl("^[A-Za-z0-9._-]+$", run) || run %in% c(".", "..")) {
    cli::cli_abort(
      "{.arg {arg}} must be a single path segment containing only letters,
       digits, dots, underscores, or dashes.",
      call = call
    )
  }
}

check_file_name <- function(
  file_name,
  arg = caller_arg(file_name),
  call = caller_env()
) {
  check_string(file_name, allow_empty = FALSE, arg = arg, call = call)
  stars <- nchar(gsub("[^*]", "", file_name))
  if (stars != 1 || grepl("/", file_name, fixed = TRUE)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single path segment containing exactly one
       {.val *}.",
      call = call
    )
  }
}

check_export_dots <- function(dots, call = caller_env()) {
  if (!is.null(dots$destination_uris)) {
    cli::cli_abort(
      c(
        "{.arg destination_uris} is controlled by {.fun bq_table_save_manifest}.",
        i = "Use {.arg bucket}, {.arg prefix}, {.arg run}, and {.arg file_name}
             instead."
      ),
      call = call
    )
  }
  invisible()
}

normalize_object_prefix <- function(
  prefix,
  arg = caller_arg(prefix),
  call = caller_env()
) {
  if (is.null(prefix)) {
    return("")
  }
  check_string(prefix, arg = arg, call = call)
  if (grepl("^gs://", prefix)) {
    cli::cli_abort(
      "{.arg {arg}} must be an object prefix, not a {.val gs://} URI.",
      call = call
    )
  }
  prefix <- gsub("^/+", "", prefix)
  prefix <- gsub("/+$", "", prefix)
  if (nzchar(prefix)) paste0(prefix, "/") else ""
}

new_run_id <- function() {
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  suffix <- sub("^file", "", basename(tempfile("file")))
  paste0(stamp, "-", suffix)
}

as_gs_object <- function(x, call = caller_env()) {
  if (inherits(x, "gs_object")) {
    return(x)
  }
  if (is_string(x) && grepl("^gs://[^/]+/.+$", x)) {
    rest <- sub("^gs://", "", x)
    bucket <- sub("/.*$", "", rest)
    object <- sub("^[^/]+/", "", rest)
    return(gs_object(bucket, object))
  }
  cli::cli_abort(
    "{.arg x} must be a {.cls gs_object} or a {.val gs://} URI.",
    call = call
  )
}
