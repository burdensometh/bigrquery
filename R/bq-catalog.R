#' Resumable project catalogs
#'
#' @description
#' A catalog incrementally records the datasets and tables in one or more
#' BigQuery projects. Pagination positions are saved to disk after every
#' successfully retrieved page, keyed by project and dataset identity, so a
#' job that is interrupted half way through resumes exactly where it stopped
#' instead of re-reading (and mixing in) old pages.
#'
#' Each fully enumerated dataset can be committed to an immutable catalog
#' *version*. Datasets that cannot be read because of missing permissions are
#' recorded as `access_denied` rather than being mistaken for empty datasets,
#' and their tables from previous versions are retained.
#'
#' Catalogs are stored as plain JSON files under `path`, so they survive
#' restarts and can be inspected or backed up like any other file.
#'
#' @param path Directory in which to store the catalog. It is created if it
#'   does not already exist.
#'
#' @section Storage layout:
#' ```
#' path/
#'   catalog.json
#'   state/
#'     <project>__<dataset>.json
#'   versions/
#'     <version-id>.json
#' ```
#' `catalog.json` holds the committed versions, `state/` holds the resumable
#' pagination position of every dataset that has been (or is being)
#' enumerated, and `versions/` holds one immutable snapshot per committed
#' version.
#'
#' @examples
#' # A catalog lives in a local directory
#' catalog <- bq_catalog(tempfile())
#' catalog
#'
#' @name bq-catalog
NULL

#' @rdname bq-catalog
#' @export
bq_catalog <- function(path) {
  check_string(path)
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(path, "state"), showWarnings = FALSE)
  dir.create(file.path(path, "versions"), showWarnings = FALSE)

  catalog_path <- file.path(path, "catalog.json")
  if (!file.exists(catalog_path)) {
    catalog_write_json(
      catalog_path,
      list(
        version = 1L,
        current_version = NULL,
        datasets = list()
      )
    )
  }

  structure(list(path = normalizePath(path, mustWork = TRUE)),
    class = "bq_catalog"
  )
}

#' @export
print.bq_catalog <- function(x, ...) {
  cat_line("<bq_catalog> ", x$path)
  datasets <- catalog_progress_all(x)
  statuses <- table(vapply(datasets, function(d) d$status, character(1)))
  if (length(statuses) > 0) {
    for (status in names(statuses)) {
      cat_line(
        "  ",
        status,
        ": ",
        statuses[[status]],
        " dataset",
        if (statuses[[status]] != 1) "s"
      )
    }
  }
  invisible(x)
}

# progress ----------------------------------------------------------------

#' Resumable pagination within a catalog
#'
#' `bq_catalog_project_datasets()` enumerates the datasets in a project and
#' `bq_catalog_dataset_tables()` enumerates the tables in a dataset. Both
#' functions save their pagination position after every page: if the process
#' is interrupted, calling them again with the same catalog and identity
#' resumes from the saved `nextPageToken` instead of restarting from page one.
#'
#' Each dataset-level state also carries a *request signature* derived from
#' the URL and query parameters. Resuming with different parameters discards
#' the old position and restarts, so a stale token can never be combined with
#' a different request.
#'
#' Permission failures (HTTP 403) do not stop the catalog run. The dataset is
#' marked `access_denied` with the server's error reason and message, and the
#' function continues with the next item.
#'
#' @param catalog A `bq_catalog` created by [bq_catalog()].
#' @param project,dataset Project and dataset identifiers.
#' @param page_size Number of items requested per API page.
#' @param ... Additional query parameters passed to the API.
#'
#' @return Both functions return the catalog, invisibly, so calls can be
#'   chained. Inspect progress with [bq_catalog_status()] and commit results
#'   with [bq_catalog_commit()].
#' @name bq-catalog-progress
NULL

#' @rdname bq-catalog-progress
#' @export
bq_catalog_project_datasets <- function(
  catalog,
  project,
  page_size = 50,
  ...
) {
  check_string(project)
  state <- catalog_progress(catalog, kind = "project", project = project)
  url <- bq_path(project, "")
  query <- list(fields = "datasets(datasetReference,kind)")
  catalog_enumerate(
    catalog = catalog,
    state = state,
    url = url,
    query = query,
    page_size = page_size,
    extra = list(...),
    item_key = "datasets",
    ref_fun = function(x) {
      ref <- x$datasetReference
      list(project = ref$projectId, dataset = ref$datasetId)
    }
  )
  invisible(catalog)
}

#' @rdname bq-catalog-progress
#' @export
bq_catalog_dataset_tables <- function(
  catalog,
  project,
  dataset,
  page_size = 50,
  ...
) {
  check_string(project)
  check_string(dataset)
  state <- catalog_progress(
    catalog,
    kind = "dataset",
    project = project,
    dataset = dataset
  )
  x <- bq_dataset(project, dataset)
  url <- bq_path(x$project, x$dataset, "")
  query <- list(fields = "tables(tableReference,type)")
  catalog_enumerate(
    catalog = catalog,
    state = state,
    url = url,
    query = query,
    page_size = page_size,
    extra = list(...),
    item_key = "tables",
    ref_fun = function(x) {
      ref <- x$tableReference
      list(
        project = ref$projectId,
        dataset = ref$datasetId,
        table = ref$tableId,
        type = x$type
      )
    }
  )
  invisible(catalog)
}

# versions ----------------------------------------------------------------

#' Commit and inspect catalog versions
#'
#' A catalog version is an immutable snapshot built only from datasets whose
#' table listing has been read to completion. Datasets still in progress (an
#' interrupted run) are excluded from the new version and keep their previous
#' committed state; datasets marked `access_denied` are recorded with an
#' explicit flag so they are never confused with empty datasets or with
#' datasets whose tables were deleted, and their previously committed tables
#' are retained.
#'
#' @param catalog A `bq_catalog` created by [bq_catalog()].
#' @param only_complete If `TRUE` (the default), an error is raised when one
#'   or more datasets are still in progress, rather than silently committing
#'   without them.
#' @return
#' * `bq_catalog_commit()` returns the catalog, invisibly.
#' * `bq_catalog_status()` returns a data frame with one row per dataset.
#' * `bq_catalog_version()` returns the current committed version as a list,
#'   or `NULL` if no version has been committed yet.
#' * `bq_catalog_tables()` returns the committed tables for one dataset as a
#'   data frame, with an `access_denied` attribute set for unreadable
#'   datasets.
#' @name bq-catalog-versions
NULL

#' @rdname bq-catalog-versions
#' @export
bq_catalog_commit <- function(catalog, only_complete = TRUE) {
  states <- catalog_progress_all(catalog)
  # Project-level states are dataset-discovery progress, not catalog entries:
  # versions are built from dataset listings only.
  states <- Filter(
    function(state) !identical(state$kind, "project"),
    states
  )
  statuses <- vapply(states, function(x) x$status, character(1))

  if (only_complete && any(statuses == "in_progress")) {
    pending <- vapply(
      states[statuses == "in_progress"],
      catalog_state_name,
      character(1)
    )
    cli::cli_abort(
      c(
        paste0(
          "Can't commit catalog while {length(pending)} dataset{?s} ",
          "still in progress:"
        ),
        i = paste0(
          "Re-run {.fun bq_catalog_dataset_tables} for {.val {pending}} ",
          "or set {.arg only_complete = FALSE}."
        )
      ),
      class = "bigrquery_catalog_incomplete",
      call = caller_env()
    )
  }

  root <- catalog_read(catalog)
  previous <- catalog_committed_datasets(root)
  ready <- states[statuses %in% c("complete", "access_denied")]

  committed <- lapply(ready, function(state) {
    catalog_commit_dataset(state, previous[[catalog_state_name(state)]])
  })
  names(committed) <- vapply(ready, catalog_state_name, character(1))
  # Datasets that were committed before but have no state this run keep their
  # previous committed entry unchanged.
  keep <- setdiff(names(previous), names(committed))
  committed <- c(committed, previous[keep])

  id <- root$version
  version <- list(
    id = id,
    committed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    datasets = committed
  )
  catalog_write_json(
    file.path(catalog$path, "versions", paste0(id, ".json")),
    version
  )

  root$current_version <- id
  root$version <- id + 1L
  root$datasets <- committed
  catalog_write_json(file.path(catalog$path, "catalog.json"), root)
  invisible(catalog)
}

#' @rdname bq-catalog-versions
#' @export
bq_catalog_status <- function(catalog) {
  states <- catalog_progress_all(catalog)
  # Project-level discovery progress is reported via its state file, but
  # status is one row per dataset as documented.
  states <- Filter(
    function(state) !identical(state$kind, "project"),
    states
  )
  if (length(states) == 0) {
    return(data.frame(
      project = character(),
      dataset = character(),
      status = character(),
      n_tables = integer(),
      pages_read = integer(),
      reason = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(states, function(state) {
    data.frame(
      project = state$project,
      dataset = state$dataset %||% NA_character_,
      status = state$status,
      n_tables = length(state$tables),
      pages_read = state$pages_read %||% 0L,
      reason = state$error$reason %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @rdname bq-catalog-versions
#' @export
bq_catalog_version <- function(catalog) {
  root <- catalog_read(catalog)
  if (is.null(root$current_version)) {
    return(NULL)
  }
  catalog_read_json(
    file.path(
      catalog$path,
      "versions",
      paste0(root$current_version, ".json")
    )
  )
}

#' @rdname bq-catalog-versions
#' @param project,dataset Identifiers of the dataset whose committed tables
#'   are requested.
#' @export
bq_catalog_tables <- function(catalog, project, dataset) {
  check_string(project)
  check_string(dataset)
  root <- catalog_read(catalog)
  entry <- root$datasets[[catalog_identity(project, dataset)]]
  if (is.null(entry)) {
    cli::cli_abort(
      paste0(
        "Dataset {.val {project}.{dataset}} has not been committed ",
        "to this catalog."
      ),
      class = "bigrquery_catalog_unknown_dataset",
      call = caller_env()
    )
  }

  tables <- entry$tables %||% list()
  out <- data.frame(
    project = map_chr(tables, function(x) x$project %||% project),
    dataset = map_chr(tables, function(x) x$dataset %||% dataset),
    table = map_chr(tables, function(x) x$table),
    type = map_chr(tables, function(x) x$type %||% NA_character_),
    stringsAsFactors = FALSE
  )
  attr(out, "access_denied") <- isTRUE(entry$access_denied)
  attr(out, "error") <- entry$error
  out
}

# enumeration engine --------------------------------------------------------

catalog_enumerate <- function(
  catalog,
  state,
  url,
  query,
  page_size,
  extra,
  item_key,
  ref_fun
) {
  check_number_whole(page_size, min = 1)
  query <- utils::modifyList(query, extra)
  signature <- catalog_signature(url, query, page_size)

  if (is.null(state$signature)) {
    state <- catalog_state_init(state, url, query, page_size, signature)
  } else if (!identical(state$signature, signature)) {
    # The request shape changed: a token issued for the old request would
    # produce pages belonging to a different listing, so restart.
    state <- catalog_state_init(state, url, query, page_size, signature)
  }

  if (identical(state$status, "complete") ||
    identical(state$status, "access_denied")) {
    # Terminal states: re-running a completed listing is a no-op, and a
    # denied dataset stays denied until its state file is removed.
    return(invisible(state))
  }

  lock <- catalog_lock(catalog, state)
  on.exit(catalog_unlock(lock), add = TRUE)

  state <- tryCatch(
    catalog_fetch_pages(catalog, state, item_key, ref_fun, lock),
    bigrquery_http_403 = function(cnd) {
      # Missing permission is a terminal state for this run, not an empty
      # dataset. Record the reason and stop fetching.
      state$status <- "access_denied"
      state$error <- list(
        reason = cnd$reason %||% "accessDenied",
        message = cli_escape(conditionMessage(cnd))
      )
      state$finished_at <- catalog_timestamp()
      catalog_state_save(catalog, state)
      state
    },
    bigrquery_http_400 = function(cnd) {
      if (!catalog_token_expired(cnd)) {
        stop(cnd)
      }
      # A page token can expire between scheduled runs. The saved position
      # is unusable, so restart the listing rather than failing the run.
      state <- catalog_state_init(
        state,
        url = state$url,
        query = state$query,
        page_size = state$page_size,
        signature = state$signature
      )
      warning(
        "Saved page token for ",
        catalog_state_name(state),
        " was rejected by the API; the listing was restarted.",
        call. = FALSE
      )
      catalog_fetch_pages(catalog, state, item_key, ref_fun, lock)
    }
  )
  invisible(state)
}

catalog_token_expired <- function(cnd) {
  reason <- cnd$reason
  !is.null(reason) && grepl("pageToken|invalid", reason)
}

catalog_fetch_pages <- function(catalog, state, item_key, ref_fun, lock) {
  repeat {
    page <- bq_get(state$url, query = catalog_page_query(state))

    items <- page[[item_key]] %||% list()
    refs <- lapply(items, ref_fun)
    state$tables <- catalog_merge_refs(state$tables, refs)
    state$pages_read <- (state$pages_read %||% 0L) + 1L
    state$next_page_token <- page$nextPageToken
    state$updated_at <- catalog_timestamp()

    if (is.null(page$nextPageToken)) {
      state$status <- "complete"
      state$next_page_token <- NULL
      state$finished_at <- catalog_timestamp()
      catalog_state_save(catalog, state)
      break
    }

    # Checkpoint after every successful page: an interruption loses at most
    # the page in flight, and resume starts from the saved nextPageToken.
    catalog_state_save(catalog, state)
    catalog_lock_touch(lock)
  }
  state
}

catalog_page_query <- function(state) {
  query <- state$query
  if (!is.null(query$fields) &&
    !grepl("nextPageToken", query$fields, fixed = TRUE)) {
    query$fields <- paste0(query$fields, ",nextPageToken")
  }
  query$maxResults <- state$page_size
  if (!is.null(state$next_page_token)) {
    query$pageToken <- state$next_page_token
  } else {
    query$pageToken <- NULL
  }
  query
}

# state files ---------------------------------------------------------------

catalog_progress <- function(catalog, kind, project, dataset = NULL) {
  path <- catalog_state_path(catalog, project, dataset)
  if (file.exists(path)) {
    state <- catalog_read_json(path)
  } else {
    state <- list(
      kind = kind,
      project = project,
      dataset = dataset,
      status = "new",
      tables = list(),
      pages_read = 0L
    )
  }
  state
}

catalog_progress_all <- function(catalog) {
  paths <- list.files(
    file.path(catalog$path, "state"),
    pattern = "\\.json$",
    full.names = TRUE
  )
  lapply(paths, catalog_read_json)
}

catalog_state_init <- function(state, url, query, page_size, signature) {
  state$url <- url
  state$query <- query
  state$page_size <- page_size
  state$signature <- signature
  state$next_page_token <- NULL
  state$tables <- list()
  state$pages_read <- 0L
  state$status <- "in_progress"
  state$error <- NULL
  state$created_at <- catalog_timestamp()
  state$updated_at <- state$created_at
  state$finished_at <- NULL
  state
}

catalog_state_save <- function(catalog, state) {
  catalog_write_json(
    catalog_state_path(catalog, state$project, state$dataset),
    state
  )
}

catalog_state_path <- function(catalog, project, dataset) {
  file.path(
    catalog$path,
    "state",
    paste0(catalog_identity(project, dataset), ".json")
  )
}

catalog_state_name <- function(state) {
  catalog_identity(state$project, state$dataset)
}

catalog_identity <- function(project, dataset = NULL) {
  # Dataset ids may contain dots but not double underscores in practice;
  # percent-encode underscores defensively to keep the key unambiguous.
  safe <- function(x) gsub("_", "%5F", x, fixed = TRUE)
  if (is.null(dataset)) {
    safe(project)
  } else {
    paste0(safe(project), "__", safe(dataset))
  }
}

catalog_signature <- function(url, query, page_size) {
  body <- jsonlite::toJSON(
    list(url = url, query = query[names(query) != "pageToken"],
      page_size = page_size),
    auto_unbox = TRUE
  )
  substr(rlang::hash(body), 1, 16)
}

catalog_merge_refs <- function(existing, refs) {
  if (length(refs) == 0) {
    return(existing)
  }
  keys <- c(
    "project",
    "dataset",
    if ("table" %in% names(refs[[1]])) "table"
  )
  key_of <- function(x) paste(unlist(x[keys]), collapse = "/")
  index <- new.env(parent = emptyenv())
  for (ref in existing) {
    assign(key_of(ref), ref, envir = index)
  }
  for (ref in refs) {
    assign(key_of(ref), ref, envir = index)
  }
  lapply(ls(index, sorted = TRUE), function(k) get(k, envir = index))
}

catalog_commit_dataset <- function(state, previous) {
  if (identical(state$status, "access_denied")) {
    # Keep the historical tables: missing permission must not look like the
    # tables were deleted, but flag the dataset so consumers know the list
    # may be stale.
    tables <- if (!is.null(previous)) previous$tables else list()
    list(
      project = state$project,
      dataset = state$dataset,
      access_denied = TRUE,
      error = state$error,
      tables = tables,
      pages_read = state$pages_read %||% 0L,
      updated_at = state$updated_at
    )
  } else {
    list(
      project = state$project,
      dataset = state$dataset,
      access_denied = FALSE,
      error = NULL,
      tables = state$tables,
      pages_read = state$pages_read,
      updated_at = state$finished_at %||% state$updated_at
    )
  }
}

catalog_committed_datasets <- function(root) {
  root$datasets %||% list()
}

# locking -------------------------------------------------------------------

catalog_lock <- function(catalog, state) {
  path <- paste0(
    catalog_state_path(catalog, state$project, state$dataset),
    ".lock"
  )
  wait <- 0
  while (!dir.create(path, showWarnings = FALSE)) {
    # A lock older than the stale threshold is presumed left behind by a
    # crashed process: the owning process refreshes it on every page.
    owner <- file.path(path, "owner.json")
    age <- if (file.exists(owner)) {
      difftime(
        Sys.time(),
        file.info(owner)$mtime,
        units = "secs"
      )
    } else {
      Inf
    }
    if (age > catalog_lock_stale_secs) {
      warning(
        "Removing stale catalog lock on ",
        catalog_state_name(state),
        call. = FALSE
      )
      unlink(path, recursive = TRUE)
      next
    }

    wait <- wait + 1
    if (wait > 50) {
      cli::cli_abort(
        paste0(
          "Timed out waiting for catalog lock on ",
          "{.val {catalog_state_name(state)}}."
        ),
        class = "bigrquery_catalog_locked",
        call = caller_env()
      )
    }
    Sys.sleep(0.1)
  }
  catalog_lock_touch(path)
  path
}

catalog_lock_stale_secs <- 3600

catalog_lock_touch <- function(path) {
  writeLines(
    jsonlite::toJSON(
      list(pid = Sys.getpid(), at = catalog_timestamp()),
      auto_unbox = TRUE
    ),
    file.path(path, "owner.json")
  )
}

catalog_unlock <- function(path) {
  unlink(path, recursive = TRUE)
}

# json helpers --------------------------------------------------------------

catalog_read <- function(catalog) {
  catalog_read_json(file.path(catalog$path, "catalog.json"))
}

catalog_read_json <- function(path) {
  jsonlite::read_json(path, simplifyVector = FALSE)
}

catalog_write_json <- function(path, data) {
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  jsonlite::write_json(
    data,
    tmp,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    digits = NA
  )
  file.rename(tmp, path)
}

catalog_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}
