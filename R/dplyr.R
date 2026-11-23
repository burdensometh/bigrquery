#' A BigQuery data source for dplyr.
#'
#' Create the connection to the database with `DBI::dbConnect()` then
#' use [dplyr::tbl()] to connect to tables within that database. Generally,
#' it's best to provide the fully qualified name of the table (i.e.
#' `project.dataset.table`) but if you supply a default `dataset` in the
#' connection, you can use just the table name. (This, however, will
#' prevent you from making joins across datasets.)
#'
#' @param project project id or name
#' @param dataset dataset name
#' @param billing billing project, if different to `project`
#' @param max_pages (IGNORED) maximum pages returned by a query
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # To run this example, replace billing with the id of one of your projects
#' # set up for billing
#' con <- DBI::dbConnect(bigquery(), project = bq_test_project())
#'
#' shakespeare <- con %>% tbl(I("publicdata.samples.shakespeare"))
#' shakespeare
#' shakespeare %>%
#'   group_by(word) %>%
#'   summarise(n = sum(word_count, na.rm = TRUE)) %>%
#'   arrange(desc(n))
#' }
src_bigquery <- function(project, dataset, billing = project, max_pages = 10) {
  check_installed("dbplyr")

  con <- DBI::dbConnect(
    bigquery(),
    project = project,
    dataset = dataset,
    billing = billing,
    use_legacy_sql = FALSE
  )

  dbplyr::src_dbi(con)
}

# registered onLoad
tbl.BigQueryConnection <- function(src, from, ...) {
  src <- dbplyr::src_dbi(src, auto_disconnect = FALSE)

  sql <- dbplyr::sql_query_fields(src$con, from)
  dataset <- if (!is.null(src$con@dataset)) as_bq_dataset(src$con)
  schema <- bq_perform_query_schema(
    sql,
    billing = src$con@billing,
    default_dataset = dataset,
    labels = src$con@labels
  )
  vars <- map_chr(schema, "[[", "name")

  if (utils::packageVersion("dbplyr") >= "2.4.0.9000") {
    tbl <- dplyr::tbl(src, from = from, vars = vars)
  } else {
    tbl <- dplyr::tbl(src, from = from, vars = vars, check_from = FALSE)
  }

  # This is ugly, but I don't see a better way of doing this
  if (!inherits(from, "sql")) {
    tb <- as_bq_table(src$con, from)
    tbl$lazy_query$is_view <- tryCatch(
      bq_table_meta(tb, "type")$type == "VIEW",
      bigrquery_notFound = function(e) TRUE
    )
  } else {
    tbl$lazy_query$is_view <- FALSE
  }
  tbl
}

# registered onLoad
dbplyr_edition.BigQueryConnection <- function(con) 2L

# registered onLoad
#
# `column_map` names the columns the query produces, using the same
# `target = source` shape as `dplyr::rename()`:
# * a named character vector, e.g. `c(region = "region_x")`, or
# * a two column data frame with columns `source` and `target`.
#
# It resolves columns whose names differ between the query and the destination
# table, and disambiguates same-named columns coming from different sources.
# Both the mapping and the query's projected columns are checked against the
# destination's partition and clustering fields before the job is submitted.
db_compute.BigQueryConnection <- function(
  con,
  table,
  sql,
  ...,
  overwrite = FALSE,
  temporary = TRUE,
  unique_indexes = list(),
  indexes = list(),
  analyze = TRUE,
  in_transaction = FALSE,
  column_map = NULL
) {
  call <- caller_env()
  destination_table <- if (!temporary) as_bq_table(con, table)
  write_disposition <- if (!temporary && overwrite) {
    "WRITE_TRUNCATE"
  } else {
    "WRITE_EMPTY"
  }

  # Validate the projection against the destination *before* submitting the
  # job, so that an existing destination table is never replaced when the query
  # can't be written to it.
  partition_field <- NULL
  clustering_fields <- character()
  if (!temporary && bq_table_exists(destination_table)) {
    meta <- bq_table_meta(
      destination_table,
      fields = "timePartitioning,clustering"
    )
    partition_field <- meta$timePartitioning$field
    clustering_fields <- unlist(meta$clustering$fields, use.names = FALSE)
  }
  needs_validation <- (is_string(partition_field) && nzchar(partition_field)) ||
    length(clustering_fields) > 0

  if (needs_validation || !is.null(column_map)) {
    projection <- bq_compute_projection(con, sql, call = call)
    projection <- bq_compute_column_map(projection, column_map, call = call)

    if (needs_validation) {
      bq_compute_check_destination(
        destination_table,
        partition_field,
        clustering_fields,
        projection,
        call = call
      )
    }

    sql <- bq_compute_alias_sql(con, sql, projection)
  }

  if (is.null(con@dataset)) {
    tb <- bq_project_query(
      con@project,
      sql,
      destination_table = destination_table,
      write_disposition = write_disposition,
      labels = con@labels
    )
  } else {
    ds <- bq_dataset(con@project, con@dataset)
    tb <- bq_dataset_query(
      ds,
      query = sql,
      destination_table = destination_table,
      write_disposition = write_disposition,
      labels = con@labels
    )
  }

  dbplyr::as_table_path(
    I(paste(tb$project, tb$dataset, tb$table, sep = ".")),
    con
  )
}

# The schema of the query result, as it would be written to the destination
# table. A dry run reports the projected column names and types without
# reading any data. Billing matches the job that will actually be submitted.
bq_compute_projection <- function(con, sql, call = caller_env()) {
  default_dataset <- if (is.null(con@dataset)) NULL else as_bq_dataset(con)
  fields <- tryCatch(
    bq_perform_query_schema(
      sql,
      billing = con@project,
      default_dataset = default_dataset,
      labels = con@labels
    ),
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      if (grepl("duplicate column", msg, ignore.case = TRUE)) {
        cli::cli_abort(
          c(
            "Can't materialise the query: it produces duplicate column names.",
            i = "Rename the conflicting columns in the query so each output
                 column has a distinct name."
          ),
          class = "bigrquery_compute_column_conflict",
          call = call
        )
      }
      stop(cnd)
    }
  )

  if (length(fields) == 0) {
    cli::cli_abort(
      "Can't determine the columns produced by the query.",
      call = call
    )
  }

  data.frame(
    source = vapply(fields, function(x) x$name, character(1)),
    type = vapply(fields, function(x) x$type %||% "STRING", character(1)),
    repeated = vapply(
      fields,
      function(x) identical(x$mode %||% "NULLABLE", "REPEATED"),
      logical(1)
    ),
    stringsAsFactors = FALSE
  )
}

# Resolve the caller's `target = source` mapping against the projection, adding
# a `target` column that holds the name each projected column will be written
# under. Detects conflicting names coming from different sources.
bq_compute_column_map <- function(
  projection,
  column_map,
  call = caller_env()
) {
  duplicated_source <- unique(projection$source[
    duplicated(projection$source)
  ])
  if (length(duplicated_source) > 0) {
    cli::cli_abort(
      c(
        "Can't materialise the query: it produces the column name{?s}
         {.field {duplicated_source}} more than once.",
        i = "Rename the columns in the query so each output column has a
             distinct name."
      ),
      class = "bigrquery_compute_column_conflict",
      call = call
    )
  }

  projection$target <- projection$source
  if (is.null(column_map)) {
    return(projection)
  }

  column_map <- as_bq_compute_column_map(column_map, projection$source, call)
  matched <- match(projection$source, unname(column_map))
  projection$target[!is.na(matched)] <- names(column_map)[matched[
    !is.na(matched)
  ]]

  duplicated_target <- unique(projection$target[
    duplicated(projection$target)
  ])
  if (length(duplicated_target) > 0) {
    sources <- projection$source[projection$target %in% duplicated_target]
    cli::cli_abort(
      c(
        "Can't materialise the query: multiple columns map to the target
         column{?s} {.field {duplicated_target}}.",
        x = "Conflicting source column{?s}: {.field {sources}}.",
        i = "Map each source column to a different target column."
      ),
      class = "bigrquery_compute_column_conflict",
      call = call
    )
  }

  projection
}

# Coerce `column_map` to a named vector of `target = source`. A two column data
# frame names its columns `source` and `target`.
as_bq_compute_column_map <- function(column_map, source, call) {
  if (is.data.frame(column_map)) {
    if (ncol(column_map) != 2) {
      cli::cli_abort(
        "When a data frame, {.arg column_map} must have exactly two columns.",
        call = call
      )
    }
    names(column_map) <- tolower(names(column_map))
    if (!all(c("source", "target") %in% names(column_map))) {
      cli::cli_abort(
        c(
          "When a data frame, {.arg column_map} must have {.field source} and
           {.field target} columns.",
          i = "Got columns {.field {names(column_map)}}."
        ),
        call = call
      )
    }
    column_map <- setNames(
      as.character(column_map$source),
      as.character(column_map$target)
    )
  }

  check_character(column_map, allow_na = FALSE, call = call)
  if (is.null(names(column_map))) {
    cli::cli_abort(
      c(
        "{.arg column_map} must be named.",
        i = "Use a vector like {.code c(target_column = \"query_column\")}."
      ),
      call = call
    )
  }

  target <- names(column_map)
  if (any(!nzchar(target)) || any(!nzchar(unname(column_map)))) {
    cli::cli_abort(
      "{.arg column_map} must not contain empty names or values.",
      call = call
    )
  }
  if (anyDuplicated(unname(column_map))) {
    cli::cli_abort(
      c(
        "Can't materialise the query: {.arg column_map} maps the same query
         column to more than one target.",
        i = "Each query column can only map to one target column."
      ),
      class = "bigrquery_compute_column_conflict",
      call = call
    )
  }

  unknown <- setdiff(unname(column_map), source)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{.arg column_map} maps unknown column{?s} {.field {unknown}}.",
        i = "The query produces the columns {.field {source}}."
      ),
      class = "bigrquery_compute_column_conflict",
      call = call
    )
  }

  column_map
}

# Validate the destination's time partition and clustering fields against the
# columns the query will actually write.
#
# Type rules follow BigQuery:
# * time partitioning requires a top-level DATE, DATETIME, or TIMESTAMP column;
# * clustering requires a top-level, non-repeated, simple-type column, where
#   the supported types are STRING, GEOGRAPHY, INT64, NUMERIC, BIGNUMERIC,
#   DATE, DATETIME, TIMESTAMP, BOOL, and RANGE.
# See https://cloud.google.com/bigquery/docs/reference/rest/v2/tables
bq_compute_check_destination <- function(
  table,
  partition_field,
  clustering_fields,
  projection,
  call = caller_env()
) {
  if (length(partition_field) == 1 && nzchar(partition_field)) {
    bq_compute_check_field(
      table,
      partition_field,
      projection,
      role = "partition",
      types = c("DATE", "DATETIME", "TIMESTAMP"),
      call = call
    )
  }

  for (field in clustering_fields) {
    bq_compute_check_field(
      table,
      field,
      projection,
      role = "clustering",
      types = c(
        "STRING",
        "GEOGRAPHY",
        "INT64",
        "NUMERIC",
        "BIGNUMERIC",
        "DATE",
        "DATETIME",
        "TIMESTAMP",
        "BOOL",
        "RANGE"
      ),
      call = call
    )
  }

  invisible()
}

bq_compute_check_field <- function(
  table,
  field,
  projection,
  role,
  types,
  call = caller_env()
) {
  table_name <- toString(table)

  i <- match(field, projection$target)
  if (is.na(i)) {
    cli::cli_abort(
      c(
        "Can't write to {.val {table_name}}: {role} column {.field {field}} is
         not produced by the query.",
        i = "The query produces the columns {.field {projection$target}}.",
        i = "Map a query column to {.field {field}}, e.g.
             {.code column_map = c({field} = \"column\")}."
      ),
      class = "bigrquery_compute_column_conflict",
      call = call
    )
  }

  if (projection$repeated[[i]]) {
    cli::cli_abort(
      c(
        "Can't write to {.val {table_name}}: {role} column {.field {field}} is
         a repeated field.",
        i = "{role} columns must be a single value, not an array."
      ),
      class = "bigrquery_compute_invalid_type",
      call = call
    )
  }

  type <- bq_compute_normalize_type(projection$type[[i]])
  if (!type %in% types) {
    cli::cli_abort(
      c(
        "Can't write to {.val {table_name}}: {role} column {.field {field}}
         has type {.val {type}}, which is not valid for {role}.",
        i = "Valid types are {.and {.field {types}}}."
      ),
      class = "bigrquery_compute_invalid_type",
      call = call
    )
  }

  invisible()
}

bq_compute_normalize_type <- function(type) {
  type <- toupper(type)
  switch(
    type,
    INTEGER = "INT64",
    FLOAT = "FLOAT64",
    BOOLEAN = "BOOL",
    RECORD = "STRUCT",
    type
  )
}

# Apply any renames in the mapping by aliasing the query's output columns. The
# query is wrapped in a subquery so that the aliases refer to the exact result
# that would otherwise have been written.
bq_compute_alias_sql <- function(con, sql, projection) {
  if (identical(projection$source, projection$target)) {
    return(sql)
  }

  # A trailing semicolon would make the wrapped query invalid.
  inner <- trimws(as.character(sql))
  inner <- sub(";+$", "", inner)

  quote <- \(x) as.character(DBI::dbQuoteIdentifier(con, x))
  columns <- ifelse(
    projection$source == projection$target,
    quote(projection$source),
    paste0(quote(projection$source), " AS ", quote(projection$target))
  )

  dbplyr::sql(paste0(
    "SELECT ",
    paste(columns, collapse = ", "),
    "\nFROM (\n",
    inner,
    "\n)"
  ))
}

# registered onLoad
db_copy_to.BigQueryConnection <- function(
  con,
  table,
  values,
  ...,
  overwrite = FALSE,
  types = NULL,
  temporary = TRUE,
  unique_indexes = NULL,
  indexes = NULL,
  analyze = TRUE,
  in_transaction = TRUE
) {
  if (temporary) {
    cli::cli_abort("BigQuery does not support temporary tables")
  }

  tb <- as_bq_table(con, table)
  write <- if (overwrite) "WRITE_TRUNCATE" else "WRITE_EMPTY"
  bq_table_upload(
    tb,
    values,
    fields = types,
    write_disposition = write,
    labels = con@labels
  )

  table
}

# Efficient downloads -----------------------------------------------

# registered onLoad

#' Collect a BigQuery table
#'
#' This collect method is specialised for BigQuery tables, generating the
#' SQL from your dplyr commands, then calling [bq_project_query()]
#' or [bq_dataset_query()] to run the query, then [bq_table_download()]
#' to download the results. Thus the arguments are a combination of the
#' arguments to [dplyr::collect()], `bq_project_query()`/`bq_dataset_query()`,
#' and `bq_table_download()`.
#'
#' @inheritParams dplyr::collect
#' @inheritParams bq_table_download
#' @param n Maximum number of results to retrieve.
#'   The default, `Inf`, will retrieve all rows.
#' @param ... Other arguments passed on to
#'   `bq_project_query()`/`bq_project_query()`
#' @method collect tbl_BigQueryConnection
collect.tbl_BigQueryConnection <- function(
  x,
  ...,
  n = Inf,
  api = c("json", "arrow"),
  page_size = NULL,
  max_connections = 6L
) {
  api <- check_api(api)
  check_number_whole(n, min = 0, allow_infinite = TRUE)
  check_number_whole(max_connections, min = 1)

  con <- dbplyr::remote_con(x)
  billing <- con@billing

  if (op_can_download(x)) {
    lq <- x$lazy_query
    name <- op_table(x, con)
    tb <- as_bq_table(con, name)
    n <- min(op_rows(x$lazy_query), n)
  } else {
    sql <- dbplyr::db_sql_render(con, x)

    if (is.null(con@dataset)) {
      tb <- bq_project_query(
        billing,
        sql,
        quiet = con@quiet,
        labels = con@labels,
        ...
      )
    } else {
      ds <- as_bq_dataset(con)
      tb <- bq_dataset_query(
        ds,
        sql,
        quiet = con@quiet,
        billing = billing,
        labels = con@labels,
        ...
      )
    }
  }

  quiet <- if (n < 100) TRUE else con@quiet
  bigint <- con@bigint %||% "integer"

  if (api == "arrow") {
    out <- bq_table_download(
      tb,
      n_max = n,
      quiet = quiet,
      bigint = bigint,
      billing = billing,
      api = "arrow"
    )
  } else {
    out <- bq_table_download(
      tb,
      n_max = n,
      page_size = page_size,
      quiet = quiet,
      max_connections = max_connections,
      bigint = bigint,
      api = "json"
    )
  }

  dplyr::grouped_df(out, intersect(dbplyr::op_grps(x), names(out)))
}

# Can download directly if only head and

op_can_download <- function(x) UseMethod("op_can_download")
#' @export
op_can_download.tbl_lazy <- function(x) op_can_download(x$lazy_query)
#' @export
op_can_download.lazy_query <- function(x) FALSE
#' @export
op_can_download.lazy_select_query <- function(x) {
  query_is_head_only(x)
}
#' @export
op_can_download.lazy_base_query <- function(x) {
  if (isTRUE(x$is_view)) {
    FALSE
  } else if (inherits(x$x, "sql")) {
    FALSE
  } else {
    dbplyr::is.ident(x$x) ||
      inherits(x$x, "dbplyr_table_ident") ||
      inherits(x$x, "dbplyr_table_path")
  }
}

query_is_head_only <- function(x) {
  if (!inherits(x$x, "lazy_base_remote_query")) {
    return(FALSE)
  }
  if (!op_can_download(x$x)) {
    return(FALSE)
  }

  vars_base <- dbplyr::op_vars(x$x)
  if (!is_select_trivial(x$select, vars_base)) {
    return(FALSE)
  }

  if (!is_empty(x$where)) {
    return(FALSE)
  }
  if (!is_empty(x$order_by)) {
    return(FALSE)
  }
  if (!is_false(x$distinct)) {
    return(FALSE)
  }

  TRUE
}

is_select_trivial <- function(select, vars_prev) {
  identical(select$name, vars_prev) &&
    all(vapply(select$expr, is_symbol, logical(1))) &&
    identical(syms(select$name), select$expr)
}


op_rows <- function(x) UseMethod("op_rows")
#' @export
op_rows.tbl_lazy <- function(x) op_rows(x$lazy_query)
#' @export
op_rows.lazy_base_query <- function(x) Inf
#' @export
op_rows.lazy_select_query <- function(x) {
  min(x$limit, op_rows(x$x))
}

op_table <- function(x, con) UseMethod("op_table")
#' @export
op_table.tbl_lazy <- function(x, con) op_table(x$lazy_query, con)
#' @export
op_table.lazy_query <- function(x, con) NULL
#' @export
op_table.lazy_base_remote_query <- function(x, con) x$x
#' @export
op_table.lazy_select_query <- function(x, con) {
  if (!query_is_head_only(x)) {
    return(NULL)
  }

  op_table(x$x)
}


# exported onLoad
same_src.tbl_BigQueryConnection <- function(x, y) {
  inherits(y, "tbl_BigQueryConnection")
}

# SQL translation -------------------------------------------------------------

# Don't import to avoid build-time dependency
sql_prefix <- function(f, n = NULL) {
  dbplyr::sql_prefix(f = f, n = n)
}

# registered onLoad
sql_join_suffix.BigQueryConnection <- function(con, ...) {
  c("_x", "_y")
}

# registered onLoad
sql_translation.BigQueryConnection <- function(x) {
  dbplyr::sql_variant(
    dbplyr::sql_translator(
      .parent = dbplyr::base_scalar,

      `^` = sql_prefix("POW"),
      `%%` = sql_prefix("MOD"),
      "%||%" = sql_prefix("IFNULL"),

      # Coercion
      as.integer = function(x) dbplyr::build_sql("SAFE_CAST(", x, " AS INT64)"),
      as.logical = function(x) {
        dbplyr::build_sql("SAFE_CAST(", x, " AS BOOLEAN)")
      },
      as.numeric = function(x) {
        dbplyr::build_sql("SAFE_CAST(", x, " AS FLOAT64)")
      },
      as.character = function(x) {
        dbplyr::build_sql("SAFE_CAST(", x, " AS STRING)")
      },

      # Date/time
      Sys.date = sql_prefix("current_date"),
      Sys.time = sql_prefix("current_time"),

      # Regular expressions
      grepl = function(pattern, x) {
        # https://cloud.google.com/bigquery/docs/reference/standard-sql/string_functions#regexp_contains
        dbplyr::build_sql("REGEXP_CONTAINS", list(x, pattern))
      },
      gsub = function(pattern, replace, x) {
        # https://cloud.google.com/bigquery/docs/reference/standard-sql/string_functions#regexp_replace
        dbplyr::build_sql("REGEXP_REPLACE", list(x, pattern, replace))
      },

      # Other scalar functions
      ifelse = sql_prefix("IF"),

      # string
      paste0 = sql_prefix("CONCAT"),

      # stringr equivalents
      str_detect = sql_prefix("REGEXP_CONTAINS", 2),
      str_extract = sql_prefix("REGEXP_EXTRACT", 2),
      str_replace = sql_prefix("REGEXP_REPLACE", 3),

      # Parallel min and max
      pmax = sql_prefix("GREATEST"),
      pmin = sql_prefix("LEAST"),

      runif = function(n = n(), min = 0, max = 1) {
        RAND <- NULL # quiet R CMD check
        dbplyr::sql_runif(RAND(), n = {{ n }}, min = min, max = max)
      },
    ),
    dbplyr::sql_translator(
      .parent = dbplyr::base_agg,

      n = function() dplyr::sql("count(*)"),

      all = sql_prefix("LOGICAL_AND", 1),
      any = sql_prefix("LOGICAL_OR", 1),

      sd = sql_prefix("STDDEV_SAMP"),
      var = sql_prefix("VAR_SAMP"),
      cor = dbplyr::sql_aggregate_2("CORR"),
      cov = dbplyr::sql_aggregate_2("COVAR_SAMP"),

      # Median
      median = function(x, na.rm = TRUE) {
        dbplyr::build_sql("APPROX_QUANTILES(", x, ", 2)[SAFE_ORDINAL(2)]")
      }
    ),
    dbplyr::sql_translator(
      .parent = dbplyr::base_win,

      all = dbplyr::win_absent("LOGICAL_AND"),
      any = dbplyr::win_absent("LOGICAL_OR"),

      sd = dbplyr::win_recycled("STDDEV_SAMP"),
      var = dbplyr::win_recycled("VAR_SAMP"),
      cor = dbplyr::win_absent("CORR"),
      cov = dbplyr::win_absent("COVAR_SAMP"),

      n_distinct = dbplyr::win_absent("n_distinct"),

      median = dbplyr::win_absent("median")
    )
  )
}

simulate_bigrquery <- function(use_legacy_sql = FALSE) {
  new(
    "BigQueryConnection",
    project = "test",
    dataset = "test",
    billing = "test",
    use_legacy_sql = use_legacy_sql,
    page_size = 0L,
    quiet = TRUE
  )
}
