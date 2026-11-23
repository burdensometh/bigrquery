#' Explicitly define query parameters
#'
#' @description
#' By default, bigrquery will assume vectors of length 1 are scalars,
#' and longer vectors are arrays. If you need to pass a length-1 array,
#' you'll need to explicitly use `bq_param_array()`.
#'
#' `bq_param_struct()` creates a single `STRUCT` parameter from a named list or
#' a one-row data frame. `bq_param_array()` also accepts a field specification
#' as its `type`, in which case each element of `value` is a row and the
#' parameter becomes an `ARRAY<STRUCT<...>>`. This makes it easy to send a
#' changing roster of rows (e.g. one row per market and its threshold) as a
#' single parameter that a query can consume with `UNNEST(@name)`, so the same
#' query can be reused when the roster changes.
#'
#' @section STRUCT semantics:
#' The `type` of a STRUCT parameter describes its fields. It can be:
#'
#' * A [bq_fields] object, or a list of [bq_field()]s.
#' * A data frame, in which case field types are inferred from the columns.
#' * A named character vector or named list of BigQuery type strings, e.g.
#'   `c(market = "STRING", threshold = "FLOAT")`.
#'
#' The `value` of a STRUCT array is a data frame or a list of named lists (or
#' one-row data frames), where each element is one row. Field values are matched
#' to fields by name:
#'
#' * An empty roster (a data frame with zero rows, or a zero-length list) sends
#'   an empty array.
#' * `NA` field values are sent as SQL `NULL`.
#' * A declared field that's absent from a row is also sent as SQL `NULL`.
#' * A row field that isn't declared in `type` is an error.
#' * Duplicated field names, either in `type` or within a row, are an error.
#'
#' When `type` is `NULL` and `value` is a data frame, field types are inferred
#' from the column classes, using the same rules as [as_bq_fields()]. Supply
#' `type` explicitly when a column is empty or contains only `NA`, because the
#' inferred type may not be what BigQuery expects.
#'
#' @param value vector of parameter values. For STRUCT parameters, a named list
#'   (or one-row data frame) or, for arrays of STRUCT, a data frame or list of
#'   rows.
#' @param name name of the parameter in the query, omitting the `@`
#' @param type BigQuery type of the parameter. For scalar parameters this is a
#'   type string like `"STRING"`. For STRUCT parameters it's a field
#'   specification, as described in the STRUCT semantics section.
#' @keywords internal
#' @export
#' @examples
#' # bq_param() automatically picks scalar vs array based on length
#' bq_param("a")
#' bq_param(c("a", "b", "c"))
#'
#' # use bq_param_array() to create a length-1 array
#' bq_param_array("a")
#'
#' # send a roster of markets and per-market thresholds as one
#' # ARRAY<STRUCT<market STRING, threshold FLOAT>> parameter
#' thresholds <- data.frame(
#'   market = c("US", "GB"),
#'   threshold = c(10, 12.5)
#' )
#' bq_param_array(thresholds, type = c(market = "STRING", threshold = "FLOAT"))
#'
#' # a single STRUCT parameter
#' bq_param_struct(list(market = "US", threshold = NA), type = c(
#'   market = "STRING",
#'   threshold = "FLOAT"
#' ))
bq_param <- function(value, type = NULL, name = NULL) {
  if (is_struct_type(type)) {
    if (is.data.frame(value)) {
      bq_param_array(value, type, name)
    } else {
      bq_param_struct(value, type, name)
    }
  } else if (is.data.frame(value) || length(value) > 1) {
    bq_param_array(value, type, name)
  } else {
    bq_param_scalar(value, type, name)
  }
}

#' @rdname bq_param
#' @export
bq_param_scalar <- function(value, type = NULL, name = NULL) {
  call <- current_env()

  if (length(value) != 1) {
    cli::cli_abort(
      "{.arg value} must be length 1, not {length(value)}.",
      call = call
    )
  }
  if (is_struct_type(type)) {
    cli::cli_abort(
      "{.arg type} describes a STRUCT; use {.fn bq_param_struct} instead.",
      call = call
    )
  }

  if (is.null(type)) {
    type <- data_type(value)
  } else {
    type <- normalize_param_type(type, call = call)
    check_not_bare_struct(type, call = call)
  }
  structure(
    list(value = value, name = name, type = type),
    class = c("bq_param_scalar", "bq_param")
  )
}

#' @rdname bq_param
#' @export
bq_param_array <- function(value, type = NULL, name = NULL) {
  call <- current_env()

  if (is.data.frame(value) || is_struct_type(type)) {
    return(bq_param_array_struct(value, type, name, call = call))
  }
  if (length(value) == 0) {
    cli::cli_abort("{.arg value} can't be zero-length.", call = call)
  }

  if (is.null(type)) {
    type <- data_type(value)
  } else {
    type <- normalize_param_type(type, call = call)
    check_not_bare_struct(type, call = call)
  }
  structure(
    list(value = value, name = name, type = type),
    class = c("bq_param_array", "bq_param")
  )
}

#' @rdname bq_param
#' @export
bq_param_struct <- function(value, type = NULL, name = NULL) {
  call <- current_env()

  fields <- param_struct_fields(type, value, call = call)
  row <- as_struct_row(value, call = call)
  struct_values_json(row, fields, call = call)

  structure(
    list(value = row, name = name, type = fields),
    class = c("bq_param_struct", "bq_param")
  )
}

bq_param_array_struct <- function(value, type, name, call = caller_env()) {
  fields <- param_struct_fields(type, value, call = call)
  rows <- as_struct_rows(value, call = call)
  # Validate eagerly so problems are reported where the parameter is built,
  # not when the query is sent.
  lapply(rows, struct_values_json, fields = fields, call = call)

  structure(
    list(value = rows, name = name, type = fields),
    class = c("bq_param_array", "bq_param")
  )
}

# Types -------------------------------------------------------------------

is_struct_type <- function(type) {
  if (is.null(type)) {
    return(FALSE)
  }
  if (is.data.frame(type) || inherits(type, "bq_fields")) {
    return(TRUE)
  }
  if (is.list(type)) {
    return(TRUE)
  }
  if (is.character(type)) {
    return(length(type) > 1 || !is.null(names(type)))
  }
  FALSE
}

normalize_param_type <- function(type, call = caller_env()) {
  if (!is_string(type)) {
    cli::cli_abort(
      "{.arg type} must be a single BigQuery type string.",
      call = call
    )
  }
  type <- toupper(type)
  if (identical(type, "RECORD")) "STRUCT" else type
}

check_not_bare_struct <- function(type, call = caller_env()) {
  if (identical(type, "STRUCT")) {
    cli::cli_abort(
      c(
        "A bare {.val STRUCT} type doesn't describe its fields.",
        i = "Use {.fn bq_param_struct} or {.fn bq_param_array} with a field specification."
      ),
      call = call
    )
  }
  invisible()
}

param_struct_fields <- function(type, value, call = caller_env()) {
  if (is.null(type)) {
    if (!is.data.frame(value)) {
      cli::cli_abort(
        c(
          "Must supply {.arg type} when {.arg value} is not a data frame.",
          i = "Use a named vector of type strings, e.g. {.code c(market = \"STRING\")}."
        ),
        call = call
      )
    }
    fields <- as_bq_fields(value)
  } else {
    fields <- as_param_fields(type, call = call)
  }

  check_param_fields(fields, call = call)
  fields
}

as_param_fields <- function(type, call = caller_env()) {
  if (inherits(type, "bq_fields")) {
    return(type)
  }
  if (is.data.frame(type)) {
    return(as_bq_fields(type))
  }
  if (is.character(type)) {
    return(as_param_fields_map(type, call = call))
  }
  if (is.list(type)) {
    if (length(type) > 0 && all(vapply(type, is_string, logical(1)))) {
      return(as_param_fields_map(type, call = call))
    }
    return(as_bq_fields(type))
  }

  cli::cli_abort(
    c(
      "{.arg type} must be a type string or a STRUCT field specification.",
      i = "Use a {.cls bq_fields} object, a data frame, or a named vector of type strings."
    ),
    call = call
  )
}

as_param_fields_map <- function(type, call = caller_env()) {
  names <- names(type)
  if (is.null(names) || any(names == "")) {
    cli::cli_abort(
      "A field specification must be named, giving each field's type.",
      call = call
    )
  }

  fields <- Map(
    function(name, field_type) {
      bq_field(name, normalize_param_type(field_type, call = call))
    },
    names,
    unname(as.list(type))
  )
  bq_fields(unname(fields))
}

check_param_fields <- function(fields, call = caller_env()) {
  if (length(fields) == 0) {
    cli::cli_abort(
      "A STRUCT type must declare at least one field.",
      call = call
    )
  }

  names <- param_field_names(fields)
  if (any(names == "")) {
    cli::cli_abort("STRUCT field names must not be empty.", call = call)
  }
  dup <- unique(names[duplicated(names)])
  if (length(dup) > 0) {
    cli::cli_abort(
      "STRUCT field names must be unique; duplicated: {.val {dup}}.",
      call = call
    )
  }

  for (field in fields) {
    if (field$type %in% c("RECORD", "STRUCT")) {
      check_param_fields(as_bq_fields(field$fields), call = call)
    }
  }
  invisible(fields)
}

param_field_names <- function(fields) {
  vapply(fields, function(field) field$name, character(1))
}

param_type_json <- function(type) {
  if (inherits(type, "bq_fields")) {
    list(
      type = unbox("STRUCT"),
      structTypes = lapply(unname(type), param_field_type_json)
    )
  } else {
    list(type = unbox(type))
  }
}

param_field_type_json <- function(field) {
  if (field$type %in% c("RECORD", "STRUCT")) {
    inner <- param_type_json(as_bq_fields(field$fields))
  } else {
    inner <- list(type = unbox(normalize_param_type(field$type)))
  }

  if (identical(field$mode, "REPEATED")) {
    inner <- list(type = unbox("ARRAY"), arrayType = inner)
  }

  list(name = unbox(field$name), type = inner)
}

# Values ------------------------------------------------------------------

as_struct_rows <- function(value, call = caller_env()) {
  if (is.null(value)) {
    return(list())
  }
  if (is.data.frame(value)) {
    names <- names(value)
    if (any(names == "")) {
      cli::cli_abort("Data frame columns must be named.", call = call)
    }
    dup <- unique(names[duplicated(names)])
    if (length(dup) > 0) {
      cli::cli_abort(
        "Data frame column names must be unique; duplicated: {.val {dup}}.",
        call = call
      )
    }
    return(lapply(seq_len(nrow(value)), function(i) lapply(value, `[[`, i)))
  }

  if (is.list(value)) {
    return(value)
  }

  if (length(value) == 0) {
    # An empty atomic vector is an empty roster, e.g. character() from a filter.
    return(list())
  }

  cli::cli_abort(
    c(
      "The value of a STRUCT array must be a data frame or a list of rows.",
      i = "Each row must be a named list or a one-row data frame."
    ),
    call = call
  )
}

as_struct_row <- function(row, call = caller_env()) {
  if (is.data.frame(row)) {
    if (nrow(row) != 1) {
      cli::cli_abort(
        "A STRUCT row must have exactly one row, not {nrow(row)}.",
        call = call
      )
    }
    row <- as.list(row)
  }

  if (!is.list(row)) {
    cli::cli_abort(
      "Each STRUCT row must be a named list or a one-row data frame.",
      call = call
    )
  }

  names <- names(row)
  if (is.null(names)) {
    if (length(row) == 0) {
      # An empty row supplies none of the declared fields, so every field
      # will be sent as NULL.
      return(row)
    }
    cli::cli_abort("A STRUCT row must have named fields.", call = call)
  }
  if (any(names == "")) {
    cli::cli_abort("A STRUCT row must have named fields.", call = call)
  }
  dup <- unique(names[duplicated(names)])
  if (length(dup) > 0) {
    cli::cli_abort(
      "STRUCT row field names must be unique; duplicated: {.val {dup}}.",
      call = call
    )
  }

  row
}

struct_values_json <- function(row, fields, call = caller_env()) {
  row <- as_struct_row(row, call = call)
  declared <- param_field_names(fields)

  extra <- setdiff(names(row), declared)
  if (length(extra) > 0) {
    cli::cli_abort(
      c(
        "STRUCT row contains fields not declared in {.arg type}: {.val {extra}}.",
        i = "Declared fields are {.val {declared}}."
      ),
      call = call
    )
  }

  out <- vector("list", length(fields))
  names(out) <- declared
  for (i in seq_along(fields)) {
    out[[i]] <- field_value_json(row[[declared[[i]]]], fields[[i]], call = call)
  }
  out
}

field_value_json <- function(value, field, call = caller_env()) {
  if (identical(field$mode, "REPEATED")) {
    values <- repeated_elements(value, field, call = call)
    element <- field
    element$mode <- "NULLABLE"
    return(list(
      arrayValues = lapply(values, function(x) {
        field_value_json(x, element, call = call)
      })
    ))
  }

  if (field$type %in% c("RECORD", "STRUCT")) {
    # A NULL (or absent) nested struct is sent as SQL NULL, represented by an
    # empty QueryParameterValue object rather than a struct of NULL fields.
    if (is.null(value)) {
      return(empty_json_object())
    }
    fields <- as_bq_fields(field$fields)
    return(list(structValues = struct_values_json(value, fields, call = call)))
  }

  list(value = scalar_json(value, field$name, call = call))
}

repeated_elements <- function(value, field, call = caller_env()) {
  if (is.null(value)) {
    return(list())
  }
  if (is.data.frame(value)) {
    return(as_struct_rows(value, call = call))
  }
  if (is.list(value)) {
    return(value)
  }
  if (length(value) == 0) {
    return(list())
  }
  if (anyNA(value)) {
    cli::cli_abort(
      c(
        "Repeated field {.val {field$name}} must not contain missing values.",
        i = "Use {.code NULL} or a zero-length vector to send an empty array."
      ),
      call = call
    )
  }
  as.list(unname(value))
}

scalar_json <- function(value, name, call = caller_env()) {
  if (is.null(value)) {
    return(unbox(NA))
  }
  if (inherits(value, "blob")) {
    if (length(value) != 1) {
      cli::cli_abort(
        "Field {.val {name}} must be a single value, not length {length(value)}.",
        call = call
      )
    }
    return(unbox(jsonlite::base64_enc(value[[1]])))
  }
  if (is.raw(value)) {
    return(unbox(jsonlite::base64_enc(value)))
  }
  if (is.list(value)) {
    cli::cli_abort(
      "Field {.val {name}} must be a single value, not a list.",
      call = call
    )
  }
  if (length(value) != 1) {
    cli::cli_abort(
      "Field {.val {name}} must be a single value, not length {length(value)}.",
      call = call
    )
  }
  if (is.na(value)) {
    return(unbox(NA))
  }
  if (is.double(value) && !is.finite(value)) {
    cli::cli_abort(
      "Field {.val {name}} must be a finite number, not {.val {value}}.",
      call = call
    )
  }
  unbox(value)
}

# jsonlite serializes an empty unnamed list as `[]`, but an empty list with a
# names attribute as `{}`. The API expects `{}` for a NULL struct value.
empty_json_object <- function() {
  structure(list(), names = character())
}

# Coercion and JSON -------------------------------------------------------

as_bq_param <- function(x, name) {
  if (inherits(x, "bq_param")) {
    if (!is.null(name) & is.null(x$name)) {
      x$name <- name
    }
    x
  } else {
    bq_param(name = name, x)
  }
}

bq_params <- function(x) {
  structure(x, class = "bq_params")
}

as_bq_params <- function(x) {
  params <- lapply(names(x), function(name) {
    as_bq_param(x[[name]], name)
  })
  bq_params(params)
}

#' @export
as_json.bq_params <- function(x) {
  json <- lapply(x, as_json)
  unname(json)
}

#' @export
as_json.bq_param_scalar <- function(x) {
  list(
    name = x$name,
    parameterType = param_type_json(x$type),
    parameterValue = list(value = scalar_json(x$value, x$name))
  )
}

#' @export
as_json.bq_param_struct <- function(x) {
  list(
    name = x$name,
    parameterType = param_type_json(x$type),
    parameterValue = list(
      structValues = struct_values_json(x$value, x$type)
    )
  )
}

#' @export
as_json.bq_param_array <- function(x) {
  if (inherits(x$type, "bq_fields")) {
    values <- lapply(x$value, function(row) {
      list(structValues = struct_values_json(row, x$type))
    })
    return(list(
      name = x$name,
      parameterType = list(type = "ARRAY", arrayType = param_type_json(x$type)),
      parameterValue = list(arrayValues = unname(values))
    ))
  }

  values <- lapply(unname(as.list(x$value)), function(v) {
    list(value = scalar_json(v, x$name))
  })
  list(
    name = x$name,
    parameterType = list(
      type = "ARRAY",
      arrayType = list(type = unbox(x$type))
    ),
    parameterValue = list(arrayValues = values)
  )
}

#' @export
print.bq_param <- function(x, ...) {
  cat(show_json(as_json(x)))
  invisible(x)
}

#' @export
print.bq_params <- function(x, ...) {
  cat(show_json(as_json(x)))
  invisible(x)
}
