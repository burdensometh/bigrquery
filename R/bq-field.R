#' BigQuery field (and fields) class
#'
#' `bq_field()` and `bq_fields()` create; `as_bq_field()` and `as_bq_fields()`
#' coerce from lists.
#'
#' @param name The field name. The name must contain only letters (a-z, A-Z),
#'   numbers (0-9), or underscores (_), and must start with a letter or
#'   underscore. The maximum length is 300 characters.
#' @param type The field data type. Possible values include:
#'   `"STRING"`, `"BYTES"`, `"INTEGER"`, `"FLOAT"`, `"BOOLEAN"`, `"TIMESTAMP"`,
#'   `"DATE"`, `"TIME"`, `"DATETIME"`, `"GEOGRAPHY"`, `"NUMERIC"`,
#'   `"BIGNUMERIC"`, `"JSON"`, `"RECORD"`.
#' @param mode The field mode. Possible values include: `"NULLABLE"`,
#'   `"REQUIRED"`, and `"REPEATED"`.
#' @param fields For a field of type "record", a list of sub-fields.
#' @param description The field description. The maximum length is 1,024
#'   characters.
#' @param x A list of `bg_fields`
#' @seealso `bq_field()` corresponds to a `TableFieldSchema`, see
#' <https://docs.cloud.google.com/bigquery/docs/reference/rest/v2/tables#TableFieldSchema>
#' for more details.
#' @export
#' @examples
#' bq_field("name", "string")
#'
#' as_bq_fields(list(
#'   list(name = "name", type = "string"),
#'   bq_field("age", "integer")
#' ))
#'
#' # as_bq_fields() can also take a data frame
#' as_bq_fields(mtcars)
bq_field <- function(
  name,
  type,
  mode = "NULLABLE",
  fields = list(),
  description = NULL
) {
  check_string(name)
  check_string(type)
  check_string(mode)
  check_string(description, allow_null = TRUE)

  structure(
    list(
      name = name,
      type = toupper(type),
      mode = toupper(mode),
      fields = as_bq_fields(fields),
      description = description %||% ""
    ),
    class = "bq_field"
  )
}

#' @export
as_json.bq_field <- function(x) {
  list(
    name = unbox(x$name),
    type = unbox(x$type),
    mode = unbox(x$mode),
    fields = as_json(x$fields),
    description = unbox(x$description)
  )
}

#' @export
#' @rdname bq_field
bq_fields <- function(x) {
  structure(x, class = "bq_fields")
}

#' @export
as_json.bq_fields <- function(x) {
  lapply(x, as_json)
}

#' @export
#' @rdname bq_field
as_bq_field <- function(x) UseMethod("as_bq_field")

#' @export
as_bq_field.bq_field <- function(x) x

#' @export
as_bq_field.list <- function(x) {
  bq_field(
    name = x$name,
    type = x$type,
    mode = x$mode %||% "NULLABLE",
    fields = lapply(x$fields, as_bq_field),
    description = x$description %||% ""
  )
}

#' @export
#' @rdname bq_field
as_bq_fields <- function(x) UseMethod("as_bq_fields")

#' @export
as_bq_fields.NULL <- function(x) x

#' @export
as_bq_fields.bq_fields <- function(x) x

#' @export
as_bq_fields.data.frame <- function(x) {
  types <- vapply(x, data_type, character(1))
  fields <- Map(function(type, name) bq_field(name, type), types, names(x))
  bq_fields(unname(fields))
}

#' @export
as_bq_fields.list <- function(x) {
  bq_fields(lapply(x, as_bq_field))
}

#' @export
format.bq_fields <- function(x, ...) {
  if (length(x) == 0) {
    return("")
  }

  fields <- lapply(x, format)
  gsub("\\n\\s+$", "\n", indent(paste0(fields, collapse = "")))
}

#' @export
print.bq_fields <- function(x, ...) {
  cat_line("<bq_fields>\n", format(x, ...))
  invisible(x)
}

#' @export
format.bq_field <- function(x, ...) {
  type <- x$type
  if (x$mode != "NULLABLE") {
    type <- paste0(type, ": ", tolower(x$mode))
  }

  paste0(x$name, " <", type, ">", "\n", format(x$fields))
}

#' @export
print.bq_field <- function(x, ...) {
  cat_line("<bq_field> ", format(x, ...))
  invisible(x)
}

data_type <- function(x) {
  if (is.factor(x)) {
    return("STRING")
  }
  if (inherits(x, "POSIXt")) {
    return("TIMESTAMP")
  }
  if (inherits(x, "hms")) {
    return("TIME")
  }
  if (inherits(x, "wk_wkt")) {
    return("GEOGRAPHY")
  }
  if (inherits(x, "blob")) {
    return("BYTES")
  }
  if (inherits(x, "Date")) {
    return("DATE")
  }

  switch(
    typeof(x),
    character = "STRING",
    logical = "BOOLEAN",
    double = "FLOAT",
    integer = "INTEGER",
    cli::cli_abort("Unsupported type {.str {typeof(x)}}.")
  )
}

# Schema diffs --------------------------------------------------------------

#' Compare two BigQuery schemas
#'
#' @description
#' `bq_schema_diff()` compares a `target` schema (typically the [bq_fields]
#' of an existing table) with an `incoming` schema (typically a data frame
#' that you want to append) and reports how they differ. The report
#' distinguishes legitimate additions (new `NULLABLE` or `REPEATED` fields)
#' from same-name type changes, which BigQuery does not allow, and from mode
#' relaxations (`REQUIRED` to `NULLABLE`), which are only allowed for fields
#' explicitly listed in `relax_fields`.
#'
#' [bq_table_append()] uses this comparison before submitting a job: if the
#' report contains problems, it errors without touching the target table;
#' otherwise it requests exactly the schema update options that the
#' legitimate changes require.
#'
#' @param target,incoming A [bq_fields] specification, or something coercible
#'   to it (like a data frame).
#' @param relax_fields A character vector naming fields that are allowed to
#'   relax from `REQUIRED` to `NULLABLE`. Use dotted paths for fields nested
#'   inside records, e.g. `"address.zip"`.
#' @return An object of class `bq_schema_diff`: a list with one component per
#'   category of difference, each named by the (possibly dotted) field path:
#'
#'   * `added`: new `NULLABLE` or `REPEATED` fields (a list of [bq_field]s).
#'     These are legitimate additions.
#'   * `relaxed`: fields allowed to relax from `REQUIRED` to `NULLABLE`
#'     because they appear in `relax_fields`.
#'   * `added_required`: new `REQUIRED` fields. BigQuery can't add required
#'     fields to an existing table, so these are problems.
#'   * `type_changed`: fields that exist on both sides but with different
#'     types.
#'   * `relax_blocked`: fields that relax from `REQUIRED` to `NULLABLE`
#'     without being listed in `relax_fields`.
#'   * `mode_changed`: fields with any other incompatible mode change.
#'   * `missing_required`: `REQUIRED` target fields absent from `incoming`.
#'
#'   `relaxed`, `type_changed`, `relax_blocked`, and `mode_changed` entries
#'   are lists with `from` and `to` components.
#' @export
#' @examples
#' target <- as_bq_fields(data.frame(id = 1L, name = "a"))
#'
#' # A new nullable field is a legitimate addition
#' bq_schema_diff(target, as_bq_fields(data.frame(id = 1L, name = "a", extra = 1.5)))
#'
#' # A same-name type change is reported separately from additions
#' bq_schema_diff(target, as_bq_fields(data.frame(id = 1.5, name = "a")))
#'
#' # Relaxing REQUIRED to NULLABLE requires an explicit opt-in
#' target <- bq_fields(list(bq_field("id", "integer", mode = "REQUIRED")))
#' bq_schema_diff(target, as_bq_fields(data.frame(id = 1L)))
#' bq_schema_diff(target, as_bq_fields(data.frame(id = 1L)), relax_fields = "id")
bq_schema_diff <- function(target, incoming, relax_fields = NULL) {
  target <- as_bq_fields(target)
  incoming <- as_bq_fields(incoming)
  check_character(relax_fields, allow_null = TRUE)

  schema_diff_fields(target, incoming, relax_fields = relax_fields %||% character())
}

schema_diff_fields <- function(target, incoming, relax_fields, prefix = "") {
  diff <- new_schema_diff()

  target_names <- map_chr(target, `[[`, "name")
  incoming_names <- map_chr(incoming, `[[`, "name")

  # Fields only in the incoming data: legitimate additions, unless they're
  # REQUIRED (BigQuery can't add required fields to an existing table)
  for (field in incoming) {
    if (field$name %in% target_names) {
      next
    }
    path <- paste0(prefix, field$name)
    if (field$mode == "REQUIRED") {
      diff$added_required[[path]] <- field
    } else {
      diff$added[[path]] <- field
    }
  }

  # Required target fields that the incoming data doesn't supply
  is_required <- map_chr(target, `[[`, "mode") == "REQUIRED"
  is_missing <- !(target_names %in% incoming_names)
  missing <- target_names[is_required & is_missing]
  if (length(missing) > 0) {
    diff$missing_required <- paste0(prefix, missing)
  }

  # Fields present on both sides
  for (name in target_names[target_names %in% incoming_names]) {
    path <- paste0(prefix, name)
    target_field <- target[[match(name, target_names)]]
    incoming_field <- incoming[[match(name, incoming_names)]]

    if (target_field$type != incoming_field$type) {
      diff$type_changed[[path]] <- list(
        from = target_field$type,
        to = incoming_field$type
      )
      next
    }

    if (target_field$mode != incoming_field$mode) {
      change <- list(from = target_field$mode, to = incoming_field$mode)
      if (target_field$mode == "REQUIRED" && incoming_field$mode == "NULLABLE") {
        if (path %in% relax_fields) {
          diff$relaxed[[path]] <- change
        } else {
          diff$relax_blocked[[path]] <- change
        }
      } else if (
        target_field$mode == "NULLABLE" && incoming_field$mode == "REQUIRED"
      ) {
        # Incoming data is stricter than the target: appends cleanly
      } else {
        diff$mode_changed[[path]] <- change
      }
    }

    if (target_field$type == "RECORD") {
      nested <- schema_diff_fields(
        target_field$fields,
        incoming_field$fields,
        relax_fields,
        prefix = paste0(path, ".")
      )
      diff <- merge_schema_diff(diff, nested)
    }
  }

  diff
}

new_schema_diff <- function() {
  structure(
    list(
      added = list(),
      relaxed = list(),
      added_required = list(),
      type_changed = list(),
      relax_blocked = list(),
      mode_changed = list(),
      missing_required = character()
    ),
    class = "bq_schema_diff"
  )
}

merge_schema_diff <- function(x, y) {
  x$added <- c(x$added, y$added)
  x$relaxed <- c(x$relaxed, y$relaxed)
  x$added_required <- c(x$added_required, y$added_required)
  x$type_changed <- c(x$type_changed, y$type_changed)
  x$relax_blocked <- c(x$relax_blocked, y$relax_blocked)
  x$mode_changed <- c(x$mode_changed, y$mode_changed)
  x$missing_required <- c(x$missing_required, y$missing_required)
  x
}

# The BigQuery `schemaUpdateOptions` needed to commit the legitimate changes,
# erroring if the diff contains any problems
bq_schema_update_options <- function(diff, call = caller_env()) {
  problems <- schema_diff_problems(diff)
  if (length(problems) > 0) {
    bullets <- set_names(problems, "x")
    if (length(diff$added) > 0) {
      fields <- paste0("`", names(diff$added), "`", collapse = ", ")
      bullets <- c(
        bullets,
        "i" = paste0(
          "New field", if (length(diff$added) > 1) "s", " ",
          fields, " would be allowed on ",
          if (length(diff$added) > 1) "their own." else "its own."
        )
      )
    }
    cli::cli_abort(
      c("Incoming data can't be safely appended to the target table:", bullets),
      call = call
    )
  }

  options <- character()
  if (length(diff$added) > 0) {
    options <- c(options, "ALLOW_FIELD_ADDITION")
  }
  if (length(diff$relaxed) > 0) {
    options <- c(options, "ALLOW_FIELD_RELAXATION")
  }
  options
}

schema_diff_problems <- function(diff) {
  c(
    imap_chr(diff$added_required, function(path, field) {
      paste0(
        "New field `", path, "` is REQUIRED; ",
        "only NULLABLE or REPEATED fields can be added to an existing table."
      )
    }),
    imap_chr(diff$type_changed, function(path, change) {
      paste0(
        "Field `", path, "` changed type from ",
        change$from, " to ", change$to, "."
      )
    }),
    imap_chr(diff$relax_blocked, function(path, change) {
      paste0(
        "Field `", path, "` relaxes from REQUIRED to NULLABLE; ",
        "list it in `relax_fields` to allow."
      )
    }),
    imap_chr(diff$mode_changed, function(path, change) {
      paste0(
        "Field `", path, "` changed mode from ",
        change$from, " to ", change$to, "."
      )
    }),
    if (length(diff$missing_required) > 0) {
      paste0(
        "Required field `", diff$missing_required,
        "` is missing from the incoming data."
      )
    }
  )
}

imap_chr <- function(x, f) {
  vapply(seq_along(x), function(i) f(names(x)[i], x[[i]]), character(1))
}

# The schema the target table will have after committing `diff`: the target
# fields with relaxations applied, plus the legitimate additions appended at
# their level
bq_fields_merge <- function(target, diff) {
  merge_schema_level(as_bq_fields(target), diff, prefix = "")
}

merge_schema_level <- function(fields, diff, prefix) {
  fields <- lapply(fields, function(field) {
    path <- paste0(prefix, field$name)
    if (path %in% names(diff$relaxed)) {
      field$mode <- "NULLABLE"
    }
    if (field$type == "RECORD") {
      field$fields <- merge_schema_level(field$fields, diff, paste0(path, "."))
    }
    field
  })

  at_level <- names(diff$added)[field_parent(names(diff$added)) == prefix]
  bq_fields(c(fields, unname(diff$added[at_level])))
}

field_parent <- function(paths) {
  has_parent <- grepl(".", paths, fixed = TRUE)
  parents <- sub("[^.]+$", "", paths)
  parents[!has_parent] <- ""
  parents
}

#' @export
format.bq_schema_diff <- function(x, ...) {
  out <- character()

  if (length(x$added) > 0) {
    types <- map_chr(x$added, `[[`, "type")
    out <- c(
      out,
      "New fields (allowed):",
      paste0("  ", names(x$added), " <", types, ">")
    )
  }
  if (length(x$relaxed) > 0) {
    out <- c(
      out,
      "Relaxations (allowed via `relax_fields`):",
      paste0("  ", names(x$relaxed), ": REQUIRED -> NULLABLE")
    )
  }

  problems <- schema_diff_problems(x)
  if (length(problems) > 0) {
    out <- c(out, "Problems:", paste0("  ", problems))
  }

  if (length(out) == 0) {
    out <- "Incoming data matches the target schema."
  }

  compatible <- length(problems) == 0
  out <- c(out, paste0("Compatible: ", if (compatible) "yes" else "no"))
  if (compatible) {
    options <- bq_schema_update_options(x)
    if (length(options) > 0) {
      out <- c(out, paste0("Schema update options: ", paste(options, collapse = ", ")))
    }
  }

  out
}

#' @export
print.bq_schema_diff <- function(x, ...) {
  cat_line("<bq_schema_diff>")
  cat_line(format(x, ...))
  invisible(x)
}
