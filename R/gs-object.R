gs_object <- function(bucket, object) {
  structure(
    list(bucket = bucket, object = object),
    class = "gs_object"
  )
}

#' @export
as.character.gs_object <- function(x, ...) {
  format(x)
}

#' @export
format.gs_object <- function(x, ...) {
  sprintf("gs://%s/%s", x$bucket, x$object)
}

#' @export
print.gs_object <- function(x, ...) {
  cat_line("<gs_object> ", format(x))
  invisible(x)
}

gs_object_delete <- function(x, token = bq_token()) {
  url <- sprintf(
    "https://storage.googleapis.com/storage/v1/b/%s/o/%s",
    x$bucket,
    x$object
  )
  req <- httr::DELETE(url, token, httr::user_agent(bq_ua()))
  process_request(req)
}

gs_object_exists <- function(x, token = bq_token()) {
  url <- sprintf(
    "https://storage.googleapis.com/storage/v1/b/%s/o/%s",
    x$bucket,
    x$object
  )
  req <- httr::GET(url, token, httr::user_agent(bq_ua()))
  req$status_code != 404
}

gs_storage_url <- "https://storage.googleapis.com/storage/v1/"
gs_upload_url <- "https://storage.googleapis.com/upload/storage/v1/"

gs_encoded_object <- function(x) {
  utils::URLencode(x$object, reserved = TRUE)
}

gs_object_read <- function(x, token = bq_token()) {
  req <- httr::GET(
    paste0(gs_storage_url, "b/", x$bucket, "/o/", gs_encoded_object(x)),
    token,
    httr::user_agent(bq_ua()),
    query = list(alt = "media")
  )
  rawToChar(process_request(req, raw = TRUE))
}

gs_object_write <- function(
  x,
  content,
  if_generation_match = NULL,
  token = bq_token()
) {
  check_string(content)

  query <- list(uploadType = "media", name = x$object)
  if (!is.null(if_generation_match)) {
    query$ifGenerationMatch <- if_generation_match
  }

  req <- httr::POST(
    paste0(gs_upload_url, "b/", x$bucket, "/o"),
    token,
    httr::user_agent(bq_ua()),
    httr::add_headers("Content-Type" = "application/json"),
    body = charToRaw(content),
    query = query
  )
  process_request(req)
}

# Lists every object in `bucket` whose name starts with `prefix`, returning a
# tibble with one row per object, sorted by name.
gs_object_list <- function(
  bucket,
  prefix = NULL,
  page_size = 1000,
  token = bq_token()
) {
  check_string(bucket, allow_empty = FALSE)
  prefix <- prefix %||% ""

  query <- list(maxResults = page_size)
  if (nzchar(prefix)) {
    query$prefix <- prefix
  }

  url <- paste0(gs_storage_url, "b/", bucket, "/o")
  items <- list()
  repeat {
    req <- httr::GET(
      url,
      token,
      httr::user_agent(bq_ua()),
      query = query
    )
    page <- process_request(req)
    items <- c(items, page$items %||% list())

    token_next <- page$nextPageToken
    if (is.null(token_next)) {
      break
    }
    query$pageToken <- token_next
  }

  encode <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  out <- tibble::tibble(
    name = map_chr(items, function(x) encode(x$name)),
    uri = map_chr(items, function(x) encode(x$uri)),
    size = if (length(items) == 0) {
      numeric()
    } else {
      vapply(items, function(x) as.numeric(x$size %||% NA), numeric(1))
    },
    generation = map_chr(items, function(x) encode(x$generation))
  )

  out[order(out$name), , drop = FALSE]
}
