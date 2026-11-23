# errors when table is known to be incomplete

    Code
      bq_table_download(tb, n_max = 35000, page_size = 35000, bigint = "integer64",
        api = "json")
    Message
      Downloading first chunk of data.
    Condition
      Error in `bq_table_download()`:
      ! First chunk is incomplete:
      x 35,000 rows were requested, but only {n} rows were received.
      i Leave `page_size` unspecified or use an even smaller value.

# warns if supplying unnused arguments

    Code
      . <- bq_table_download(tb, api = "arrow", page_size = 1, start_index = 1,
        max_connections = 1, quiet = TRUE)
    Condition
      Warning in `bq_table_download()`:
      `page_size` is ignored when `api == "arrow"`
      Warning in `bq_table_download()`:
      `start_index` is ignored when `api == "arrow"`
      Warning in `bq_table_download()`:
      `max_connections` is ignored when `api == "arrow"`

# check_columns() validates its input

    Code
      check_columns(character())
    Condition
      Error:
      ! `columns` must be NULL or a non-empty character vector.

---

    Code
      check_columns(c("a", NA))
    Condition
      Error:
      ! `columns` must not contain NA or empty strings.

---

    Code
      check_columns(c("a", "a"))
    Condition
      Error:
      ! `columns` must not contain duplicated names.

---

    Code
      check_columns(1:3)
    Condition
      Error:
      ! `columns` must be NULL or a non-empty character vector.

# bq_download_schema_filter() keeps only requested columns, in order

    Code
      bq_download_schema_filter(path, "missing")
    Condition
      Error in `bq_download_schema_filter()`:
      ! Column not found in table schema: "missing".
      i Available columns: "name" and "id".

