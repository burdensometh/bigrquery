test_that("same results regardless of page size", {
  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")

  df3 <- bq_table_download(tb, n_max = 30, page_size = 10, api = "json")
  df1 <- bq_table_download(tb, n_max = 30, page_size = 30, api = "json")
  expect_equal(nrow(df1), 30)
  expect_equal(df1, df3)
})

test_that("can retrieve fraction of page size", {
  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  df <- bq_table_download(tb, n_max = 15, page_size = 10, api = "json")
  expect_equal(nrow(df), 15)
})

test_that("can retrieve zero rows", {
  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  df <- bq_table_download(tb, n_max = 0, api = "json")
  expect_equal(nrow(df), 0)
  expect_named(df, c("phase", "phase_emoji", "peak_datetime"))
})

test_that("can specify large integers in page params", {
  skip_if_no_auth()

  # Use scipen to nudge R to prefer scientific formatting for very small numbers
  # to allow this test to exercise issue #395 with small datasets.
  withr::local_options(list(scipen = -4))

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  df <- bq_table_download(tb, n_max = 100, page_size = 20, api = "json")
  expect_equal(nrow(df), 100)
})

test_that("errors when table is known to be incomplete", {
  # If this snapshot changes, it's likely because the table has been updated

  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.chicago_taxi_trips.taxi_trips")
  expect_snapshot(
    bq_table_download(
      tb,
      n_max = 35000,
      page_size = 35000,
      bigint = "integer64",
      api = "json"
    ),
    transform = function(x) {
      gsub(
        "[0-9,]+ rows were received",
        "{n} rows were received",
        x,
        perl = TRUE
      )
    },
    error = TRUE
  )
})

# api = "arrow" ----------------------------------------------------------------

test_that("check_api respects inputs", {
  expect_equal(check_api("arrow"), "arrow")
  expect_equal(check_api("json"), "json")
})

test_that("uses arrow api if bigrquerystorage installed", {
  local_mocked_bindings(is_installed = function(...) TRUE)
  expect_equal(check_api(), "arrow")

  local_mocked_bindings(is_installed = function(...) FALSE)
  expect_equal(check_api(), "json")
})

test_that("warns if supplying unnused arguments", {
  tb <- bq_project_query(bq_test_project(), "SELECT 1.0", quiet = TRUE)
  expect_snapshot(
    . <- bq_table_download(
      tb,
      api = "arrow",
      page_size = 1,
      start_index = 1,
      max_connections = 1,
      quiet = TRUE
    )
  )
})

test_that("arrow api can convert non-nested types", {
  sql <- "SELECT
    '\U0001f603' as unicode,
    datetime,
    TRUE as logicaltrue,
    FALSE as logicalfalse,
    CAST ('Hi' as BYTES) as bytes,
    CAST (datetime as DATE) as date,
    CAST (datetime as TIME) as time,
    CAST (datetime as TIMESTAMP) as timestamp,
    ST_GEOGFROMTEXT('POINT (30 10)') as geography
    FROM (SELECT DATETIME '2000-01-02 03:04:05.67' as datetime)
  "

  tb <- bq_project_query(bq_test_project(), sql, quiet = TRUE)
  df <- bq_table_download(tb, api = "arrow", quiet = TRUE)

  base <- ISOdatetime(2000, 1, 2, 3, 4, 5.67, tz = "UTC")
  expect_identical(df$unicode, "\U0001f603", ignore_encoding = FALSE)

  expect_equal(df$logicaltrue, TRUE)
  expect_equal(df$logicalfalse, FALSE)

  expect_equal(df$bytes, blob::as_blob(as.raw(c(0x48, 0x69))))

  expect_equal(df$date, as.Date(base))
  expect_equal(df$timestamp, base)
  expect_equal(df$datetime, base)
  expect_equal(df$time, hms::hms(hours = 3, minutes = 4, seconds = 5.67))

  expect_identical(df$geography, wk::wkt("POINT(30 10)"))
})

test_that("arrow api can convert nested types", {
  sql <- "SELECT
    STRUCT(1.0 AS a, 'abc' AS b) as s,
    [1.0, 2.0, 3.0] as a,
    [STRUCT(1.0 as a, 'a' as b), STRUCT(2.0, 'b'), STRUCT(3, 'c')] as aos,
    STRUCT([1.0, 2.0, 3.0] as a, ['a', 'b'] as b) as soa
  "

  tb <- bq_project_query(bq_test_project(), sql, quiet = TRUE)
  df <- bq_table_download(tb, api = "arrow", quiet = TRUE)

  expect_equal(df$s, tibble(a = 1, b = "abc"))
  expect_equal(df$a, list(c(1, 2, 3)))
  expect_equal(df$aos, list(tibble(a = c(1, 2, 3), b = c("a", "b", "c"))))
  expect_equal(df$soa, tibble(a = list(c(1, 2, 3)), b = list(c("a", "b"))))
})

test_that("arrow api respects bigint", {
  x <- c(
    "-2147483648",
    "-2147483647",
    "-1",
    "0",
    "1",
    "2147483647",
    "2147483648"
  )
  sql <- paste0("SELECT * FROM UNNEST ([", paste0(x, collapse = ","), "]) AS x")
  qry <- bq_project_query(bq_test_project(), sql)

  out_int64 <- bq_table_download(
    qry,
    bigint = "integer64",
    api = "arrow",
    quiet = TRUE
  )$x
  expect_identical(out_int64, bit64::as.integer64(x))

  out_dbl <- bq_table_download(
    qry,
    bigint = "numeric",
    api = "arrow",
    quiet = TRUE
  )$x
  expect_identical(out_dbl, as.double(x))

  out_chr <- bq_table_download(
    qry,
    bigint = "character",
    api = "arrow",
    quiet = TRUE
  )$x
  expect_identical(out_chr, x)
})

# helpers around row and chunk params ------------------------------------------

test_that("set_row_params() works ", {
  # n_max > nrow
  expect_equal(
    set_row_params(10, n_max = 15),
    list(n_max = 10, start_index = 0)
  )
  # start_index > nrow
  expect_equal(
    set_row_params(10, start_index = 12),
    list(n_max = 0, start_index = 12)
  )
  expect_equal(
    set_row_params(10, n_max = 5, start_index = 12),
    list(n_max = 0, start_index = 12)
  )
  # n_max > nrow - start_index
  expect_equal(
    set_row_params(10, n_max = 5, start_index = 7),
    list(n_max = 3, start_index = 7)
  )
})

test_that("set_chunk_params() works", {
  # no chunk_size, no n_chunks
  expect_equal(set_chunk_params(5), list(chunk_size = 5, n_chunks = 1))

  # yes chunk_size, no n_chunks
  expect_equal(
    set_chunk_params(10, chunk_size = 3),
    list(chunk_size = 3, n_chunks = 4)
  )
  expect_equal(
    set_chunk_params(10, chunk_size = 10),
    list(chunk_size = 10, n_chunks = 1)
  )
  expect_equal(
    set_chunk_params(10, chunk_size = 9),
    list(chunk_size = 9, n_chunks = 2)
  )
  expect_equal(
    set_chunk_params(10, chunk_size = 15),
    list(chunk_size = 10, n_chunks = 1)
  )

  # no chunk_size, yes n_chunks
  expect_equal(
    set_chunk_params(10, n_chunks = 1),
    list(chunk_size = 10, n_chunks = 1)
  )
  expect_equal(
    set_chunk_params(10, n_chunks = 3),
    list(chunk_size = 4, n_chunks = 3)
  )
  expect_equal(
    set_chunk_params(10, n_chunks = 10),
    list(chunk_size = 1, n_chunks = 10)
  )
  expect_equal(
    set_chunk_params(10, n_chunks = 11),
    list(chunk_size = 1, n_chunks = 10)
  )

  # yes chunk_size, yes n_chunks
  expect_equal(
    set_chunk_params(10, chunk_size = 3, n_chunks = 1),
    list(chunk_size = 3, n_chunks = 1)
  )
  expect_equal(
    set_chunk_params(10, chunk_size = 7, n_chunks = 5),
    list(chunk_size = 7, n_chunks = 2)
  )
})

test_that("set_chunk_plan() works", {
  dat <- set_chunk_plan(5, chunk_size = 5, n_chunks = 1)
  expect_equal(dat$chunk_begin, 0)
  expect_equal(dat$chunk_rows, 5)

  dat <- set_chunk_plan(5, chunk_size = 5, n_chunks = 1, start_index = 3)
  expect_equal(dat$chunk_begin, 3)
  expect_equal(dat$chunk_rows, 5)

  dat <- set_chunk_plan(10, chunk_size = 3, n_chunks = 4)
  expect_equal(dat$chunk_begin, c(0, 3, 6, 9))
  expect_equal(dat$chunk_rows, c(3, 3, 3, 1))

  dat <- set_chunk_plan(10, chunk_size = 3, n_chunks = 4, start_index = 8)
  expect_equal(dat$chunk_begin, 8 + c(0, 3, 6, 9))
  expect_equal(dat$chunk_rows, c(3, 3, 3, 1))
})

# types -------------------------------------------------------------------

test_that("can read utf-8 strings", {
  sql <- "SELECT '\U0001f603' as x"
  tb <- bq_project_query(bq_test_project(), sql)
  df <- bq_table_download(tb)
  x <- df$x[[1]]

  expect_equal(Encoding(x), "UTF-8")
  expect_equal(x, "\U0001f603")
})

test_that("can convert date time types", {
  sql <- "SELECT
    datetime,
    CAST (datetime as DATE) as date,
    CAST (datetime as TIME) as time,
    CAST (datetime as TIMESTAMP) as timestamp
    FROM (SELECT DATETIME '2000-01-02 03:04:05.67' as datetime)
  "

  tb <- bq_project_query(bq_test_project(), sql, quiet = TRUE)
  df <- bq_table_download(tb, api = "json")

  base <- ISOdatetime(2000, 1, 2, 3, 4, 5.67, tz = "UTC")
  base_naive <- clock::naive_time_parse(
    "2000-01-02T03:04:05.670000",
    precision = "microsecond"
  )

  # DATETIME is timezone-less wall time; TIMESTAMP is a UTC instant.
  expect_s3_class(df$datetime, "clock_naive_time")
  expect_equal(df$datetime, base_naive)
  expect_equal(
    bq_datetime_format(df$datetime),
    "2000-01-02 03:04:05.670000"
  )
  expect_s3_class(df$timestamp, "POSIXct")
  expect_equal(df$timestamp, base)
  expect_equal(df$date, as.Date(base))
  expect_equal(df$time, hms::hms(hours = 3, minutes = 4, seconds = 5.67))
})


test_that("can parse fractional seconds", {
  x <- c(
    "2000-01-02T03:04:05.67",
    "2000-01-02T03:04:05",
    "2000-01-02T03:04:05.123456"
  )
  parsed <- bq_datetime_parse(x)
  expect_s3_class(parsed, "clock_naive_time")
  expect_false(inherits(parsed, "POSIXt"))
  expect_null(attr(parsed, "tzone"))

  # Microsecond precision is retained, including for shorter fraction strings.
  ymd <- clock::as_year_month_day(parsed)
  expect_equal(clock::get_microsecond(ymd), c(670000L, 0L, 123456L))
})

test_that("DATETIME keeps the local wall-clock value with no time zone", {
  # Both separators accepted: the REST API uses a space, ISO 8601 uses "T".
  x <- c("2000-03-09 02:30:00", "2000-03-09T02:30:00.5")
  parsed <- bq_datetime_parse(x)

  expect_equal(
    bq_datetime_format(parsed),
    c("2000-03-09 02:30:00.000000", "2000-03-09 02:30:00.500000")
  )
  # The value is independent of the session time zone.
  expect_equal(clock::as_date(parsed), rep(as.Date("2000-03-09"), 2))
  expect_equal(
    clock::get_hour(clock::as_year_month_day(parsed)),
    c(2L, 2L)
  )
})

test_that("DATETIME survives the date rollover at local midnight", {
  # These are naive business times, so grouping must follow the printed
  # calendar day rather than any UTC conversion.
  x <- c(
    "2025-03-09 23:59:59",
    "2025-03-10 00:00:00",
    "2025-03-10 00:30:00"
  )
  parsed <- bq_datetime_parse(x)
  days <- clock::as_date(parsed)

  expect_equal(as.character(days), c("2025-03-09", "2025-03-10", "2025-03-10"))
  expect_equal(as.integer(tapply(seq_along(x), days, length)), c(1L, 2L))
})

test_that("DATETIME keeps nonexistent DST-gap wall times verbatim", {
  # 02:30 did not exist in America/New_York on 2025-03-09 (spring forward),
  # but a timezone-less DATETIME must store it anyway.
  withr::local_timezone("America/New_York")

  parsed <- bq_datetime_parse(c(
    "2025-03-09 01:30:00",
    "2025-03-09 02:30:00",
    "2025-03-09 03:30:00"
  ))

  expect_equal(
    bq_datetime_format(parsed),
    c(
      "2025-03-09 01:30:00.000000",
      "2025-03-09 02:30:00.000000",
      "2025-03-09 03:30:00.000000"
    )
  )
  expect_equal(
    clock::as_date(parsed),
    rep(as.Date("2025-03-09"), 3)
  )
})

test_that("DATETIME and TIMESTAMP stay distinct in the same table", {
  schema <- jsonlite::toJSON(
    list(
      schema = list(fields = list(
        list(name = "business_dt", type = "DATETIME"),
        list(name = "event_ts", type = "TIMESTAMP")
      ))
    ),
    auto_unbox = TRUE
  )
  values <- paste0(
    '{"rows":[',
    '{"f":[{"v":"2025-03-09 23:30:00"},{"v":"1741563000"}]},',
    '{"f":[{"v":"2025-03-10 00:30:00.123456"},{"v":"1741566600.5"}]},',
    '{"f":[{"v":null},{"v":null}]}',
    "]}",
    collapse = ""
  )

  df <- parse_postprocess(bq_parse(schema, values), bigint = "integer")

  expect_s3_class(df$business_dt, "clock_naive_time")
  expect_false(inherits(df$business_dt, "POSIXt"))
  expect_s3_class(df$event_ts, "POSIXct")
  expect_equal(attr(df$event_ts, "tzone"), "UTC")

  expect_equal(
    bq_datetime_format(df$business_dt),
    c(
      "2025-03-09 23:30:00.000000",
      "2025-03-10 00:30:00.123456",
      NA
    )
  )
  expect_true(is.na(df$business_dt[3]))
  expect_true(is.na(df$event_ts[3]))

  # Grouping by the naive business day follows the printed local date.
  business_day <- clock::as_date(df$business_dt)
  expect_equal(
    as.character(business_day[1:2]),
    c("2025-03-09", "2025-03-10")
  )
})

test_that("correctly parse logical values", {
  query <- "SELECT TRUE as x"
  tb <- bq_project_query(bq_test_project(), query)
  df <- bq_table_download(tb, api = "json")

  expect_true(df$x)
})

test_that("the return type of integer columns is set by the bigint argument", {
  x <- c(
    "-2147483648",
    "-2147483647",
    "-1",
    "0",
    "1",
    "2147483647",
    "2147483648"
  )
  sql <- paste0("SELECT * FROM UNNEST ([", paste0(x, collapse = ","), "]) AS x")
  qry <- bq_project_query(bq_test_project(), sql)

  expect_warning(
    out_int <- bq_table_download(qry, bigint = "integer", api = "json")$x,
    "integer overflow"
  )
  expect_identical(out_int, suppressWarnings(as.integer(x)))

  out_int64 <- bq_table_download(qry, bigint = "integer64", api = "json")$x
  expect_identical(out_int64, bit64::as.integer64(x))

  out_dbl <- bq_table_download(qry, bigint = "numeric", api = "json")$x
  expect_identical(out_dbl, as.double(x))

  out_chr <- bq_table_download(qry, bigint = "character", api = "json")$x
  expect_identical(out_chr, x)
})

test_that("can convert geography type", {
  skip_if_not_installed("wk")
  sql <- "SELECT ST_GEOGFROMTEXT('POINT (30 10)') as geography"
  tb <- bq_project_query(bq_test_project(), sql, quiet = TRUE)
  df <- bq_table_download(tb, api = "json")

  expect_identical(df$geography, wk::wkt("POINT(30 10)"))
})

test_that("can convert bytes type", {
  sql <- "SELECT ST_ASBINARY(ST_GEOGFROMTEXT('POINT (30 10)')) as bytes"
  tb <- bq_project_query(bq_test_project(), sql, quiet = TRUE)
  df <- bq_table_download(tb, api = "json")

  expect_identical(
    df$bytes,
    blob::blob(
      as.raw(c(
        0x01,
        0x01,
        0x00,
        0x00,
        0x00,
        0xff,
        0xff,
        0xff,
        0xff,
        0xff,
        0xff,
        0x3d,
        0x40,
        0x00,
        0x00,
        0x00,
        0x00,
        0x00,
        0x00,
        0x24,
        0x40
      ))
    )
  )
})
