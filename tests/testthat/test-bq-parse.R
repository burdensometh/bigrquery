# Individual values -------------------------------------------------------

test_that("can parse atomic vectors", {
  expect_identical(bq_parse_single("x", "string"), "x")
  expect_identical(bq_parse_single("10", "integer"), bit64::as.integer64(10))
  expect_identical(bq_parse_single("10", "float"), 10)
  expect_identical(bq_parse_single("true", "boolean"), TRUE)
  expect_identical(bq_parse_single("false", "boolean"), FALSE)
})

test_that("gracefully handles int64 values", {
  # Largest/smallest 32-bit int
  expect_equal(
    bq_parse_single("2147483647", "integer"),
    bit64::as.integer64(2147483647L)
  )
  expect_equal(
    bq_parse_single("-2147483647", "integer"),
    bit64::as.integer64(-2147483647L)
  )
  # 2147483647 + 1
  expect_equal(
    bq_parse_single("2147483648", "integer"),
    bit64::as.integer64("2147483648")
  )
  expect_equal(
    bq_parse_single("-2147483648", "integer"),
    bit64::as.integer64("-2147483648")
  )
  # Largest/smallest integer64
  expect_equal(
    bq_parse_single("9223372036854775807", "integer"),
    bit64::as.integer64("9223372036854775807")
  )
  expect_equal(
    bq_parse_single("-9223372036854775807", "integer"),
    bit64::as.integer64("-9223372036854775807")
  )
  # Largest/smallest integer64 + 1
  expect_equal(
    bq_parse_single("9223372036854775808", "integer"),
    bit64::as.integer64(NA)
  )
  expect_equal(
    bq_parse_single("-9223372036854775808", "integer"),
    bit64::as.integer64(NA)
  )
})

test_that("can parse NULLs", {
  expect_identical(bq_parse_single(NULL, "string"), NA_character_)
  expect_identical(bq_parse_single(NULL, "integer"), bit64::as.integer64(NA))
  expect_identical(bq_parse_single(NULL, "float"), NA_real_)
  expect_identical(bq_parse_single(NULL, "boolean"), NA)

  na_dtm <- structure(as.POSIXct(NA), tzone = "UTC")
  expect_identical(bq_parse_single(NULL, "timestamp"), na_dtm)
})


test_that("can parse arrays of simple values", {
  x <- vs("1", "2", "3")

  out1 <- bq_parse_single(x, "string", mode = "repeated")
  expect_equal(out1, list(c("1", "2", "3")))

  out2 <- bq_parse_single(x, "integer", mode = "repeated")
  expect_equal(out2, list(bit64::as.integer64(1:3)))
})

test_that("can parse structs of simple values", {
  fields <- list(
    bq_field("x", "integer"),
    bq_field("y", "string")
  )

  x <- f(v("1"), v("a"))
  out <- bq_parse_single(x, "record", fields = fields)

  expect_equal(out, list(list(x = bit64::as.integer64(1L), y = "a")))
})

test_that("can parse structs of arrays", {
  fields <- list(
    bq_field("x", "integer", "repeated"),
    bq_field("y", "string", "repeated")
  )

  x <- f(v(vs("1", "2", "3")), v(vs("a", "b")))
  out <- bq_parse_single(x, "record", fields = fields)

  expect_equal(out, list(list(x = bit64::as.integer64(1:3), y = c("a", "b"))))
})


test_that("can parse arrays of structs", {
  fields <- list(
    bq_field("x", "integer"),
    bq_field("y", "string")
  )

  x <- vs(list(f = vs("1", "a")), list(f = vs("2", "b")))
  out <- bq_parse_single(x, "record", mode = "repeated", fields = fields)

  expect_equal(out, list(tibble(x = bit64::as.integer64(1:2), y = c("a", "b"))))
})


# Complete files ----------------------------------------------------------

replay_query <- function(name, sql) {
  schema_path <- test_path(sprintf("parse-schema-%s.json", name))
  values_path <- test_path(sprintf("parse-values-%s.json", name))

  if (!file.exists(schema_path)) {
    tbl <- bq_project_query(bq_test_project(), sql)
    bq_table_save_schema(tbl, schema_path)
    bq_table_save_values(tbl, values_path)
  }

  out <- bq_parse_file(schema_path, values_path)
  tibble::as_tibble(out)
}

test_that("can parse nested structures", {
  df <- replay_query("struct", "SELECT STRUCT(1 AS a, 'abc' AS b) as x")
  expect_named(df, "x")
  expect_type(df$x, "list")
  expect_equal(df$x[[1]], list(a = bit64::as.integer64(1), b = "abc"))

  df <- replay_query("array", "SELECT [1, 2, 3] as x")
  expect_named(df, "x")
  expect_type(df$x, "list")
  expect_equal(df$x[[1]], bit64::as.integer64(1:3))

  df <- replay_query(
    "array-struct",
    "SELECT [STRUCT(1 as a, 'a' as b), STRUCT(2, 'b'), STRUCT(3, 'c')] as x"
  )
  expect_named(df, "x")
  expect_type(df$x, "list")
  expect_equal(
    df$x[[1]],
    tibble(a = bit64::as.integer64(1:3), b = c("a", "b", "c"))
  )

  df <- replay_query(
    "struct-array",
    "SELECT STRUCT([1, 2, 3] as a, ['a', 'b'] as b) as x"
  )
  expect_named(df, "x")
  expect_type(df$x, "list")
  expect_equal(df$x[[1]], list(a = bit64::as.integer64(1:3), b = c("a", "b")))
})

test_that("can parse empty arrays", {
  tb <- bq_project_query(bq_test_project(), "SELECT ARRAY<INT64>[] as x")
  df <- bq_table_download(tb, api = "json")
  expect_equal(df$x, list(integer(length = 0)))

  tb <- bq_project_query(
    bq_test_project(),
    "SELECT ARRAY<STRUCT<a INT64, b STRING>>[] as x"
  )
  df <- bq_table_download(tb, api = "json")
  expect_equal(
    df$x,
    list(tibble::tibble(a = integer(length = 0), b = character()))
  )
})

test_that("can parse geography", {
  skip_if_not_installed("wk")

  wkt <- wk::wkt("POINT (30 10)")
  expect_identical(bq_parse_single(as.character(wkt), "geography"), wkt)
  expect_identical(
    bq_parse_single(NA_character_, "geography"),
    wk::wkt(NA_character_)
  )
})

test_that("can parse bytes", {
  bytes <- blob::blob(
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

  expect_identical(bq_parse_single(bytes[[1]], "bytes"), bytes)
  expect_identical(
    bq_parse_single(NA_character_, "bytes"),
    blob::blob(NULL)
  )
})

# Exact decimal (NUMERIC/BIGNUMERIC) --------------------------------------

test_that("NUMERIC defaults to double and BIGNUMERIC to character", {
  schema_path <- test_path("parse-schema-decimal.json")
  values_path <- test_path("parse-values-decimal.json")

  out <- bq_parse_file(schema_path, values_path)

  expect_type(out$num, "double")
  expect_type(out$bignum, "character")
  expect_equal(out$num[1], 123456789.123456789, tolerance = 1e-9)
  expect_true(is.na(out$num[2]))
  expect_true(is.na(out$bignum[2]))
  # Repeated and nested NUMERIC stay double in the default mode
  expect_type(out$num_array[[1]], "double")
  expect_equal(out$num_array[[1]], c(0.1, -2.5, 100))
  expect_equal(out$num_array[[2]], numeric(0))
  expect_type(out$rec[[1]]$n, "double")
  expect_type(out$rec_array[[1]]$n[[1]], "double")
})

test_that("exact decimal mode keeps NUMERIC/BIGNUMERIC as canonical strings", {
  schema_path <- test_path("parse-schema-decimal.json")
  values_path <- test_path("parse-values-decimal.json")

  out <- bq_parse_file(schema_path, values_path, exact_decimal = TRUE)

  expect_type(out$num, "character")
  expect_type(out$bignum, "character")
  expect_identical(out$num, c(
    "123456789.123456789",
    NA,
    "-0.000000001"
  ))
  expect_identical(out$bignum, c(
    "12345678901234567890123456789.12345678901234567890123456789012345678",
    NA,
    "99999999999999999999999999999.999999999"
  ))
})

test_that("exact decimal mode works inside repeated and nested fields", {
  schema_path <- test_path("parse-schema-decimal.json")
  values_path <- test_path("parse-values-decimal.json")

  out <- bq_parse_file(schema_path, values_path, exact_decimal = TRUE)

  # Repeated NUMERIC
  expect_identical(out$num_array[[1]], c("0.1", "-2.5", "100"))
  expect_identical(out$num_array[[2]], character(0))
  expect_identical(out$num_array[[3]], "1")

  # Nested record
  expect_identical(out$rec[[1]]$n, "3.141592653589793238")
  expect_true(is.na(out$rec[[1]]$b))
  expect_true(is.na(out$rec[[3]]$n))
  expect_identical(out$rec[[3]]$b, "7.7")

  # Repeated record containing a repeated NUMERIC
  expect_identical(out$rec_array[[1]]$n[[1]], c("1.1", "2.2"))
  expect_identical(out$rec_array[[1]]$n[[2]], "3.3")
  expect_equal(nrow(out$rec_array[[2]]), 0)
  expect_true(is.na(out$rec_array[[3]]$n[[1]]))
})

test_that("bq_parse_single honours exact_decimal", {
  expect_type(bq_parse_single("1.23456789", "numeric"), "double")
  expect_identical(
    bq_parse_single("1.23456789", "numeric", exact_decimal = TRUE),
    "1.23456789"
  )
  expect_identical(
    bq_parse_single(
      "12345678901234567890123456789.123456789",
      "bignumeric",
      exact_decimal = TRUE
    ),
    "12345678901234567890123456789.123456789"
  )

  na_num <- bq_parse_single(NULL, "numeric", exact_decimal = TRUE)
  expect_identical(na_num, NA_character_)
  na_big <- bq_parse_single(NULL, "bignumeric", exact_decimal = TRUE)
  expect_identical(na_big, NA_character_)

  repeated <- bq_parse_single(
    vs("1.1", "2.2"),
    "numeric",
    mode = "repeated",
    exact_decimal = TRUE
  )
  expect_identical(repeated, list(c("1.1", "2.2")))
})
