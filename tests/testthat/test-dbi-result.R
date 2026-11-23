test_that("can retrieve full query results", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata",
    bigint = "integer"
  )

  df <- DBI::dbGetQuery(con, "SELECT count(*) as count FROM mtcars")
  expect_equal(df, tibble(count = 32L))
})

test_that("can retrieve without dataset", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    bigint = "integer"
  )
  df <- DBI::dbGetQuery(con, "SELECT count(*) as count FROM `basedata.mtcars`")
  expect_equal(df, tibble(count = 32L))
})

test_that("can retrieve query in pieces and that quiet is respected", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )

  res <- DBI::dbSendQuery(con, "SELECT cyl, mpg FROM mtcars")
  expect_equal(DBI::dbGetRowCount(res), 0L)

  expect_snapshot(DBI::dbFetch(res, NA), error = TRUE)

  res@quiet <- FALSE
  expect_snapshot(df <- DBI::dbFetch(res, 10))

  expect_equal(nrow(df), 10)
  expect_false(DBI::dbHasCompleted(res))
  expect_equal(DBI::dbGetRowCount(res), 10L)

  res@quiet <- TRUE
  expect_snapshot(df <- DBI::dbFetch(res, -1))
  expect_equal(nrow(df), 22)
  expect_true(DBI::dbHasCompleted(res))
})

test_that("dbHasCompleted() is accurate if you fetch past end", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )

  res <- DBI::dbSendQuery(con, "SELECT cyl FROM mtcars LIMIT 5")
  df <- DBI::dbFetch(res, 10)

  expect_equal(DBI::dbGetRowCount(res), 5)
  expect_true(DBI::dbHasCompleted(res))
})

test_that("can get metadata", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )
  sql <- "SELECT cyl, mpg FROM mtcars"
  res <- DBI::dbSendQuery(con, sql)
  expect_snapshot(res)

  col_info <- DBI::dbColumnInfo(res)
  expect_equal(dim(col_info), c(2, 2))
  expect_equal(col_info$name, c("cyl", "mpg"))

  expect_equal(DBI::dbGetStatement(res), sql)
})

test_that("dbExecute returns modified rows", {
  ds <- bq_test_dataset()
  con <- DBI::dbConnect(ds)

  DBI::dbExecute(con, "CREATE TABLE foo (a INT64)")
  expect_equal(DBI::dbExecute(con, "INSERT INTO foo VALUES (1), (2), (3)"), 3)
  expect_equal(DBI::dbExecute(con, "DELETE FROM foo WHERE a >= 2"), 2)
  DBI::dbExecute(con, "DROP TABLE foo")
})

test_that("dbFetch() passes the connection decimal setting to download", {
  con <- DBI::dbConnect(
    bigquery(),
    project = "p",
    dataset = "d",
    decimal = "character"
  )
  res <- new(
    "BigQueryResult",
    bq_table = as_bq_table("p.d.t"),
    statement = "SELECT 1",
    nrow = 1,
    affected = 0,
    page_size = 1e4,
    quiet = TRUE,
    cursor = cursor(1),
    bigint = "integer",
    decimal = "character",
    billing = "p"
  )

  local_mocked_bindings(
    has_bigrquerystorage = function() FALSE,
    bq_table_download = function(x, ..., decimal = "double", api) {
      expect_equal(decimal, "character")
      expect_equal(api, "json")
      tibble::tibble(num = c("1.23456789"))
    }
  )

  df <- DBI::dbFetch(res)
  expect_identical(df$num, "1.23456789")
})

test_that("dbFetch() defaults to the historical double behaviour", {
  res <- new(
    "BigQueryResult",
    bq_table = as_bq_table("p.d.t"),
    statement = "SELECT 1",
    nrow = 1,
    affected = 0,
    page_size = 1e4,
    quiet = TRUE,
    cursor = cursor(1),
    bigint = "integer",
    decimal = "double",
    billing = "p"
  )

  local_mocked_bindings(
    has_bigrquerystorage = function() FALSE,
    bq_table_download = function(x, ..., decimal = "double", api) {
      expect_equal(decimal, "double")
      expect_equal(api, "json")
      tibble::tibble(num = 1.23456789)
    }
  )

  df <- DBI::dbFetch(res)
  expect_type(df$num, "double")
})
