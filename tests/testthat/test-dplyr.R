test_that("historical API continues to work", {
  src <- src_bigquery(bq_test_project(), "basedata")

  x <- dplyr::tbl(src, "mtcars")

  expect_s3_class(x, "tbl")
  expect_true("cyl" %in% dbplyr::op_vars(x))
})

test_that("can work with literal SQL", {
  con_us <- DBI::dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    dataset = "utility_us",
    billing = bq_test_project()
  )
  x <- dplyr::tbl(con_us, dplyr::sql("SELECT * FROM country_code_iso"))

  expect_s3_class(x, "tbl")
  expect_true("fips_code" %in% dbplyr::op_vars(x))

  # If if has three or more periods (#611)
  y <- dplyr::tbl(
    con_us,
    dplyr::sql("SELECT c.fips_code, c.country_name FROM country_code_iso c")
  )
  expect_s3_class(y, "tbl")
})

test_that("can work with nested table identifier", {
  con1 <- DBI::dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    billing = bq_test_project()
  )
  # As far as I can tell from the BigQuery API there's no way to provide
  # a default project; you can either provide a default dataset + project or
  # nothing
  table_name <- I("bigquery-public-data.utility_us.country_code_iso")
  expect_no_error(dplyr::collect(head(dplyr::tbl(con1, table_name))))

  con2 <- DBI::dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    dataset = "utility_us",
    billing = bq_test_project(),
  )
  expect_no_error(dplyr::collect(head(dplyr::tbl(con2, "country_code_iso"))))
})

test_that("can copy_to", {
  ds <- bq_test_dataset()
  con <- DBI::dbConnect(ds)

  expect_snapshot(dplyr::copy_to(con, mtcars), error = TRUE)
  bq_mtcars <- dplyr::copy_to(con, mtcars, temporary = FALSE)

  expect_s3_class(bq_mtcars, "tbl_BigQueryConnection")
})

test_that("can collect and compute (no dataset)", {
  con <- DBI::dbConnect(bigquery(), project = bq_test_project())
  bq_mtcars <- dplyr::tbl(con, "basedata.mtcars") %>% dplyr::filter(cyl == 4)

  # collect
  x <- dplyr::collect(bq_mtcars)
  expect_equal(dim(x), c(11, 11))

  # compute: temporary
  temp <- dplyr::compute(bq_mtcars)
  expect_equal(dim(dplyr::collect(temp)), c(11, 11))

  # compute: persistent
  name <- paste0("basedata.", random_name())
  if (packageVersion("dbplyr") >= "2.4.0.9000") {
    name <- I(name)
  }
  perm <- dplyr::compute(bq_mtcars, name = name, temporary = FALSE)
  defer(DBI::dbRemoveTable(con, name))

  expect_true(DBI::dbExistsTable(con, name))
  expect_equal(dim(dplyr::collect(perm)), c(11, 11))
})

test_that("can collect and compute (with dataset)", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )
  bq_mtcars <- dplyr::tbl(con, "mtcars") %>% dplyr::filter(cyl == 4)

  # collect
  x <- dplyr::collect(bq_mtcars)
  expect_equal(dim(x), c(11, 11))

  # compute: temporary
  temp <- dplyr::compute(bq_mtcars)
  expect_equal(dim(dplyr::collect(temp)), c(11, 11))

  # compute: persistent
  name <- random_name()
  perm <- dplyr::compute(bq_mtcars, temporary = FALSE, name = name)
  expect_equal(dim(dplyr::collect(perm)), c(11, 11))
  defer(DBI::dbRemoveTable(con, name))

  expect_true(DBI::dbExistsTable(con, name))
})

test_that("collect can identify directly download tables", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )

  bq1 <- dplyr::tbl(con, "mtcars")
  expect_true(op_can_download(bq1))
  expect_equal(op_rows(bq1), Inf)
  expect_equal(format(op_table(bq1)), "`mtcars`")

  bq2 <- head(bq1, 4)
  expect_true(op_can_download(bq2))
  expect_equal(op_rows(bq2), 4)
  expect_equal(format(op_table(bq1)), "`mtcars`")

  bq3 <- head(bq2, 2)
  expect_true(op_can_download(bq3))
  expect_equal(op_rows(bq3), 2)

  bq3 <- dplyr::filter(bq1, cyl == 1)
  expect_false(op_can_download(bq3))

  x <- dplyr::collect(bq1)
  expect_s3_class(x, "tbl")

  bq5 <- dplyr::tbl(con, dplyr::sql("SELECT * FROM mtcars"))
  expect_false(op_can_download(bq5))
  expect_false(op_can_download(head(bq5)))

  view_name <- random_name()
  DBI::dbExecute(
    con,
    paste0("CREATE VIEW ", view_name, " AS SELECT * FROM basedata.mtcars")
  )
  defer(DBI::dbExecute(con, paste0("DROP VIEW ", view_name)))
  bq6 <- dplyr::tbl(con, view_name)
  expect_false(op_can_download(bq6))

  # INFORMATION_SCHEMA.TABLES errors when we attempt to get metadata
  bq7 <- dplyr::tbl(con, "INFORMATION_SCHEMA.PARTITIONS")
  expect_false(op_can_download(bq7))
  expect_false(op_can_download(head(bq7)))
})

test_that("casting uses bigquery types", {
  skip_if_not_installed("dbplyr")
  con <- simulate_bigrquery()

  expect_equal(
    dbplyr::translate_sql(as.integer(x), con = con),
    dbplyr::sql("SAFE_CAST(`x` AS INT64)")
  )
  expect_equal(
    dbplyr::translate_sql(as.numeric(x), con = con),
    dbplyr::sql("SAFE_CAST(`x` AS FLOAT64)")
  )
})

test_that("%||% translates to IFNULL", {
  skip_if_not_installed("dbplyr")

  expect_equal(
    dbplyr::translate_sql(x %||% 2L, con = simulate_bigrquery()),
    dbplyr::sql("IFNULL(`x`, 2)")
  )
})

test_that("suffixes use _", {
  skip_if_not_installed("dbplyr", "1.99")

  expect_equal(dbplyr::sql_join_suffix(simulate_bigrquery()), c("_x", "_y"))
})

test_that("all BigQuery tbls share the same src", {
  con1 <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project()
  )
  con2 <- DBI::dbConnect(
    bigquery(),
    project = "publicdata",
    billing = bq_test_project()
  )

  tbl1 <- dplyr::tbl(con1, I("basedata.mtcars"), vars = "x")
  tbl2 <- dplyr::tbl(con2, I("publicdata.samples.natality"), vars = "x")
  expect_true(dplyr::same_src(tbl1, tbl2))
  expect_false(dplyr::same_src(tbl1, mtcars))
})

test_that("runif is correctly translated", {
  skip_if_not_installed("dbplyr", "2.4.0")
  expect_equal(
    dbplyr::translate_sql(runif(n()), con = simulate_bigrquery()),
    dbplyr::sql("RAND()")
  )
})

test_that("string functions correctly", {
  con <- simulate_bigrquery()

  expect_snapshot({
    dbplyr::translate_sql(grepl("a.c", x), con = con)
    dbplyr::translate_sql(gsub("a.c", "", x), con = con)
  })
})

test_that("median is correctly translated", {
  expect_equal(
    dbplyr::translate_sql(
      median(x),
      con = simulate_bigrquery(),
      window = FALSE
    ),
    dbplyr::sql("APPROX_QUANTILES(`x`, 2)[SAFE_ORDINAL(2)]")
  )
})

test_that("can correctly print a lazy query", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )

  bq_mtcars <- dplyr::tbl(con, "mtcars")

  # not a snapshot test because column order can vary
  expect_no_error(
    expect_output(
      print(bq_mtcars)
    )
  )
})

# compute() into an existing partitioned/clustered table --------------------

# Sets up a destination table with the given partition and clustering fields
# and the columns produced by the query, capturing the SQL that would be
# submitted. Returns an environment whose `sql` is the submitted SQL, or `NULL`
# when no job was submitted.
local_compute_destination <- function(
  columns,
  partition = NULL,
  clustering = NULL,
  exists = TRUE,
  env = caller_env()
) {
  capture <- new.env(parent = emptyenv())
  capture$sql <- NULL

  fields <- lapply(names(columns), function(name) {
    type <- columns[[name]]
    list(
      name = name,
      type = if (identical(type, "REPEATED")) "STRING" else type,
      mode = if (identical(type, "REPEATED")) "REPEATED" else "NULLABLE"
    )
  })

  meta <- list()
  if (!is.null(partition)) {
    meta$timePartitioning <- list(type = "DAY", field = partition)
  }
  if (!is.null(clustering)) {
    meta$clustering <- list(fields = as.list(clustering))
  }

  local_mocked_bindings(
    bq_table_exists = function(x) exists,
    bq_table_meta = function(x, fields = NULL) meta,
    bq_perform_query_schema = function(query, billing, ...) fields,
    bq_dataset_query = function(
      x,
      query,
      destination_table = NULL,
      ...,
      billing = NULL
    ) {
      capture$sql <- as.character(query)
      bq_table("test", "test", "destination")
    },
    bq_project_query = function(x, query, destination_table = NULL, ...) {
      capture$sql <- as.character(query)
      bq_table("test", "test", "destination")
    },
    .env = env
  )

  capture
}

test_that("compute() validates the partition and clustering columns", {
  con <- simulate_bigrquery()
  sql <- dbplyr::sql("SELECT region_x, event_date, value FROM src")
  columns <- list(region_x = "STRING", event_date = "DATE", value = "INTEGER")

  # A column map resolves a differently named clustering column and renames it
  # in the submitted query.
  capture <- local_compute_destination(
    columns,
    partition = "event_date",
    clustering = "region"
  )
  out <- db_compute.BigQueryConnection(
    con,
    I("test.test.destination"),
    sql,
    temporary = FALSE,
    overwrite = TRUE,
    column_map = c(region = "region_x")
  )
  expect_s3_class(out, "dbplyr_table_path")
  expect_snapshot(cat(capture$sql))

  # Without a map the clustering column isn't produced, so no job is submitted
  capture <- local_compute_destination(
    columns,
    partition = "event_date",
    clustering = "region"
  )
  expect_snapshot(
    db_compute.BigQueryConnection(
      con,
      I("test.test.destination"),
      sql,
      temporary = FALSE,
      overwrite = TRUE
    ),
    error = TRUE
  )
  expect_null(capture$sql)
})

test_that("compute() rejects invalid partition and clustering types", {
  con <- simulate_bigrquery()
  sql <- dbplyr::sql("SELECT * FROM src")

  capture <- local_compute_destination(
    list(day = "STRING", region = "STRING"),
    partition = "day"
  )
  expect_snapshot(
    db_compute.BigQueryConnection(
      con,
      I("test.test.destination"),
      sql,
      temporary = FALSE,
      overwrite = TRUE
    ),
    error = TRUE
  )
  expect_null(capture$sql)

  capture <- local_compute_destination(
    list(day = "DATE", region = "BYTES"),
    partition = "day",
    clustering = "region"
  )
  expect_snapshot(
    db_compute.BigQueryConnection(
      con,
      I("test.test.destination"),
      sql,
      temporary = FALSE,
      overwrite = TRUE
    ),
    error = TRUE
  )
  expect_null(capture$sql)

  capture <- local_compute_destination(
    list(day = "REPEATED", region = "STRING"),
    partition = "day"
  )
  expect_snapshot(
    db_compute.BigQueryConnection(
      con,
      I("test.test.destination"),
      sql,
      temporary = FALSE,
      overwrite = TRUE
    ),
    error = TRUE
  )
  expect_null(capture$sql)
})

test_that("compute() detects conflicting target column names", {
  con <- simulate_bigrquery()
  sql <- dbplyr::sql("SELECT * FROM src")

  # Both query columns would be renamed to the same target `region`
  capture <- local_compute_destination(
    list(region_x = "STRING", region_y = "STRING"),
    clustering = "region"
  )
  expect_snapshot(
    db_compute.BigQueryConnection(
      con,
      I("test.test.destination"),
      sql,
      temporary = FALSE,
      overwrite = TRUE,
      column_map = c(region = "region_x", region = "region_y")
    ),
    error = TRUE
  )
  expect_null(capture$sql)
})

test_that("compute() maps same-named columns from different sources", {
  con <- simulate_bigrquery()
  # A join of two sources yields `region_x` and `region_y`; the destination
  # expects a single `region` clustering column.
  sql <- dbplyr::sql("SELECT region_x, region_y, day FROM src")
  capture <- local_compute_destination(
    list(region_x = "STRING", region_y = "STRING", day = "DATE"),
    partition = "day",
    clustering = "region"
  )
  expect_snapshot(
    db_compute.BigQueryConnection(
      con,
      I("test.test.destination"),
      sql,
      temporary = FALSE,
      overwrite = TRUE
    ),
    error = TRUE
  )
  expect_null(capture$sql)

  out <- db_compute.BigQueryConnection(
    con,
    I("test.test.destination"),
    sql,
    temporary = FALSE,
    overwrite = TRUE,
    column_map = c(region = "region_x", unused_region = "region_y")
  )
  expect_s3_class(out, "dbplyr_table_path")
  expect_snapshot(cat(capture$sql))
})

test_that("compute() rejects a duplicate source column name in the query", {
  con <- simulate_bigrquery()
  capture <- local_compute_destination(
    list(region = "STRING", region = "STRING", day = "DATE"),
    partition = "day"
  )
  expect_snapshot(
    db_compute.BigQueryConnection(
      con,
      I("test.test.destination"),
      dbplyr::sql("SELECT * FROM src"),
      temporary = FALSE,
      overwrite = TRUE
    ),
    error = TRUE
  )
  expect_null(capture$sql)
})

test_that("compute() accepts column_map as a data frame", {
  con <- simulate_bigrquery()
  sql <- dbplyr::sql("SELECT region_x, day FROM src")
  capture <- local_compute_destination(
    list(region_x = "STRING", day = "DATE"),
    partition = "day",
    clustering = "region"
  )
  out <- db_compute.BigQueryConnection(
    con,
    I("test.test.destination"),
    sql,
    temporary = FALSE,
    overwrite = TRUE,
    column_map = data.frame(source = "region_x", target = "region")
  )
  expect_s3_class(out, "dbplyr_table_path")
  expect_snapshot(cat(capture$sql))
})

test_that("compute() errors on a column_map naming an unknown column", {
  con <- simulate_bigrquery()
  capture <- local_compute_destination(
    list(region = "STRING", day = "DATE"),
    partition = "day"
  )
  expect_snapshot(
    db_compute.BigQueryConnection(
      con,
      I("test.test.destination"),
      dbplyr::sql("SELECT * FROM src"),
      temporary = FALSE,
      overwrite = TRUE,
      column_map = c(area = "region_x")
    ),
    error = TRUE
  )
  expect_null(capture$sql)
})

test_that("compute() doesn't validate when the destination is new", {
  con <- simulate_bigrquery()
  sql <- dbplyr::sql("SELECT * FROM src")

  capture <- local_compute_destination(
    list(value = "INTEGER"),
    partition = "day",
    exists = FALSE
  )
  out <- db_compute.BigQueryConnection(
    con,
    I("test.test.destination"),
    sql,
    temporary = FALSE,
    overwrite = TRUE
  )
  expect_s3_class(out, "dbplyr_table_path")
  expect_snapshot(cat(capture$sql))
})
