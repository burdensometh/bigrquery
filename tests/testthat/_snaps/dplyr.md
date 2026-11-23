# string functions correctly

    Code
      dbplyr::translate_sql(grepl("a.c", x), con = con)
    Output
      <SQL> REGEXP_CONTAINS(`x`, 'a.c')
    Code
      dbplyr::translate_sql(gsub("a.c", "", x), con = con)
    Output
      <SQL> REGEXP_REPLACE(`x`, 'a.c', '')

# compute() validates the partition and clustering columns

    Code
      cat(capture$sql)
    Output
      SELECT `region_x` AS `region`, `event_date`, `value`
      FROM (
      SELECT region_x, event_date, value FROM src
      )

---

    Code
      db_compute.BigQueryConnection(con, I("test.test.destination"), sql, temporary = FALSE,
      overwrite = TRUE)
    Condition
      Error:
      ! Can't write to "test.test.destination": clustering column region is not produced by the query.
      i The query produces the columns region_x, event_date, and value.
      i Map a query column to region, e.g. `column_map = c(region = "column")`.

# compute() rejects invalid partition and clustering types

    Code
      db_compute.BigQueryConnection(con, I("test.test.destination"), sql, temporary = FALSE,
      overwrite = TRUE)
    Condition
      Error:
      ! Can't write to "test.test.destination": partition column day has type "STRING", which is not valid for partition.
      i Valid types are DATE, DATETIME, and TIMESTAMP.

---

    Code
      db_compute.BigQueryConnection(con, I("test.test.destination"), sql, temporary = FALSE,
      overwrite = TRUE)
    Condition
      Error:
      ! Can't write to "test.test.destination": clustering column region has type "BYTES", which is not valid for clustering.
      i Valid types are STRING, GEOGRAPHY, INT64, NUMERIC, BIGNUMERIC, DATE, DATETIME, TIMESTAMP, BOOL, and RANGE.

---

    Code
      db_compute.BigQueryConnection(con, I("test.test.destination"), sql, temporary = FALSE,
      overwrite = TRUE)
    Condition
      Error:
      ! Can't write to "test.test.destination": partition column day is a repeated field.
      i partition columns must be a single value, not an array.

# compute() detects conflicting target column names

    Code
      db_compute.BigQueryConnection(con, I("test.test.destination"), sql, temporary = FALSE,
      overwrite = TRUE, column_map = c(region = "region_x", region = "region_y"))
    Condition
      Error:
      ! Can't materialise the query: multiple columns map to the target column region.
      x Conflicting source columns: region_x and region_y.
      i Map each source column to a different target column.

# compute() maps same-named columns from different sources

    Code
      db_compute.BigQueryConnection(con, I("test.test.destination"), sql, temporary = FALSE,
      overwrite = TRUE)
    Condition
      Error:
      ! Can't write to "test.test.destination": clustering column region is not produced by the query.
      i The query produces the columns region_x, region_y, and day.
      i Map a query column to region, e.g. `column_map = c(region = "column")`.

---

    Code
      cat(capture$sql)
    Output
      SELECT `region_x` AS `region`, `region_y` AS `unused_region`, `day`
      FROM (
      SELECT region_x, region_y, day FROM src
      )

# compute() rejects a duplicate source column name in the query

    Code
      db_compute.BigQueryConnection(con, I("test.test.destination"), dbplyr::sql(
        "SELECT * FROM src"), temporary = FALSE, overwrite = TRUE)
    Condition
      Error:
      ! Can't materialise the query: it produces the column name region more than once.
      i Rename the columns in the query so each output column has a distinct name.

# compute() accepts column_map as a data frame

    Code
      cat(capture$sql)
    Output
      SELECT `region_x` AS `region`, `day`
      FROM (
      SELECT region_x, day FROM src
      )

# compute() errors on a column_map naming an unknown column

    Code
      db_compute.BigQueryConnection(con, I("test.test.destination"), dbplyr::sql(
        "SELECT * FROM src"), temporary = FALSE, overwrite = TRUE, column_map = c(
        area = "region_x"))
    Condition
      Error:
      ! `column_map` maps unknown column region_x.
      i The query produces the columns region and day.

# compute() doesn't validate when the destination is new

    Code
      cat(capture$sql)
    Output
      SELECT * FROM src

