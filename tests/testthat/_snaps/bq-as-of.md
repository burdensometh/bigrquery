# as_of rejects invalid inputs

    Code
      as_bq_as_of("not a time")
    Condition
      Error:
      ! `"not a time"` must be a valid UTC date-time, not "not a time".
      i Use e.g. "2024-01-01 12:00:00" or "2024-01-01T12:00:00Z".
    Code
      as_bq_as_of("2024-01-01junk")
    Condition
      Error:
      ! `"2024-01-01junk"` must be a valid UTC date-time, not "2024-01-01junk".
      i Use e.g. "2024-01-01 12:00:00" or "2024-01-01T12:00:00Z".
    Code
      as_bq_as_of("2024-02-30 10:00:00")
    Condition
      Error:
      ! `"2024-02-30 10:00:00"` must be a valid UTC date-time, not "2024-02-30 10:00:00".
      i Use e.g. "2024-01-01 12:00:00" or "2024-01-01T12:00:00Z".
    Code
      as_bq_as_of(c("2024-01-01", "2024-01-02"))
    Condition
      Error:
      ! `c("2024-01-01", "2024-01-02")` must be a single, non-missing time.
    Code
      as_bq_as_of(NA_character_)
    Condition
      Error:
      ! `NA_character_` must be a single, non-missing time.
    Code
      as_bq_as_of(1)
    Condition
      Error:
      ! `1` must be a POSIXct, a Date, or a string, not the number 1.

# bq_table_as_of stores source and read time and reports them

    Code
      print(out)
    Output
      <bq_table_as_of> billing._anon.result
      Source: my-project.my_dataset.my_table
      As of:  2024-01-01 12:00:00 UTC

# bq_table_as_of only accepts its own tables' metadata

    Code
      bq_table_as_of_source(bq_table("a", "b", "c"))
    Condition
      Error in `bq_table_as_of_source()`:
      ! `x` must be a table created by `bq_table_as_of()`.
    Code
      bq_table_as_of_time(bq_table("a", "b", "c"))
    Condition
      Error in `bq_table_as_of_time()`:
      ! `x` must be a table created by `bq_table_as_of()`.

# out-of-window errors are distinguishable from empty results

    Code
      bq_table_as_of(source, as_of = "2020-09-26 00:00:00")
    Condition
      Error:
      ! Can't read my-project.my_dataset.my_table as of 2020-09-26 00:00:00 UTC.
      x That instant is outside the time travel window for my-project.my_dataset.my_table.
      i The oldest readable data starts at 2020-10-01 17:30:10.026 UTC.
      i BigQuery said: Invalid snapshot time 1601168925462 for table my-project:my_dataset.my_table@1601168925462. Cannot read before 1601573410026. [invalidQuery]

# unrelated job errors are not reclassified

    Code
      bq_table_as_of(source, as_of = "2024-01-01 00:00:00")
    Condition
      Error in `bq_job_wait()`:
      ! Job <bq_job> failed

