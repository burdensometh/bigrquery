# as_recalc_date coerces and validates

    Code
      as_recalc_date("not a date")
    Condition
      Error:
      ! `"not a date"` must be a <Date> or a date string like "2026-11-22".
      x character string is not in a standard unambiguous format

---

    Code
      as_recalc_date(as.Date("2026-11-22") + 0:1)
    Condition
      Error:
      ! `as.Date("2026-11-22") + 0:1` must be a single, non-missing date.

# validation failure keeps staging and does not publish

    Code
      bq_table_recalc(bq_table("proj", "ds", "trades"), "SELECT 1", "2026-11-22",
      date_column = "trade_date", quiet = TRUE)
    Condition
      Error in `bq_table_recalc()`:
      ! Recalculation of "proj.ds.trades" for 2026-11-22 failed validation:
      x `trade_date` ranges from 2026-11-21 to 2026-11-22, but every row must be 2026-11-22.
      i Target table was not modified.
      i Staging table "proj.ds.trades_staging_20261122" kept for inspection (load job "proj.job_load.US").

# missing target table fails before loading

    Code
      bq_table_recalc(bq_table("proj", "ds", "trades"), "SELECT 1", "2026-11-22")
    Condition
      Error in `bq_table_recalc()`:
      ! Target table "proj.ds.trades" does not exist.

