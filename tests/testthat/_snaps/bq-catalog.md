# an expired page token restarts the listing once

    Code
      bq_catalog_dataset_tables(catalog, "p", "ds")
    Condition
      Warning:
      Saved page token for p__ds was rejected by the API; the listing was restarted.

# committing builds a version from completed datasets only

    Code
      bq_catalog_commit(catalog)
    Condition
      Error:
      ! Can't commit catalog while 1 dataset still in progress:
      i Re-run `bq_catalog_dataset_tables()` for "p__half" or set `only_complete = FALSE`.

# print method summarises dataset states

    Code
      print(catalog)
    Output
      <bq_catalog> <CATALOG>

---

    Code
      print(catalog)
    Output
      <bq_catalog> <CATALOG>
        complete: 1 dataset

# a stale lock left by a crashed run is taken over

    Code
      bq_catalog_dataset_tables(catalog, "p", "ds")
    Condition
      Warning:
      Removing stale catalog lock on p__ds

# unknown and uncommitted datasets give a clear error

    Code
      bq_catalog_tables(catalog, "p", "missing")
    Condition
      Error:
      ! Dataset "p.missing" has not been committed to this catalog.

