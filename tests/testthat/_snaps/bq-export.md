# a failed job publishes no manifest

    Code
      bq_table_save_manifest(tb, "bkt", prefix = "exports/table", run = "run-1")
    Condition
      Error in `bq_table_save_manifest()`:
      ! Export job "proj.job_1.US" failed.
      x No manifest was published.
      i Use a new `run` to try again; any partial shards are left in place.
      Caused by error in `bq_job_wait()`:
      ! Job failed

# shards that don't match the job report publish no manifest

    Code
      bq_table_save_manifest(tb, "bkt", prefix = "exports/table", run = "run-1")
    Condition
      Error in `bq_table_save_manifest()`:
      ! Export shards could not be verified, so no manifest was published.
      x The job reported 2 shards, but 1 object was found below "exports/table/run-1/".

# a run that already contains objects is refused

    Code
      bq_table_save_manifest(tb, "bkt", prefix = "exports/table", run = "run-1")
    Condition
      Error in `bq_table_save_manifest()`:
      ! Export run "run-1" already contains 1 object.
      i Each run must write to an empty prefix, so the objects of two batches are never mixed.
      i Use a new `run`, or delete the objects below "gs://bkt/exports/table/run-1/" first.

# an existing manifest is never overwritten

    Code
      bq_table_save_manifest(tb, "bkt", prefix = "exports/table", run = "run-1")
    Condition
      Error in `bq_table_save_manifest()`:
      ! The manifest "gs://bkt/exports/table/run-1/manifest.json" already exists.
      i Manifests are never overwritten; use a new `run`.
      Caused by error in `gs_object_write()`:
      ! Precondition failed

# invalid manifests are rejected

    Code
      bq_export_read("gs://bkt/manifest.json")
    Condition
      Error in `bq_export_read()`:
      ! `x` contains an unknown export status "partial".

---

    Code
      bq_export_read("gs://bkt/manifest.json")
    Condition
      Error in `bq_export_read()`:
      ! `x` does not record the identity of the export job.

# validates its arguments

    Code
      bq_table_save_manifest(tb, "gs://bkt")
    Condition
      Error in `bq_table_save_manifest()`:
      ! `bucket` must be a bucket name, not a "gs://" URI.

---

    Code
      bq_table_save_manifest(tb, "bkt", run = "a/b")
    Condition
      Error in `bq_table_save_manifest()`:
      ! `run` must be a single path segment containing only letters, digits, dots, underscores, or dashes.

---

    Code
      bq_table_save_manifest(tb, "bkt", file_name = "part-0")
    Condition
      Error in `bq_table_save_manifest()`:
      ! `file_name` must be a single path segment containing exactly one "*".

---

    Code
      bq_table_save_manifest(tb, "bkt", file_name = "a/part-*.json")
    Condition
      Error in `bq_table_save_manifest()`:
      ! `file_name` must be a single path segment containing exactly one "*".

---

    Code
      bq_table_save_manifest(tb, "bkt", destination_uris = "gs://bkt/x-*")
    Condition
      Error in `bq_table_save_manifest()`:
      ! `destination_uris` is controlled by `bq_table_save_manifest()`.
      i Use `bucket`, `prefix`, `run`, and `file_name` instead.

# has a useful print method

    Code
      export
    Output
      <bq_export> complete
        Job:      proj.job_1.US
        Table:    proj.ds.table
        Prefix:   exports/table/run-1/
        Shards:   2
        Manifest: gs://bkt/exports/table/run-1/manifest.json

