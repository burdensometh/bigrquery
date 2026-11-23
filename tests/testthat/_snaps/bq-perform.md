# bq_perform_upload creates job that succeeds

    Code
      bq_job_wait(job, quiet = FALSE)
    Message
      Job complete
      Input: <xxx> kB
      Output: <xxx> kB
    Code
      bq_job_wait(job, quiet = FALSE)
    Message
      Job complete
      Input: <xxx> kB
      Output: <xxx> kB

# bq_perform_copy creates job that succeeds

    Code
      bq_job_wait(job, quiet = FALSE)
    Message
      Job complete

# bq_perform_load checks max_bad_records

    Code
      bq_perform_load("p.d.t", "gs://bucket/file.json", max_bad_records = -1)
    Condition
      Error in `bq_perform_load()`:
      ! `max_bad_records` must be a whole number larger than or equal to 0, not the number -1.

# bq_perform_query creates job that succeeds

    Code
      bq_job_wait(job, quiet = FALSE)
    Message
      Job complete
      Billed: 0 B

