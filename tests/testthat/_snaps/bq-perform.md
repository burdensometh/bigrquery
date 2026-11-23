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

# check_recovered_load_job rejects a non-load job

    Code
      check_recovered_load_job(NULL, job_id = "job_1", x = x, source_format = "NEWLINE_DELIMITED_JSON",
        create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
    Condition
      Error:
      ! A job with id "job_1" already exists, but it isn't a load job.
      i Use a different `job_id` for each batch, reusing an id only to retry the same batch.

# check_recovered_load_job rejects a different destination

    Code
      check_recovered_load_job(load, job_id = "job_1", x = x, source_format = "NEWLINE_DELIMITED_JSON",
        create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
    Condition
      Error:
      ! A job with id "job_1" already exists, but it loads into a different table.
      i The existing job loads into "project.dataset.other_table".
      i This batch targets "project.dataset.table".
      i Use a different `job_id` for each batch, reusing an id only to retry the same batch.

# check_recovered_load_job rejects a different load configuration

    Code
      check_recovered_load_job(load, job_id = "job_1", x = x, source_format = "NEWLINE_DELIMITED_JSON",
        create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
    Condition
      Error:
      ! A job with id "job_1" already exists, but its load configuration doesn't match this batch.
      x source format: existing job uses PARQUET, but this batch uses NEWLINE_DELIMITED_JSON
      x write disposition: existing job uses WRITE_TRUNCATE, but this batch uses WRITE_APPEND
      i Use a different `job_id` for each batch, reusing an id only to retry the same batch.

# bq_perform_copy creates job that succeeds

    Code
      bq_job_wait(job, quiet = FALSE)
    Message
      Job complete

# bq_perform_query creates job that succeeds

    Code
      bq_job_wait(job, quiet = FALSE)
    Message
      Job complete
      Billed: 0 B

