# bq_table_load_staged reports the outcome

    Code
      bq_table_load_staged("p.d.target", "gs://bucket/file.json", "p.d.staging",
        max_bad_records = 2, quiet = FALSE)
    Message
      Published 98 rows to p.d.target (2 discarded).
    Output
      $published
      [1] TRUE
      
      $rows_total
      [1] 100
      
      $rows_loaded
      [1] 98
      
      $rows_discarded
      [1] 2
      
      $max_bad_records
      [1] 2
      
      $job
      <bq_job> p.job_1.US
      
      $staging
      <bq_table> p.d.staging
      

# bq_table_load_staged reports rejection

    Code
      bq_table_load_staged("p.d.target", "gs://bucket/file.json", "p.d.staging",
        max_bad_records = 2, quiet = FALSE)
    Message
      ! Load rejected: 5 bad records.
      i Limit is 2 bad records.
      i Staged results left in p.d.staging; see job p.job_1.US.
    Output
      $published
      [1] FALSE
      
      $rows_total
      [1] 100
      
      $rows_loaded
      [1] 95
      
      $rows_discarded
      [1] 5
      
      $max_bad_records
      [1] 2
      
      $job
      <bq_job> p.job_1.US
      
      $staging
      <bq_table> p.d.staging
      

# bq_table_load_staged checks its inputs

    Code
      bq_table_load_staged("p.d.t", "gs://bucket/file.json", "p.d.s",
        max_bad_records = -1)
    Condition
      Error in `bq_table_load_staged()`:
      ! `max_bad_records` must be a whole number larger than or equal to 0, not the number -1.

