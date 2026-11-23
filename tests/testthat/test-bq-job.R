test_that("bq_job_load_stats extracts load statistics", {
  local_mocked_bindings(
    bq_job_meta = function(x, fields = NULL) {
      list(statistics = list(load = list(
        inputFiles = "2",
        inputFileBytes = "1000",
        outputRows = "90",
        outputBytes = "900",
        badRecords = "10"
      )))
    }
  )

  stats <- bq_job_load_stats(bq_job("p", "j"))
  expect_equal(
    stats,
    list(
      input_files = 2,
      input_bytes = 1000,
      output_rows = 90,
      output_bytes = 900,
      bad_records = 10
    )
  )
})

test_that("bq_job_load_stats treats missing values as 0", {
  local_mocked_bindings(
    bq_job_meta = function(x, fields = NULL) {
      list(statistics = list(load = list(outputRows = "5")))
    }
  )

  stats <- bq_job_load_stats(bq_job("p", "j"))
  expect_equal(stats$output_rows, 5)
  expect_equal(stats$bad_records, 0)
})

test_that("bq_job_load_stats errors for jobs without load statistics", {
  local_mocked_bindings(
    bq_job_meta = function(x, fields = NULL) {
      list(statistics = list(query = list()))
    }
  )

  expect_snapshot(bq_job_load_stats(bq_job("p", "j")), error = TRUE)
})

test_that("bq_job_show_statistics reveals discarded bad records", {
  local_mocked_bindings(
    bq_job_meta = function(x, fields = NULL) {
      list(statistics = list(load = list(
        inputFileBytes = "100",
        outputBytes = "90",
        badRecords = "3"
      )))
    }
  )

  expect_snapshot(bq_job_show_statistics(bq_job("p", "j")))
})

test_that("can control chattiness of bq_job_wait", {
  withr::local_options(cli.progress_show_after = Inf)
  job <- bq_perform_query("SELECT 1 + 1", bq_test_project())

  expect_snapshot({
    bq_job_wait(job, quiet = TRUE)
    bq_job_wait(job, quiet = FALSE)
  })
})

test_that("informative errors on failure", {
  withr::local_options(cli.progress_show_after = Inf)
  ds <- bq_test_dataset()

  tb <- bq_test_table()
  bq_table_create(
    tb,
    fields = list(bq_field("x", "integer"), bq_field("y", "string"))
  )

  expect_snapshot(
    {
      "One error"
      bq_dataset_query(ds, "SELECT 1 +")

      "Multiple errors"
      bq_table_upload(tb, data.frame(x = "x", y = 1:5))
    },
    error = TRUE,
    transform = function(x) gsub("Job (.*?) failed", "Job <bq_job> failed", x)
  )
})
