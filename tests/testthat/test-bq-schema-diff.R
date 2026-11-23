test_that("identical schemas are compatible and need no update options", {
  target <- as_bq_fields(data.frame(id = 1L, x = "a"))

  diff <- bq_schema_diff(target, target)
  expect_s3_class(diff, "bq_schema_diff")
  expect_equal(bq_schema_update_options(diff), character())
})

test_that("new nullable fields are legitimate additions", {
  target <- as_bq_fields(data.frame(id = 1L))
  incoming <- as_bq_fields(data.frame(id = 1L, extra = 1.5, note = "b"))

  diff <- bq_schema_diff(target, incoming)
  expect_equal(names(diff$added), c("extra", "note"))
  expect_equal(diff$added$extra, bq_field("extra", "float"))
  expect_equal(bq_schema_update_options(diff), "ALLOW_FIELD_ADDITION")
})

test_that("new required fields can't be added", {
  target <- as_bq_fields(data.frame(id = 1L))
  incoming <- bq_fields(list(
    bq_field("id", "integer"),
    bq_field("req", "string", mode = "REQUIRED")
  ))

  diff <- bq_schema_diff(target, incoming)
  expect_equal(names(diff$added_required), "req")
  expect_length(diff$added, 0)
  expect_snapshot(bq_schema_update_options(diff), error = TRUE)
})

test_that("type changes are distinguished from legitimate additions", {
  target <- as_bq_fields(data.frame(id = 1L, x = "a"))
  incoming <- as_bq_fields(data.frame(id = 1.5, x = "a", extra = TRUE))

  diff <- bq_schema_diff(target, incoming)
  expect_equal(diff$type_changed, list(id = list(from = "INTEGER", to = "FLOAT")))
  expect_equal(names(diff$added), "extra")
  expect_snapshot(bq_schema_update_options(diff), error = TRUE)
})

test_that("relaxations require an explicit opt-in via relax_fields", {
  target <- bq_fields(list(
    bq_field("id", "integer", mode = "REQUIRED"),
    bq_field("x", "string")
  ))
  incoming <- as_bq_fields(data.frame(id = 1L, x = "a"))

  diff <- bq_schema_diff(target, incoming)
  expect_equal(names(diff$relax_blocked), "id")
  expect_snapshot(bq_schema_update_options(diff), error = TRUE)

  diff <- bq_schema_diff(target, incoming, relax_fields = "id")
  expect_equal(diff$relaxed, list(id = list(from = "REQUIRED", to = "NULLABLE")))
  expect_equal(bq_schema_update_options(diff), "ALLOW_FIELD_RELAXATION")
})

test_that("stricter incoming modes append cleanly", {
  target <- as_bq_fields(data.frame(id = 1L))
  incoming <- bq_fields(list(bq_field("id", "integer", mode = "REQUIRED")))

  diff <- bq_schema_diff(target, incoming)
  expect_equal(bq_schema_update_options(diff), character())
})

test_that("other mode changes are rejected", {
  target <- bq_fields(list(bq_field("x", "string", mode = "REPEATED")))
  incoming <- as_bq_fields(data.frame(x = "a"))

  diff <- bq_schema_diff(target, incoming)
  expect_equal(diff$mode_changed, list(x = list(from = "REPEATED", to = "NULLABLE")))
  expect_snapshot(bq_schema_update_options(diff), error = TRUE)
})

test_that("missing required fields are rejected", {
  target <- bq_fields(list(
    bq_field("id", "integer", mode = "REQUIRED"),
    bq_field("x", "string")
  ))
  incoming <- as_bq_fields(data.frame(x = "a"))

  diff <- bq_schema_diff(target, incoming)
  expect_equal(diff$missing_required, "id")
  expect_snapshot(bq_schema_update_options(diff), error = TRUE)
})

test_that("missing nullable fields are fine", {
  target <- as_bq_fields(data.frame(id = 1L, x = "a"))
  incoming <- as_bq_fields(data.frame(id = 1L))

  diff <- bq_schema_diff(target, incoming)
  expect_equal(bq_schema_update_options(diff), character())
})

test_that("nested record fields are compared recursively", {
  target <- bq_fields(list(
    bq_field("id", "integer"),
    bq_field("rec", "record", fields = list(bq_field("a", "string")))
  ))
  incoming <- bq_fields(list(
    bq_field("id", "integer"),
    bq_field(
      "rec",
      "record",
      fields = list(bq_field("a", "string"), bq_field("b", "float"))
    )
  ))

  diff <- bq_schema_diff(target, incoming)
  expect_equal(names(diff$added), "rec.b")
  expect_equal(bq_schema_update_options(diff), "ALLOW_FIELD_ADDITION")
})

test_that("nested type changes are reported with dotted paths", {
  target <- bq_fields(list(
    bq_field("rec", "record", fields = list(bq_field("a", "string")))
  ))
  incoming <- bq_fields(list(
    bq_field("rec", "record", fields = list(bq_field("a", "integer")))
  ))

  diff <- bq_schema_diff(target, incoming)
  expect_equal(
    diff$type_changed,
    list(rec.a = list(from = "STRING", to = "INTEGER"))
  )
})

test_that("additions and relaxations combine in the update options", {
  target <- bq_fields(list(bq_field("id", "integer", mode = "REQUIRED")))
  incoming <- bq_fields(list(bq_field("id", "integer"), bq_field("extra", "string")))

  diff <- bq_schema_diff(target, incoming, relax_fields = "id")
  expect_equal(
    bq_schema_update_options(diff),
    c("ALLOW_FIELD_ADDITION", "ALLOW_FIELD_RELAXATION")
  )
})

test_that("merged schema applies relaxations and appends additions", {
  target <- bq_fields(list(
    bq_field("id", "integer", mode = "REQUIRED"),
    bq_field("rec", "record", fields = list(bq_field("a", "string")))
  ))
  incoming <- bq_fields(list(
    bq_field("id", "integer"),
    bq_field(
      "rec",
      "record",
      fields = list(bq_field("a", "string"), bq_field("b", "float"))
    ),
    bq_field("extra", "string")
  ))

  diff <- bq_schema_diff(target, incoming, relax_fields = "id")
  merged <- bq_fields_merge(target, diff)

  expect_s3_class(merged, "bq_fields")
  expect_equal(vapply(merged, `[[`, "", "name"), c("id", "rec", "extra"))
  expect_equal(merged[[1]]$mode, "NULLABLE")
  expect_equal(vapply(merged[[2]]$fields, `[[`, "", "name"), c("a", "b"))
  expect_equal(merged[[3]], bq_field("extra", "string"))
})

test_that("merged schema is unchanged when there's nothing to merge", {
  target <- as_bq_fields(data.frame(id = 1L, x = "a"))
  diff <- bq_schema_diff(target, target)

  expect_equal(bq_fields_merge(target, diff), target)
})

test_that("bq_schema_diff checks its inputs", {
  target <- as_bq_fields(data.frame(id = 1L))
  expect_snapshot(
    bq_schema_diff(target, target, relax_fields = 1),
    error = TRUE
  )
})

test_that("report output separates additions from problems", {
  target <- bq_fields(list(
    bq_field("id", "integer"),
    bq_field("req", "string", mode = "REQUIRED")
  ))
  incoming <- bq_fields(list(
    bq_field("id", "float"),
    bq_field("extra", "boolean")
  ))

  expect_snapshot(print(bq_schema_diff(target, incoming)))
})

test_that("report output for a compatible diff", {
  target <- bq_fields(list(bq_field("id", "integer", mode = "REQUIRED")))
  incoming <- bq_fields(list(bq_field("id", "integer"), bq_field("extra", "string")))

  expect_snapshot(print(bq_schema_diff(target, incoming, relax_fields = "id")))
})

test_that("report output for identical schemas", {
  target <- as_bq_fields(data.frame(id = 1L))
  expect_snapshot(print(bq_schema_diff(target, target)))
})
