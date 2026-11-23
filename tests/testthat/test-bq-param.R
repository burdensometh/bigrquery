test_that("can create parameters from list", {
  x <- list(a = bq_param(1, "integer"), b = "x", c = 1:3)
  p <- as_bq_params(x)

  expect_length(p, 3)
  expect_equal(p[[1]], bq_param_scalar(1, "integer", name = "a"))
  expect_equal(p[[2]], bq_param_scalar("x", name = "b"))
  expect_equal(p[[3]], bq_param_array(1:3, name = "c"))
})

test_that("parameter json doesn't change without notice", {
  expect_snapshot({
    as_bq_params(list(
      scalar = "a",
      vector = c("a", "b", "c")
    ))
  })
})

test_that("checks inputs", {
  expect_snapshot(error = TRUE, {
    bq_param_scalar(1:3)
    bq_param_array(integer())
  })
})

test_that("explicit scalar types are uppercased", {
  expect_equal(bq_param_scalar(1, "int64")$type, "INT64")
  expect_equal(bq_param_array(1:2, "float64")$type, "FLOAT64")
  expect_equal(bq_param_scalar(TRUE, "bool")$type, "BOOL")
})

test_that("a bare STRUCT type must describe its fields", {
  expect_snapshot(error = TRUE, {
    bq_param_scalar(1, "STRUCT")
    bq_param_array(1:2, "RECORD")
  })
})

test_that("struct arrays accept a typed data frame", {
  roster <- data.frame(
    market = c("US", "GB"),
    threshold = c(10, 12.5)
  )
  p <- bq_param_array(
    roster,
    type = c(market = "STRING", threshold = "FLOAT"),
    name = "roster"
  )

  expect_s3_class(p, "bq_param_array")
  expect_equal(
    as_json(p),
    list(
      name = "roster",
      parameterType = list(
        type = "ARRAY",
        arrayType = list(
          type = unbox("STRUCT"),
          structTypes = list(
            list(name = unbox("market"), type = list(type = unbox("STRING"))),
            list(name = unbox("threshold"), type = list(type = unbox("FLOAT")))
          )
        )
      ),
      parameterValue = list(
        arrayValues = list(
          list(
            structValues = list(
              market = list(value = unbox("US")),
              threshold = list(value = unbox(10))
            )
          ),
          list(
            structValues = list(
              market = list(value = unbox("GB")),
              threshold = list(value = unbox(12.5))
            )
          )
        )
      )
    )
  )
})

test_that("btq_param() detects data frames", {
  roster <- data.frame(market = "US", threshold = 10)
  p <- bq_param(roster)

  expect_s3_class(p, "bq_param_array")
  expect_equal(p$type[[1]]$name, "market")
})

test_that("struct array field types can be inferred from a data frame", {
  roster <- data.frame(
    market = "US",
    threshold = 10,
    day = as.Date("2026-11-23")
  )
  p <- bq_param_array(roster)

  types <- vapply(p$type, function(field) field$type, character(1))
  names(types) <- vapply(p$type, function(field) field$name, character(1))
  expect_equal(types, c(market = "STRING", threshold = "FLOAT", day = "DATE"))
})

test_that("struct type can be a bq_fields object or a list of fields", {
  fields <- bq_fields(list(
    bq_field("market", "string"),
    bq_field("threshold", "float")
  ))
  roster <- data.frame(market = "US", threshold = 10)

  expect_equal(bq_param_array(roster, type = fields)$type, fields)
  expect_equal(
    bq_param_array(roster, type = unclass(fields))$type,
    fields
  )
})

test_that("an empty roster sends an empty array with a declared type", {
  p <- bq_param_array(
    data.frame(market = character(), threshold = numeric()),
    type = c(market = "STRING", threshold = "FLOAT"),
    name = "roster"
  )

  expect_equal(as_json(p)$parameterValue, list(arrayValues = list()))
})

test_that("an empty roster can be a zero-length list", {
  p <- bq_param_array(list(), type = c(market = "STRING"), name = "roster")

  expect_equal(as_json(p)$parameterValue, list(arrayValues = list()))
})

test_that("an empty roster can be a zero-length atomic vector or NULL", {
  type <- c(market = "STRING")

  expect_equal(
    as_json(bq_param_array(
      character(),
      type = type,
      name = "r"
    ))$parameterValue,
    list(arrayValues = list())
  )
  expect_equal(
    as_json(bq_param_array(NULL, type = type, name = "r"))$parameterValue,
    list(arrayValues = list())
  )
})

test_that("bq_param() routes STRUCT types to the right constructor", {
  row <- bq_param(list(market = "US"), type = c(market = "STRING"))
  roster <- bq_param(data.frame(market = "US"), type = c(market = "STRING"))

  expect_s3_class(row, "bq_param_struct")
  expect_s3_class(roster, "bq_param_array")
})


test_that("missing and NA struct fields become SQL NULL", {
  p <- bq_param_struct(
    list(market = "US", threshold = NA),
    type = c(market = "STRING", threshold = "FLOAT"),
    name = "row"
  )

  expect_equal(
    as_json(p)$parameterValue,
    list(
      structValues = list(
        market = list(value = unbox("US")),
        threshold = list(value = unbox(NA))
      )
    )
  )
})

test_that("a declared but absent field is sent as NULL", {
  p <- bq_param_struct(
    list(market = "US"),
    type = c(market = "STRING", threshold = "FLOAT"),
    name = "row"
  )

  expect_equal(
    as_json(p)$parameterValue$structValues$threshold,
    list(value = unbox(NA))
  )
})

test_that("an empty row sends every declared field as NULL", {
  p <- bq_param_struct(list(), type = c(market = "STRING"), name = "row")

  expect_equal(
    as_json(p)$parameterValue$structValues$market,
    list(value = unbox(NA))
  )
})

test_that("struct rows can be a list of one-row data frames", {
  rows <- list(
    data.frame(market = "US", threshold = 10),
    data.frame(market = "GB", threshold = 12.5)
  )
  p <- bq_param_array(rows, type = c(market = "STRING", threshold = "FLOAT"))

  expect_equal(
    vapply(as_json(p)$parameterValue$arrayValues, length, integer(1)),
    c(1L, 1L)
  )
})

test_that("nested structs and repeated fields are supported", {
  type <- bq_fields(list(
    bq_field("market", "STRING"),
    bq_field(
      "meta",
      "RECORD",
      fields = bq_fields(list(
        bq_field("lo", "FLOAT"),
        bq_field("hi", "FLOAT")
      ))
    ),
    bq_field("days", "INTEGER", mode = "REPEATED")
  ))
  p <- bq_param_struct(
    list(market = "US", meta = list(lo = 1, hi = 2), days = c(1L, 2L)),
    type = type,
    name = "row"
  )

  expect_equal(
    as_json(p)$parameterType$structTypes[[2]]$type$structTypes,
    list(
      list(name = unbox("lo"), type = list(type = unbox("FLOAT"))),
      list(name = unbox("hi"), type = list(type = unbox("FLOAT")))
    )
  )
  expect_equal(
    as_json(p)$parameterValue$structValues$days,
    list(arrayValues = list(list(value = unbox(1L)), list(value = unbox(2L))))
  )
})

test_that("a NULL nested struct is sent as an empty object", {
  type <- bq_fields(list(
    bq_field("market", "STRING"),
    bq_field(
      "meta",
      "RECORD",
      fields = bq_fields(list(bq_field("lo", "FLOAT")))
    )
  ))
  p <- bq_param_struct(list(market = "US"), type = type, name = "row")

  expect_equal(
    as_json(p)$parameterValue$structValues$meta,
    structure(list(), names = character())
  )
})

test_that("unexpected, duplicate and empty fields are clear errors", {
  expect_snapshot(error = TRUE, {
    bq_param_array(data.frame(a = 1, b = 2), type = c(a = "STRING"))
    bq_param_array(list(list(a = 1, a = 2)), type = c(a = "INTEGER"))
    bq_param_array(data.frame(a = 1), type = c(a = "STRING", a = "FLOAT"))
    bq_param_array(data.frame(a = 1), type = c("STRING"))
    bq_param_struct(list(), type = bq_fields(list()))
  })
})

test_that("invalid struct values are clear errors", {
  expect_snapshot(error = TRUE, {
    bq_param_struct(list(a = 1))
    bq_param_struct(data.frame(a = 1:2), type = c(a = "INTEGER"))
    bq_param_array(1:3, type = c(a = "STRING"))
    bq_param_array(list(1:3), type = c(a = "STRING"))
    bq_param_struct(list(a = 1:2), type = c(a = "INTEGER"))
    bq_param_struct(list(a = list(1)), type = c(a = "INTEGER"))
    bq_param_struct(
      list(a = c(1L, NA)),
      type = bq_fields(list(bq_field("a", "INTEGER", mode = "REPEATED")))
    )
  })
})

test_that("struct parameter json doesn't change without notice", {
  roster <- data.frame(
    market = c("US", "GB"),
    threshold = c(10, 12.5)
  )
  expect_snapshot({
    as_bq_params(list(
      day = as.Date("2026-11-23"),
      roster = bq_param_array(
        roster,
        type = c(market = "STRING", threshold = "FLOAT")
      )
    ))
  })
})

test_that("struct parameter values keep full double precision", {
  p <- bq_param_struct(
    list(threshold = 0.001234567),
    type = c(threshold = "FLOAT")
  )
  json <- jsonlite::toJSON(as_json(p), auto_unbox = TRUE, digits = 22L)

  expect_match(json, "0.001234567", fixed = TRUE)
})
