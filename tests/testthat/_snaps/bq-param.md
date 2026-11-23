# parameter json doesn't change without notice

    Code
      as_bq_params(list(scalar = "a", vector = c("a", "b", "c")))
    Output
      [
        {
          "name": "scalar",
          "parameterType": {
            "type": "STRING"
          },
          "parameterValue": {
            "value": "a"
          }
        },
        {
          "name": "vector",
          "parameterType": {
            "type": "ARRAY",
            "arrayType": {
              "type": "STRING"
            }
          },
          "parameterValue": {
            "arrayValues": [
              {
                "value": "a"
              },
              {
                "value": "b"
              },
              {
                "value": "c"
              }
            ]
          }
        }
      ]

# checks inputs

    Code
      bq_param_scalar(1:3)
    Condition
      Error in `bq_param_scalar()`:
      ! `value` must be length 1, not 3.
    Code
      bq_param_array(integer())
    Condition
      Error in `bq_param_array()`:
      ! `value` can't be zero-length.

# a bare STRUCT type must describe its fields

    Code
      bq_param_scalar(1, "STRUCT")
    Condition
      Error in `bq_param_scalar()`:
      ! A bare "STRUCT" type doesn't describe its fields.
      i Use `bq_param_struct()` or `bq_param_array()` with a field specification.
    Code
      bq_param_array(1:2, "RECORD")
    Condition
      Error in `bq_param_array()`:
      ! A bare "STRUCT" type doesn't describe its fields.
      i Use `bq_param_struct()` or `bq_param_array()` with a field specification.

# unexpected, duplicate and empty fields are clear errors

    Code
      bq_param_array(data.frame(a = 1, b = 2), type = c(a = "STRING"))
    Condition
      Error in `bq_param_array()`:
      ! STRUCT row contains fields not declared in `type`: "b".
      i Declared fields are "a".
    Code
      bq_param_array(list(list(a = 1, a = 2)), type = c(a = "INTEGER"))
    Condition
      Error in `bq_param_array()`:
      ! STRUCT row field names must be unique; duplicated: "a".
    Code
      bq_param_array(data.frame(a = 1), type = c(a = "STRING", a = "FLOAT"))
    Condition
      Error in `bq_param_array()`:
      ! STRUCT field names must be unique; duplicated: "a".
    Code
      bq_param_array(data.frame(a = 1), type = c("STRING"))
    Condition
      Error in `bq_param_array()`:
      ! A field specification must be named, giving each field's type.
    Code
      bq_param_struct(list(), type = bq_fields(list()))
    Condition
      Error in `bq_param_struct()`:
      ! A STRUCT type must declare at least one field.

# invalid struct values are clear errors

    Code
      bq_param_struct(list(a = 1))
    Condition
      Error in `bq_param_struct()`:
      ! Must supply `type` when `value` is not a data frame.
      i Use a named vector of type strings, e.g. `c(market = "STRING")`.
    Code
      bq_param_struct(data.frame(a = 1:2), type = c(a = "INTEGER"))
    Condition
      Error in `bq_param_struct()`:
      ! A STRUCT row must have exactly one row, not 2.
    Code
      bq_param_array(1:3, type = c(a = "STRING"))
    Condition
      Error in `bq_param_array()`:
      ! The value of a STRUCT array must be a data frame or a list of rows.
      i Each row must be a named list or a one-row data frame.
    Code
      bq_param_array(list(1:3), type = c(a = "STRING"))
    Condition
      Error in `bq_param_array()`:
      ! Each STRUCT row must be a named list or a one-row data frame.
    Code
      bq_param_struct(list(a = 1:2), type = c(a = "INTEGER"))
    Condition
      Error in `bq_param_struct()`:
      ! Field "a" must be a single value, not length 2.
    Code
      bq_param_struct(list(a = list(1)), type = c(a = "INTEGER"))
    Condition
      Error in `bq_param_struct()`:
      ! Field "a" must be a single value, not a list.
    Code
      bq_param_struct(list(a = c(1L, NA)), type = bq_fields(list(bq_field("a",
        "INTEGER", mode = "REPEATED"))))
    Condition
      Error in `bq_param_struct()`:
      ! Repeated field "a" must not contain missing values.
      i Use `NULL` or a zero-length vector to send an empty array.

# struct parameter json doesn't change without notice

    Code
      as_bq_params(list(day = as.Date("2026-11-23"), roster = bq_param_array(roster,
        type = c(market = "STRING", threshold = "FLOAT"))))
    Output
      [
        {
          "name": "day",
          "parameterType": {
            "type": "DATE"
          },
          "parameterValue": {
            "value": "2026-11-23"
          }
        },
        {
          "name": "roster",
          "parameterType": {
            "type": "ARRAY",
            "arrayType": {
              "type": "STRUCT",
              "structTypes": [
                {
                  "name": "market",
                  "type": {
                    "type": "STRING"
                  }
                },
                {
                  "name": "threshold",
                  "type": {
                    "type": "FLOAT"
                  }
                }
              ]
            }
          },
          "parameterValue": {
            "arrayValues": [
              {
                "structValues": {
                  "market": {
                    "value": "US"
                  },
                  "threshold": {
                    "value": 10
                  }
                }
              },
              {
                "structValues": {
                  "market": {
                    "value": "GB"
                  },
                  "threshold": {
                    "value": 12.5
                  }
                }
              }
            ]
          }
        }
      ]

