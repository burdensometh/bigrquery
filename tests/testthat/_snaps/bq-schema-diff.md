# new required fields can't be added

    Code
      bq_schema_update_options(diff)
    Condition
      Error:
      ! Incoming data can't be safely appended to the target table:
      x New field `req` is REQUIRED; only NULLABLE or REPEATED fields can be added to an existing table.

# type changes are distinguished from legitimate additions

    Code
      bq_schema_update_options(diff)
    Condition
      Error:
      ! Incoming data can't be safely appended to the target table:
      x Field `id` changed type from INTEGER to FLOAT.
      i New field `extra` would be allowed on its own.

# relaxations require an explicit opt-in via relax_fields

    Code
      bq_schema_update_options(diff)
    Condition
      Error:
      ! Incoming data can't be safely appended to the target table:
      x Field `id` relaxes from REQUIRED to NULLABLE; list it in `relax_fields` to allow.

# other mode changes are rejected

    Code
      bq_schema_update_options(diff)
    Condition
      Error:
      ! Incoming data can't be safely appended to the target table:
      x Field `x` changed mode from REPEATED to NULLABLE.

# missing required fields are rejected

    Code
      bq_schema_update_options(diff)
    Condition
      Error:
      ! Incoming data can't be safely appended to the target table:
      x Required field `id` is missing from the incoming data.

# bq_schema_diff checks its inputs

    Code
      bq_schema_diff(target, target, relax_fields = 1)
    Condition
      Error in `bq_schema_diff()`:
      ! `relax_fields` must be a character vector or `NULL`, not the number 1.

# report output separates additions from problems

    Code
      print(bq_schema_diff(target, incoming))
    Output
      <bq_schema_diff>
      New fields (allowed):
        extra <BOOLEAN>
      Problems:
        Field `id` changed type from INTEGER to FLOAT.
        Required field `req` is missing from the incoming data.
      Compatible: no

# report output for a compatible diff

    Code
      print(bq_schema_diff(target, incoming, relax_fields = "id"))
    Output
      <bq_schema_diff>
      New fields (allowed):
        extra <STRING>
      Relaxations (allowed via `relax_fields`):
        id: REQUIRED -> NULLABLE
      Compatible: yes
      Schema update options: ALLOW_FIELD_ADDITION, ALLOW_FIELD_RELAXATION

# report output for identical schemas

    Code
      print(bq_schema_diff(target, target))
    Output
      <bq_schema_diff>
      Incoming data matches the target schema.
      Compatible: yes

