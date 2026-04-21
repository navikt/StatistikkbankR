# List available codelists for an SSB table

Returns the aggregation codelists that SSB provides for each dimension
of a table. A codelist lets you request data at a different grouping
level without manually remapping codes — for example, using a historical
municipality boundary instead of current municipality codes. Use this to
discover which codelists exist before passing one to
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
via the `codelists` argument.

## Usage

``` r
ssb_codelists(
  table_id,
  dimension = NULL,
  language = "no",
  cache = TRUE,
  refresh_metadata = FALSE
)
```

## Arguments

- table_id:

  Character. The SSB table identifier.

- dimension:

  Optional character. If supplied, only codelists for that dimension are
  returned. Must match a dimension identifier exactly as shown by
  [`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md).

- language:

  Character. Language for labels. `"no"` (default) or `"en"`.

- cache:

  Logical. If `TRUE` (the default), table metadata is stored in memory
  for the session.

- refresh_metadata:

  Logical. If `TRUE`, discard any cached metadata and fetch fresh
  information from SSB. Defaults to `FALSE`.

## Value

A `data.frame` with one row per codelist, containing columns `table_id`,
`dimension`, `dimension_label`, `codelist_id`, `codelist_label`,
`codelist_type`, `supports_output_values`, `suggested_output_values`,
and `codelist_href`. If no codelists exist, an empty `data.frame` with
the same structure is returned.

## Details

The `supports_output_values` column indicates whether the codelist
supports the `output_values` argument in
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md).
When `TRUE`, you can pass `"aggregated"` to receive one row per group or
`"single"` to receive one row per underlying member code. The
`suggested_output_values` column shows the accepted values as a
convenience reminder.

## See also

[`ssb_codelist_details()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelist_details.md)
to inspect the individual codes inside a specific codelist.
[`ssb_get_by_codelist()`](https://navikt.github.io/StatistikkbankR/reference/ssb_get_by_codelist.md)
to download data using a codelist without specifying the dimension
manually.
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
for the `codelists` and `output_values` arguments.

## Examples

``` r
if (FALSE) { # \dontrun{
# List all codelists for table 07459
ssb_codelists("07459")

# Filter to a specific geographic dimension
ssb_codelists("07459", dimension = "Region")
} # }
```
