# List measurement units for an SSB table

Returns unit information (such as `"persons"`, `"NOK"`, or `"per cent"`)
for the codes in a table's measure dimension. Most tables expose units
on the contents/measure dimension; geographic and time dimensions
typically have none.

## Usage

``` r
ssb_units(
  table_id,
  dimension = NULL,
  language = "no",
  cache = TRUE,
  refresh_metadata = FALSE
)
```

## Arguments

- table_id:

  Character. The SSB table identifier, for example `"07459"`.

- dimension:

  Optional character. The dimension identifier to filter results to a
  single dimension. If `NULL` (the default), units for all dimensions
  are returned.

- language:

  Character. Language for labels. `"no"` (default) or `"en"`.

- cache:

  Logical. If `TRUE` (the default), table metadata is stored in memory
  for the session.

- refresh_metadata:

  Logical. If `TRUE`, discard any cached metadata and fetch fresh
  information from SSB. Defaults to `FALSE`.

## Value

A `data.frame` with columns `table_id`, `dimension`, `dimension_label`,
`code`, `label`, `unit_base` (the unit name, e.g. `"persons"`), and
`unit_decimals` (the number of decimal places SSB recommends for
display).

## Details

Not every code in a measure dimension has a unit; codes without unit
metadata are excluded from the result. If a table has no unit metadata
at all, an empty `data.frame` with the correct column structure is
returned rather than an error.

## See also

[`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md)
to check whether a dimension has units before calling this function (the
`has_units` column).

## Examples

``` r
if (FALSE) { # \dontrun{
# List all units for table 07459
ssb_units("07459")

# Filter to the contents dimension only
ssb_units("07459", dimension = "ContentsCode")
} # }
```
