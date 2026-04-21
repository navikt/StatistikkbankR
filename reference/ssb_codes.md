# List valid codes for one dimension of an SSB table

Returns every code that is accepted as a filter value for one dimension
of the given table, together with the human-readable label for each
code. Use this before calling
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
to verify that the codes you want to filter on are valid.

## Usage

``` r
ssb_codes(
  table_id,
  dimension,
  language = "no",
  cache = TRUE,
  refresh_metadata = FALSE
)
```

## Arguments

- table_id:

  Character. The SSB table identifier, for example `"07459"`.

- dimension:

  Character. The dimension identifier, exactly as shown in the
  `dimension` column of
  [`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md).
  For example `"Region"` or `"Tid"`.

- language:

  Character. Language for labels. `"no"` (default) or `"en"`.

- cache:

  Logical. If `TRUE` (the default), table metadata is stored in memory
  for the session so that repeated calls for the same table are fast.

- refresh_metadata:

  Logical. If `TRUE`, discard any cached metadata and fetch fresh
  information from SSB. Defaults to `FALSE`.

## Value

A `data.frame` with four columns: `table_id`, `dimension`, `code` (the
value to use in a filter), and `label` (the human-readable name).

## See also

[`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md)
to list all dimensions first.
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
to download data using the codes as filters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Inspect all dimensions of table 07459
ssb_describe("07459")

# List every valid code for the Region dimension
ssb_codes("07459", "Region")

# Same in English
ssb_codes("07459", "Region", language = "en")
} # }
```
