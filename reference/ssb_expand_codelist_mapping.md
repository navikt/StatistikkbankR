# Expand grouped codelist codes to individual member codes

Takes a data frame that contains grouped codelist codes (for example,
the aggregated region codes returned when using
`output_values = "aggregated"` in
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md))
and expands each row into one row per individual member code. This makes
it easy to join SSB aggregate data onto a dataset that uses the
underlying individual codes.

## Usage

``` r
ssb_expand_codelist_mapping(
  data,
  code_col,
  codelist_id,
  language = "no",
  member_col = "member_code",
  keep_unmatched = TRUE,
  as_tibble = TRUE
)
```

## Arguments

- data:

  A `data.frame` or tibble that contains a column of grouped codelist
  codes.

- code_col:

  Character. The name of the column in `data` that holds the grouped
  codes, for example `"Region"`.

- codelist_id:

  Character. The codelist identifier that defines the grouping, for
  example `"agg_KommSummer"`.

- language:

  Character. Language used when fetching codelist details. `"no"`
  (default) or `"en"`.

- member_col:

  Character. The name of the new column to add to the result that will
  contain the expanded member codes. Defaults to `"member_code"`. Must
  not already exist in `data`.

- keep_unmatched:

  Logical. If `TRUE` (the default), rows whose grouped code is not found
  in the codelist are kept in the result with `NA` in `member_col`. If
  `FALSE`, those rows are silently dropped.

- as_tibble:

  Logical. If `TRUE` (the default), the result is returned as a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) (or
`data.frame` when `as_tibble = FALSE`) with the same columns as `data`
plus one new column named by `member_col`. The number of rows may be
larger than the input when groups contain multiple members.

## Details

Each row in `data` may expand into multiple rows if its grouped code
maps to several member codes. All original columns are duplicated
accordingly. If a grouped code maps to no members and
`keep_unmatched = TRUE`, the row is kept with `NA` in `member_col`; with
`keep_unmatched = FALSE` it is dropped.

## See also

[`ssb_codelist_details()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelist_details.md)
to inspect the codelist mapping.
[`ssb_codelists()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md)
to discover available codelists for a table.
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
for the `codelists` and `output_values` arguments that produce the
grouped codes this function expands.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download population aggregated by summer municipality boundary
pop_agg <- get_ssb_data(
    "07459",
    Tid           = "top(1)",
    codelists     = list(Region = "agg_KommSummer"),
    output_values = list(Region = "aggregated")
)

# Expand each group row to one row per member municipality code
pop_expanded <- ssb_expand_codelist_mapping(
    data        = pop_agg,
    code_col    = "Region",
    codelist_id = "agg_KommSummer"
)

# Rows whose group code has no members are kept with NA
# by default; set keep_unmatched = FALSE to drop them
pop_expanded_clean <- ssb_expand_codelist_mapping(
    data            = pop_agg,
    code_col        = "Region",
    codelist_id     = "agg_KommSummer",
    keep_unmatched  = FALSE
)
} # }
```
