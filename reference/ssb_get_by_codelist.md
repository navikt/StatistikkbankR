# Download SSB data using a codelist without specifying the dimension

A convenience wrapper around
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
that automatically looks up which dimension a codelist belongs to and
applies it for you. Use this instead of
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
when you know the codelist identifier you want to use but do not want to
manually identify the matching dimension.

## Usage

``` r
ssb_get_by_codelist(
  table_id,
  codelist_id,
  ...,
  dimension = NULL,
  output_value = NULL,
  language = "no",
  as_tibble = TRUE,
  override_large_query = FALSE,
  table_format = "wide",
  include_singleton_dims = FALSE,
  cache = TRUE,
  refresh_metadata = FALSE,
  max_get_query_chars = 1500L,
  show_chunk_progress = FALSE
)
```

## Arguments

- table_id:

  Character. The SSB table identifier, for example `"07459"`.

- codelist_id:

  Character. The codelist identifier to apply, for example
  `"agg_KommSummer"`. Use
  [`ssb_codelists()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md)
  to find available identifiers for a table.

- ...:

  Filters for table dimensions, passed directly to
  [`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md).
  Each filter is a named argument whose name is a dimension identifier
  and whose value is a character vector of codes or a server-side
  expression such as `"top(3)"`.

- dimension:

  Optional character. Only needed if the same codelist identifier is
  used on more than one dimension in the table. Specify the dimension
  identifier explicitly to resolve the ambiguity.

- output_value:

  Optional character. Controls how the codelist dimension appears in the
  result. `"aggregated"` (default when supported) returns one row per
  group defined in the codelist. `"single"` returns one row per
  underlying member code. Leave `NULL` to use the SSB default. Only
  applicable when the codelist supports output values (see
  `supports_output_values` in
  [`ssb_codelists()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md)).

- language:

  Character. Language for labels. `"no"` (default) or `"en"`.

- as_tibble:

  Logical. If `TRUE` (the default), the result is returned as a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html).

- override_large_query:

  Logical. If `TRUE`, bypass the cell limit guard. See
  [`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
  for details.

- table_format:

  Character. `"wide"` (default) or `"long"`. See
  [`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
  for details.

- include_singleton_dims:

  Logical. Only used with `table_format = "long"`. See
  [`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
  for details.

- cache:

  Logical. If `TRUE` (the default), table metadata is cached in memory
  for the session.

- refresh_metadata:

  Logical. If `TRUE`, discard cached metadata and re-fetch from SSB.
  Defaults to `FALSE`.

- max_get_query_chars:

  Numeric. Internal tuning parameter; see
  [`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
  for details. Defaults to `1500L`.

- show_chunk_progress:

  Logical. Deprecated. Has no effect.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) (or
`data.frame` when `as_tibble = FALSE`) as described in
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md).

## Details

The function calls
[`ssb_codelists()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md)
internally to confirm that the codelist identifier exists for the table
and to determine which dimension it belongs to. It then calls
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
with the correct `codelists` and `output_values` arguments filled in
automatically.

If the codelist identifier is present on more than one dimension, the
function stops with a message listing the affected dimensions so you can
pass the correct one via `dimension`.

## See also

[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
for the full set of download options.
[`ssb_codelists()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md)
to discover available codelists for a table.
[`ssb_codelist_details()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelist_details.md)
to inspect the group-to-member mapping.
[`ssb_expand_codelist_mapping()`](https://navikt.github.io/StatistikkbankR/reference/ssb_expand_codelist_mapping.md)
to expand aggregated result rows to individual member codes after
downloading.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download population aggregated by summer municipality boundary.
# The function detects that "agg_KommSummer" belongs to Region.
pop <- ssb_get_by_codelist(
    "07459",
    "agg_KommSummer",
    Tid          = "top(3)",
    output_value = "aggregated"
)

# Request individual member codes instead of aggregated groups
pop_single <- ssb_get_by_codelist(
    "07459",
    "agg_KommSummer",
    Tid          = "top(3)",
    output_value = "single"
)

# When the codelist exists on multiple dimensions, specify which one
pop_explicit <- ssb_get_by_codelist(
    "07459",
    "agg_KommSummer",
    Tid          = "top(3)",
    dimension    = "Region",
    output_value = "aggregated"
)
} # }
```
