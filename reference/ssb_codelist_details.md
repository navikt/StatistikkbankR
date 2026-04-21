# Inspect the members of an SSB codelist

Downloads the full definition of a single codelist — the group codes it
defines and which underlying member codes each group contains. Use this
to understand exactly how a codelist maps individual codes to
higher-level groups before using it in
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md).

## Usage

``` r
ssb_codelist_details(codelist_id, language = "no", as_tibble = TRUE)
```

## Arguments

- codelist_id:

  Character. The codelist identifier, for example `"agg_KommSummer"`.
  Find available identifiers with
  [`ssb_codelists()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md).

- language:

  Character. Language for labels. `"no"` (default) or `"en"`.

- as_tibble:

  Logical. If `TRUE` (the default), the `$values` element of the
  returned list is a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Value

A named list with two elements:

- `$codelist`:

  A list with metadata about the codelist: `id`, `label`, `type` (e.g.
  `"Aggregation"`), `language`, `elimination` (logical), and
  `elimination_value_code`.

- `$values`:

  A `data.frame` or tibble with one row per group code, containing
  `code` (the group code), `label` (the group name), `mapped_values` (a
  comma-separated string of the member codes that belong to this group),
  and `mapped_count` (how many members the group contains).

## Details

A codelist groups individual codes (for example, municipality codes)
into aggregate units (for example, historical municipality boundaries or
counties). This function lets you inspect those groupings directly.

## See also

[`ssb_codelists()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md)
to list which codelists are available for a table.
[`ssb_expand_codelist_mapping()`](https://navikt.github.io/StatistikkbankR/reference/ssb_expand_codelist_mapping.md)
to join the member codes back onto a data frame you have already
downloaded.

## Examples

``` r
if (FALSE) { # \dontrun{
# Inspect the summer municipality boundary codelist
cl <- ssb_codelist_details("agg_KommSummer")

# See top-level metadata
cl$codelist

# View the group-to-member mapping
cl$values
} # }
```
