# Describe an SSB table and its dimensions

Returns a compact summary of a table's structure: its title, when it was
last updated, and a row for each dimension showing how many codes are
available and what role the dimension plays. This is the recommended
first call when working with a new table.

## Usage

``` r
ssb_describe(table_id, language = "no", cache = TRUE, refresh_metadata = FALSE)
```

## Arguments

- table_id:

  Character. The SSB table identifier, for example `"07459"`.

- language:

  Character. Language for labels. `"no"` (default) or `"en"`.

- cache:

  Logical. If `TRUE` (the default), table metadata is stored in memory
  for the session so that repeated calls are fast.

- refresh_metadata:

  Logical. If `TRUE`, discard any cached metadata and fetch fresh
  information from SSB. Defaults to `FALSE`.

## Value

A named list with elements `$table` and `$dimensions`. See Details for
the exact structure.

## Details

The function returns a named list with two elements:

- `$table`:

  A list with `table_id`, `table_label`, `updated` (ISO timestamp), and
  `full_table_estimated_rows` (the total number of cells if you were to
  download the whole table).

- `$dimensions`:

  A `data.frame` with one row per dimension, containing the dimension
  `id`, its `label`, its `role`, the number of available codes
  (`n_codes`), and a logical column `has_units` indicating whether unit
  metadata is available.

The `role` column classifies each dimension:

- `"time"`:

  The primary time dimension.

- `"geo"`:

  A geographic dimension such as municipality or county.

- `"contents"`:

  The measure dimension (what is being counted or measured).

- `"other"`:

  Any other classification variable.

## See also

[`ssb_search()`](https://navikt.github.io/StatistikkbankR/reference/ssb_search.md)
to find table identifiers.
[`ssb_codes()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codes.md)
to list valid filter values for a dimension.
[`ssb_units()`](https://navikt.github.io/StatistikkbankR/reference/ssb_units.md)
to inspect measurement units.

## Examples

``` r
if (FALSE) { # \dontrun{
desc <- ssb_describe("07459")

# Print the table-level summary
desc$table

# View the dimension list — use the 'dimension' column as
# the names of filter arguments in get_ssb_data()
desc$dimensions

# Same table with English labels
ssb_describe("07459", language = "en")
} # }
```
