# Download data from Statistics Norway (SSB)

Downloads one or more variables from an SSB Statistikkbank table and
returns them as a tidy data frame. Supply the table identifier and,
optionally, filters that narrow the download to the rows you need. Use
[`ssb_search()`](https://navikt.github.io/StatistikkbankR/reference/ssb_search.md)
to find table identifiers and
[`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md)
to inspect the dimensions and valid codes for a table before
downloading.

## Usage

``` r
get_ssb_data(
  table_id,
  ...,
  language = "no",
  as_tibble = TRUE,
  override_large_query = FALSE,
  table_format = "wide",
  include_singleton_dims = FALSE,
  include_status = FALSE,
  character_as_factor = TRUE,
  convert_quarter_to_yearqtr = TRUE,
  convert_month_to_yearmon = TRUE,
  codelists = NULL,
  output_values = NULL,
  cache = TRUE,
  refresh_metadata = FALSE,
  max_get_query_chars = 1500L,
  show_chunk_progress = FALSE
)
```

## Arguments

- table_id:

  Character. The SSB table identifier, for example `"07459"`. You can
  find identifiers by searching on <https://www.ssb.no/statbank> or by
  calling
  [`ssb_search()`](https://navikt.github.io/StatistikkbankR/reference/ssb_search.md).

- ...:

  Filters that limit which rows are downloaded. Each filter is a named
  argument whose name is a dimension identifier (exactly as shown by
  [`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md))
  and whose value is a character vector of the codes you want. If you
  supply no filters the entire table is downloaded.

  You may also use server-side selection expressions instead of listing
  codes one by one:

  `"*"`

  :   All available values for that dimension.

  `"top(n)"`

  :   The *n* most recent values (useful for time dimensions).

  `"bottom(n)"`

  :   The *n* oldest values.

  `"from(code)"`

  :   All values from *code* onwards.

  `"to(code)"`

  :   All values up to and including *code*.

  `"range(a, b)"`

  :   All values between *a* and *b*.

- language:

  Character. The language used for column and value labels in the
  result. Use `"no"` for Norwegian (the default) or `"en"` for English.

- as_tibble:

  Logical. If `TRUE` (the default), the result is returned as a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html). Set to
  `FALSE` to get a plain `data.frame`.

- override_large_query:

  Logical. SSB limits how many data cells a single request may return
  (typically 800,000). If your query would exceed this limit, the
  function stops with an informative error. Set
  `override_large_query = TRUE` to skip the check and send the request
  anyway. Use with care on very large tables.

- table_format:

  Character. Shape of the returned data frame. `"wide"` (the default)
  places each measure in its own column. `"long"` returns one row per
  value with separate `dimension`, `code`, `label`, and `value` columns,
  which is convenient for plotting with ggplot2 or for tables with many
  measures.

- include_singleton_dims:

  Logical. Only used when `table_format = "long"`. If `FALSE` (the
  default), dimensions that have only one selected value are dropped
  from the long-format output to keep it concise. Set to `TRUE` to keep
  all dimensions regardless.

- include_status:

  Logical. Some cells carry a status flag such as `"c"` (confidential)
  or `"."` (not applicable) instead of a numeric value. If `TRUE`, a
  `status` column is added to the result containing these flags.
  Defaults to `FALSE`.

- character_as_factor:

  Logical. If `TRUE` (the default), dimension label columns are returned
  as factors, which preserves the ordering that SSB defines for the
  codes. Set to `FALSE` to keep them as plain character strings.

- convert_quarter_to_yearqtr:

  Logical. If `TRUE` (the default), quarterly time codes such as
  `"2024K1"` are converted to
  [zoo::yearqtr](https://rdrr.io/pkg/zoo/man/yearqtr.html) objects,
  which are suitable for time series analysis. Set to `FALSE` to keep
  the original SSB code strings.

- convert_month_to_yearmon:

  Logical. If `TRUE` (the default), monthly time codes such as
  `"2024M01"` are converted to
  [zoo::yearmon](https://rdrr.io/pkg/zoo/man/yearmon.html) objects,
  which are suitable for time series analysis. Set to `FALSE` to keep
  the original SSB code strings.

- codelists:

  Named list. Applies an SSB aggregation grouping (codelist) to one or
  more dimensions. This is how you request data at an aggregated
  geographic or administrative level — for example, using a summer
  municipality boundary instead of current municipality codes. The name
  of each entry must match a dimension identifier and the value must be
  a codelist identifier. Use
  [`ssb_codelists()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md)
  to discover which codelists are available for a table. Example:
  `list(Region = "agg_KommSummer")`.

- output_values:

  Named list. When a codelist groups codes together, this argument
  controls whether the result shows the aggregated group value
  (`"aggregated"`, one row per group) or all the individual member codes
  (`"single"`, one row per member). Each name must match a dimension
  that also appears in `codelists`. Example:
  `list(Region = "aggregated")`.

- cache:

  Logical. If `TRUE` (the default), table metadata (dimension names,
  valid codes, etc.) is stored in memory for the duration of the R
  session. This avoids repeated network round-trips when you call the
  function multiple times for the same table.

- refresh_metadata:

  Logical. If `TRUE`, any cached metadata for this table is discarded
  and fresh metadata is fetched from SSB. Useful if you know the table
  definition has changed. Defaults to `FALSE`.

- max_get_query_chars:

  Numeric. Internal tuning parameter. When the encoded filter string is
  shorter than this limit (in bytes) the request is sent as a standard
  GET request; when longer, it is sent as a POST request to avoid URL
  length restrictions. The default value of `1500L` works for the vast
  majority of queries and rarely needs to be changed.

- show_chunk_progress:

  Logical. Deprecated. Has no effect and will be removed in a future
  version.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) (or
`data.frame` when `as_tibble = FALSE`) where each row is one
observation. In wide format, dimension label columns appear first
followed by one numeric column per measure. In long format there are
four fixed columns: `dimension`, `code`, `label`, and `value`. When
`include_status = TRUE` an additional `status` column follows `value`.

## Finding tables and dimensions

Use
[`ssb_search()`](https://navikt.github.io/StatistikkbankR/reference/ssb_search.md)
to search table titles by keyword and
[`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md)
to list the dimensions of a specific table together with their roles and
the number of available codes. Once you know the dimension identifiers,
call
[`ssb_codes()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codes.md)
to list the exact code strings that can be used as filter values.

## Filtering

Filter arguments are matched by dimension identifier. Identifiers are
case-sensitive and must be spelled exactly as returned by
[`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md).
If you omit a dimension, all of its codes are included automatically.
You can mix explicit code vectors and server-side expressions in the
same call.

## Handling large tables

SSB imposes a cell limit on each request. The function estimates the
number of cells your query will return before sending it and stops early
with a clear message if the limit would be exceeded. Strategies for
staying within the limit: supply filters to reduce the number of rows,
use `"top(n)"` on the time dimension, or split the download into several
calls with different filter values. When the query is small enough, the
function sends it as a GET request; for larger but still-valid queries
it switches to a POST request automatically.

## Codelists and geographic aggregation

SSB publishes aggregation codelists that map individual municipality or
region codes to higher-level groupings such as county or historical
boundary sets. Pass the codelist identifier in `codelists` to use one.
The `output_values` argument then controls whether you receive the
aggregated label for each group (`"aggregated"`) or each individual
member code (`"single"`). See
[`ssb_codelists()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md)
and
[`ssb_get_by_codelist()`](https://navikt.github.io/StatistikkbankR/reference/ssb_get_by_codelist.md)
for discovery and convenience wrappers.

## Time formatting

SSB encodes quarters in the form `"YYYYKn"` (e.g., `"2024K1"`) and
months in the form `"YYYYMmm"` (e.g., `"2024M01"`). By default,
quarterly codes are converted to
[zoo::yearqtr](https://rdrr.io/pkg/zoo/man/yearqtr.html) and monthly
codes are converted to
[zoo::yearmon](https://rdrr.io/pkg/zoo/man/yearmon.html). Set
`convert_quarter_to_yearqtr = FALSE` and/or
`convert_month_to_yearmon = FALSE` to keep original SSB code strings.

## See also

[`ssb_search()`](https://navikt.github.io/StatistikkbankR/reference/ssb_search.md)
to find table identifiers by keyword.
[`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md)
to inspect dimensions before downloading.
[`ssb_codes()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codes.md)
to list valid filter values for a dimension.
[`ssb_codelists()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md)
to discover available aggregation groupings.
[`ssb_get_by_codelist()`](https://navikt.github.io/StatistikkbankR/reference/ssb_get_by_codelist.md)
as a shortcut that resolves codelist dimension mapping automatically.

## Examples

``` r
if (FALSE) { # \dontrun{
# --- 1. Download an entire small table --------------------------------
dat <- get_ssb_data("07459")

# --- 2. Download with filters -----------------------------------------
# Use ssb_describe("07459") and ssb_codes("07459", "Region")
# to find valid dimension names and code values first.
dat <- get_ssb_data(
    "07459",
    Region = c("0301", "1103"),
    Tid    = c("2023", "2024")
)

# --- 3. Server-side selection expressions -----------------------------
# "top(3)" on Tid keeps only the three most recent years.
# "*" on Kjonn selects all available sex categories.
dat <- get_ssb_data(
    "07459",
    Tid   = "top(3)",
    Kjonn = "*"
)

# --- 4. Long format for plotting --------------------------------------
dat_long <- get_ssb_data(
    "07459",
    Tid        = "top(5)",
    table_format = "long"
)

# --- 5. English labels ------------------------------------------------
dat_en <- get_ssb_data(
    "07459",
    Tid      = "top(3)",
    language = "en"
)

# --- 6. Codelist: aggregated regions ----------------------------------
# Download population using historical summer municipality boundary.
# output_values = "aggregated" returns one row per boundary group.
dat_agg <- get_ssb_data(
    "07459",
    Tid          = "top(3)",
    codelists    = list(Region = "agg_KommSummer"),
    output_values = list(Region = "aggregated")
)

# --- 7. Status flags --------------------------------------------------
# Keep the status column to see suppressed or missing cells.
dat_status <- get_ssb_data(
    "07459",
    Tid            = "top(3)",
    include_status = TRUE
)

# --- 8. Quarterly time values as zoo::yearqtr ---------------------------
# Table 12452 has quarterly time codes such as "2024K1".
# By default, these are converted to zoo::yearqtr objects.
dat_q <- get_ssb_data(
    "12452",
    Tid = "top(4)"
)

# --- 9. Monthly time values as zoo::yearmon ---------------------------
# Table 13966 has monthly time codes such as "2024M01".
# By default, these are converted to zoo::yearmon objects.
# dat_m <- get_ssb_data(
#     "13966",
#     Tid = "top(4)"
# )
} # }
```
