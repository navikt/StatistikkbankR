# Search SSB tables by keyword

Searches the SSB Statistikkbank table catalogue and returns a summary of
matching tables. Use this as the starting point when you do not yet know
the table identifier you need.

## Usage

``` r
ssb_search(
  query = NULL,
  language = "no",
  page = 1L,
  page_size = 20L,
  fetch_all = FALSE,
  as_tibble = TRUE
)
```

## Arguments

- query:

  Optional character. A keyword to search on. The search matches table
  titles. Pass `NULL` (the default) to list all available tables without
  filtering.

- language:

  Character. Language for table titles and labels. Use `"no"` for
  Norwegian (the default) or `"en"` for English.

- page:

  Integer. Which page of results to return. Defaults to `1` (the first
  page). SSB paginates search results; use `fetch_all = TRUE` if you
  want all pages at once.

- page_size:

  Integer. Number of tables per page. Must be between 1 and 1000.
  Defaults to `20`.

- fetch_all:

  Logical. If `TRUE`, all pages are fetched automatically and combined
  into a single result. Defaults to `FALSE`.

- as_tibble:

  Logical. If `TRUE` (the default), the result is returned as a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) (or
`data.frame` when `as_tibble = FALSE`) with one row per matching table.

## Details

Each row in the result describes one table and contains the following
columns:

- `id`:

  The table identifier to pass to
  [`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md).

- `label`:

  The full title of the table.

- `description`:

  A short description (may be `NA`).

- `updated`:

  Date and time the table was last updated.

- `first_period`, `last_period`:

  The earliest and latest time periods available.

- `category`:

  Thematic category (e.g. `"befolkning"`).

- `source`:

  The SSB unit that publishes the table.

- `variables`:

  Comma-separated dimension names, excluding the measure dimension which
  is present in every table.

The result also carries four attributes describing the current page:
`page_number`, `page_size`, `total_elements`, and `total_pages`. When
`fetch_all = TRUE` the attributes reflect the last fetched page.

## See also

[`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md)
to inspect the dimensions of a specific table.
[`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md)
to download data from a table.

## Examples

``` r
if (FALSE) { # \dontrun{
# Search for tables about population
ssb_search("befolkning")

# List all tables in English, 50 at a time
ssb_search(language = "en", page_size = 50)

# Retrieve all pages of results for a broad keyword
ssb_search("arbeid", fetch_all = TRUE)
} # }
```
