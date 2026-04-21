# StatistikkbankR: Access Statistikkbank Data from Statistics Norway

Statistics Norway (SSB) publishes a wide range of official statistics
through its Statistikkbank API. This package provides a simple and
consistent interface for searching the table catalogue, inspecting table
structure, and downloading data directly into R as tidy `data.frame` or
[tibble](https://tibble.tidyverse.org/reference/tibble.html) objects.

The main entry points are:

- [`ssb_search()`](https://navikt.github.io/StatistikkbankR/reference/ssb_search.md):

  Find tables by keyword.

- [`ssb_describe()`](https://navikt.github.io/StatistikkbankR/reference/ssb_describe.md):

  Inspect the dimensions and structure of a specific table.

- [`ssb_codes()`](https://navikt.github.io/StatistikkbankR/reference/ssb_codes.md):

  List valid filter codes for one dimension.

- [`get_ssb_data()`](https://navikt.github.io/StatistikkbankR/reference/get_ssb_data.md):

  Download data from a table, with optional filters, codelist
  aggregation, and wide or long output.

Additional helpers
[ssb_units](https://navikt.github.io/StatistikkbankR/reference/ssb_units.md),
[ssb_codelists](https://navikt.github.io/StatistikkbankR/reference/ssb_codelists.md),
[ssb_codelist_details](https://navikt.github.io/StatistikkbankR/reference/ssb_codelist_details.md),
[ssb_expand_codelist_mapping](https://navikt.github.io/StatistikkbankR/reference/ssb_expand_codelist_mapping.md),
and
[ssb_get_by_codelist](https://navikt.github.io/StatistikkbankR/reference/ssb_get_by_codelist.md)
support working with measurement units and geographic aggregation
codelists.

## See also

The SSB Statistikkbank web interface: <https://www.ssb.no/statbank>

The underlying PXWeb API documentation:
<https://data.ssb.no/api/pxwebapi/v2/>

## Author

**Maintainer**: Tobias Lunde <tobias.lunde@nav.no>
