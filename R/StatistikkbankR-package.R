#' StatistikkbankR: Access Statistikkbank Data from Statistics Norway
#'
#' @description
#' Statistics Norway (SSB) publishes a wide range of official statistics
#' through its Statistikkbank API.  This package provides a simple and
#' consistent interface for searching the table catalogue, inspecting
#' table structure, and downloading data directly into R as tidy
#' `data.frame` or [tibble][tibble::tibble] objects.
#'
#' The main entry points are:
#' \describe{
#'   \item{[ssb_search()]}{Find tables by keyword.}
#'   \item{[ssb_describe()]}{Inspect the dimensions and structure of a
#'     specific table.}
#'   \item{[ssb_codes()]}{List valid filter codes for one dimension.}
#'   \item{[get_ssb_data()]}{Download data from a table, with optional
#'     filters, codelist aggregation, and wide or long output.}
#' }
#'
#' Additional helpers \link{ssb_units}, \link{ssb_codelists},
#' \link{ssb_codelist_details}, \link{ssb_expand_codelist_mapping}, and
#' \link{ssb_get_by_codelist} support working with measurement units and
#' geographic aggregation codelists.
#'
#' @seealso
#' The SSB Statistikkbank web interface:
#' <https://www.ssb.no/statbank>
#'
#' The underlying PXWeb API documentation:
#' <https://data.ssb.no/api/pxwebapi/v2/>
#'
#' @importFrom stats setNames
#' @importFrom utils head
#' @keywords internal
"_PACKAGE"
