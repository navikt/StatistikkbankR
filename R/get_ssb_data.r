#' Download data from Statistics Norway (SSB)
#'
#' @description
#' Downloads one or more variables from an SSB Statistikkbank table and
#' returns them as a tidy data frame.  Supply the table identifier and,
#' optionally, filters that narrow the download to the rows you need.
#' Use [ssb_search()] to find table identifiers and [ssb_describe()] to
#' inspect the dimensions and valid codes for a table before downloading.
#'
#' @param table_id Character.  The SSB table identifier, for example
#'   `"07459"`.  You can find identifiers by searching on
#'   \url{https://www.ssb.no/statbank} or by calling [ssb_search()].
#' @param ... Filters that limit which rows are downloaded.  Each filter
#'   is a named argument whose name is a dimension identifier (exactly as
#'   shown by [ssb_describe()]) and whose value is a character vector of
#'   the codes you want.  If you supply no filters the entire table is
#'   downloaded.
#'
#'   You may also use server-side selection expressions instead of
#'   listing codes one by one:
#'   \describe{
#'     \item{`"*"`}{All available values for that dimension.}
#'     \item{`"top(n)"`}{The \emph{n} most recent values (useful for time
#'       dimensions).}
#'     \item{`"bottom(n)"`}{The \emph{n} oldest values.}
#'     \item{`"from(code)"`}{All values from \emph{code} onwards.}
#'     \item{`"to(code)"`}{All values up to and including \emph{code}.}
#'     \item{`"range(a, b)"`}{All values between \emph{a} and \emph{b}.}
#'   }
#' @param language Character.  The language used for column and value
#'   labels in the result.  Use `"no"` for Norwegian (the default) or
#'   `"en"` for English.
#' @param as_tibble Logical.  If `TRUE` (the default), the result is
#'   returned as a [tibble][tibble::tibble].  Set to `FALSE` to get a
#'   plain `data.frame`.
#' @param override_large_query Logical.  SSB limits how many data cells
#'   a single request may return (typically 800,000).  If your query
#'   would exceed this limit, the function stops with an informative
#'   error.  Set `override_large_query = TRUE` to skip the check and send
#'   the request anyway.  Use with care on very large tables.
#' @param table_format Character.  Shape of the returned data frame.
#'   `"wide"` (the default) places each measure in its own column.
#'   `"long"` returns one row per value with separate `dimension`,
#'   `code`, `label`, and `value` columns, which is convenient for
#'   plotting with ggplot2 or for tables with many measures.
#' @param include_singleton_dims Logical.  Only used when
#'   `table_format = "long"`.  If `FALSE` (the default), dimensions that
#'   have only one selected value are dropped from the long-format output
#'   to keep it concise.  Set to `TRUE` to keep all dimensions
#'   regardless.
#' @param include_status Logical.  Some cells carry a status flag such as
#'   `"c"` (confidential) or `"."` (not applicable) instead of a numeric
#'   value.  If `TRUE`, a `status` column is added to the result
#'   containing these flags.  Defaults to `FALSE`.
#' @param character_as_factor Logical.  If `TRUE` (the default),
#'   dimension label columns are returned as factors, which preserves the
#'   ordering that SSB defines for the codes.  Set to `FALSE` to keep
#'   them as plain character strings.
#' @param quarter_as Character.  Controls how quarterly time codes are
#'   formatted in the result.  The default `"year_quarter"` converts
#'   codes such as `"2024K1"` to `"2024-Q1"`.  Use `"character"` to
#'   keep the original code string unchanged.
#' @param codelists Named list.  Applies an SSB aggregation grouping
#'   (codelist) to one or more dimensions.  This is how you request data
#'   at an aggregated geographic or administrative level — for example,
#'   using a summer municipality boundary instead of current municipality
#'   codes.  The name of each entry must match a dimension identifier and
#'   the value must be a codelist identifier.  Use [ssb_codelists()] to
#'   discover which codelists are available for a table.
#'   Example: `list(Region = "agg_KommSummer")`.
#' @param output_values Named list.  When a codelist groups codes
#'   together, this argument controls whether the result shows the
#'   aggregated group value (`"aggregated"`, one row per group) or all
#'   the individual member codes (`"single"`, one row per member).
#'   Each name must match a dimension that also appears in `codelists`.
#'   Example: `list(Region = "aggregated")`.
#' @param cache Logical.  If `TRUE` (the default), table metadata
#'   (dimension names, valid codes, etc.) is stored in memory for the
#'   duration of the R session.  This avoids repeated network round-trips
#'   when you call the function multiple times for the same table.
#' @param refresh_metadata Logical.  If `TRUE`, any cached metadata for
#'   this table is discarded and fresh metadata is fetched from SSB.
#'   Useful if you know the table definition has changed.  Defaults to
#'   `FALSE`.
#' @param max_get_query_chars Numeric.  Internal tuning parameter.  When
#'   the encoded filter string is shorter than this limit (in bytes) the
#'   request is sent as a standard GET request; when longer, it is sent
#'   as a POST request to avoid URL length restrictions.  The default
#'   value of `1500L` works for the vast majority of queries and rarely
#'   needs to be changed.
#' @param show_chunk_progress Logical.  Deprecated.  Has no effect and
#'   will be removed in a future version.
#'
#' @section Finding tables and dimensions:
#' Use [ssb_search()] to search table titles by keyword and
#' [ssb_describe()] to list the dimensions of a specific table together
#' with their roles and the number of available codes.  Once you know the
#' dimension identifiers, call [ssb_codes()] to list the exact code
#' strings that can be used as filter values.
#'
#' @section Filtering:
#' Filter arguments are matched by dimension identifier.  Identifiers are
#' case-sensitive and must be spelled exactly as returned by
#' [ssb_describe()].  If you omit a dimension, all of its codes are
#' included automatically.  You can mix explicit code vectors and
#' server-side expressions in the same call.
#'
#' @section Handling large tables:
#' SSB imposes a cell limit on each request.  The function estimates the
#' number of cells your query will return before sending it and stops
#' early with a clear message if the limit would be exceeded.  Strategies
#' for staying within the limit: supply filters to reduce the number of
#' rows, use `"top(n)"` on the time dimension, or split the download into
#' several calls with different filter values.  When the query is small
#' enough, the function sends it as a GET request; for larger but
#' still-valid queries it switches to a POST request automatically.
#'
#' @section Codelists and geographic aggregation:
#' SSB publishes aggregation codelists that map individual municipality
#' or region codes to higher-level groupings such as county or historical
#' boundary sets.  Pass the codelist identifier in `codelists` to use
#' one.  The `output_values` argument then controls whether you receive
#' the aggregated label for each group (`"aggregated"`) or each
#' individual member code (`"single"`).  See [ssb_codelists()] and
#' [ssb_get_by_codelist()] for discovery and convenience wrappers.
#'
#' @section Time formatting:
#' SSB encodes quarters in the form `"YYYYKn"` (e.g., `"2024K1"`).
#' With the default `quarter_as = "year_quarter"` these are reformatted
#' to `"YYYY-Qn"` (e.g., `"2024-Q1"`), which sorts and displays
#' correctly in most tools.  Set `quarter_as = "character"` to suppress
#' reformatting and keep the original SSB encoding.
#'
#' @return A [tibble][tibble::tibble] (or `data.frame` when
#'   `as_tibble = FALSE`) where each row is one observation.  In wide
#'   format, dimension label columns appear first followed by one numeric
#'   column per measure.  In long format there are four fixed columns:
#'   `dimension`, `code`, `label`, and `value`.  When
#'   `include_status = TRUE` an additional `status` column follows
#'   `value`.
#'
#' @seealso
#' [ssb_search()] to find table identifiers by keyword.
#' [ssb_describe()] to inspect dimensions before downloading.
#' [ssb_codes()] to list valid filter values for a dimension.
#' [ssb_codelists()] to discover available aggregation groupings.
#' [ssb_get_by_codelist()] as a shortcut that resolves codelist
#' dimension mapping automatically.
#'
#' @examples
#' \dontrun{
#' # --- 1. Download an entire small table --------------------------------
#' dat <- get_ssb_data("07459")
#'
#' # --- 2. Download with filters -----------------------------------------
#' # Use ssb_describe("07459") and ssb_codes("07459", "Region")
#' # to find valid dimension names and code values first.
#' dat <- get_ssb_data(
#'     "07459",
#'     Region = c("0301", "1103"),
#'     Tid    = c("2023", "2024")
#' )
#'
#' # --- 3. Server-side selection expressions -----------------------------
#' # "top(3)" on Tid keeps only the three most recent years.
#' # "*" on Kjonn selects all available sex categories.
#' dat <- get_ssb_data(
#'     "07459",
#'     Tid   = "top(3)",
#'     Kjonn = "*"
#' )
#'
#' # --- 4. Long format for plotting --------------------------------------
#' dat_long <- get_ssb_data(
#'     "07459",
#'     Tid        = "top(5)",
#'     table_format = "long"
#' )
#'
#' # --- 5. English labels ------------------------------------------------
#' dat_en <- get_ssb_data(
#'     "07459",
#'     Tid      = "top(3)",
#'     language = "en"
#' )
#'
#' # --- 6. Codelist: aggregated regions ----------------------------------
#' # Download population using historical summer municipality boundary.
#' # output_values = "aggregated" returns one row per boundary group.
#' dat_agg <- get_ssb_data(
#'     "07459",
#'     Tid          = "top(3)",
#'     codelists    = list(Region = "agg_KommSummer"),
#'     output_values = list(Region = "aggregated")
#' )
#'
#' # --- 7. Status flags --------------------------------------------------
#' # Keep the status column to see suppressed or missing cells.
#' dat_status <- get_ssb_data(
#'     "07459",
#'     Tid            = "top(3)",
#'     include_status = TRUE
#' )
#'
#' # --- 8. Quarter time values -------------------------------------------
#' # Table 12452 has quarterly time codes such as "2024K1".
#' # The default formats them as "2024-Q1".
#' dat_q <- get_ssb_data(
#'     "12452",
#'     Tid = "top(4)"
#' )
#' }
#'
#' @export

get_ssb_data <- function(
  table_id,
  ...,
  language = "no",
  as_tibble = TRUE,
  override_large_query = FALSE,
  table_format = "wide",
  include_singleton_dims = FALSE,
  include_status = FALSE,
  character_as_factor = TRUE,
  quarter_as = "year_quarter",
  codelists = NULL,
  output_values = NULL,
  cache = TRUE,
  refresh_metadata = FALSE,
  max_get_query_chars = 1500L,
  show_chunk_progress = FALSE
) {
  filters <- rlang::list2(...)

  .validate_main_args(
    table_id,
    language,
    as_tibble,
    override_large_query,
    table_format,
    include_singleton_dims,
    include_status,
    character_as_factor,
    quarter_as
  )
  .validate_metadata_cache_args(cache, refresh_metadata)
  .validate_chunking_args(max_get_query_chars, show_chunk_progress)

  .validate_filters(filters)
  .validate_codelists(codelists)
  .validate_output_values(output_values)

  normalised_filters <- .normalise_filters(filters)
  normalised_quarter_as <- .normalise_quarter_as(quarter_as)
  normalised_codelists <- .normalise_codelists(codelists)
  normalised_output_values <- .normalise_output_values(output_values)
  api_config <- .get_api_config(cache = cache, refresh_metadata = refresh_metadata)
  override_large_query_limit <- api_config$max_data_cells
  metadata <- .get_table_metadata(
    table_id,
    language,
    cache = cache,
    refresh_metadata = refresh_metadata
  )

  .validate_filter_names_against_metadata(normalised_filters, metadata)
  .validate_codelist_names_against_metadata(normalised_codelists, metadata)
  .validate_output_value_names_against_metadata(normalised_output_values, metadata)
  .validate_output_values_against_codelists(normalised_output_values, normalised_codelists)
  .validate_filter_values_against_metadata(normalised_filters, metadata, normalised_codelists)

  completed_filters <- .auto_complete_filters(metadata, normalised_filters, normalised_codelists)
  estimated_row_count <- .estimate_row_count(metadata, completed_filters)

  if (
    estimated_row_count > override_large_query_limit && !override_large_query
  ) {
    rlang::abort(paste0(
      "Query would return approximately ",
      estimated_row_count,
      " cells, ",
      "which exceeds the API-reported limit of ",
      override_large_query_limit,
      " cells",
      if (!is.na(api_config$max_calls_per_time_window) && !is.na(api_config$time_window)) {
        paste0(
          ".\nCurrent API rate limit: ",
          api_config$max_calls_per_time_window,
          " calls per ",
          api_config$time_window,
          " second(s)."
        )
      } else {
        "."
      },
      "\n",
      "Add `override_large_query = TRUE` to proceed anyway."
    ))
  }

  query_params <- .build_query_params(
    language,
    completed_filters,
    codelists = normalised_codelists,
    output_values = normalised_output_values
  )
  query_size <- .estimate_query_size(query_params)

  if (query_size > max_get_query_chars) {
    request_spec <- .build_post_request_spec(
      table_id,
      language,
      completed_filters,
      codelists = normalised_codelists,
      output_values = normalised_output_values
    )
    resp <- .execute_request_spec(request_spec)
    return(.tidy_response_json(
      .parse_response_json(resp),
      as_tibble,
      table_format,
      include_singleton_dims,
      metadata,
      include_status = include_status,
      character_as_factor = character_as_factor,
      quarter_as = normalised_quarter_as
    ))
  }

  request_spec <- .build_request_spec(
    table_id,
    language,
    completed_filters,
    codelists = normalised_codelists,
    output_values = normalised_output_values
  )
  resp <- .execute_request_spec(request_spec)

  .tidy_response_json(
    .parse_response_json(resp),
    as_tibble,
    table_format,
    include_singleton_dims,
    metadata,
    include_status = include_status,
    character_as_factor = character_as_factor,
    quarter_as = normalised_quarter_as
  )
}
