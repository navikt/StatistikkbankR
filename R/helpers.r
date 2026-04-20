.flatten_dimension_units <- function(metadata, dimension = NULL) {
  if (!is.null(dimension)) {
    if (!dimension %in% names(metadata$dimensions)) {
      rlang::abort(
        paste0(
          "Unknown dimension '",
          dimension,
          "'. Valid dimensions are: ",
          paste(names(metadata$dimensions), collapse = ", "),
          "."
        )
      )
    }
    dimension_ids <- dimension
  } else {
    dimension_ids <- names(metadata$dimensions)
  }

  rows <- list()

  for (dimension_id in dimension_ids) {
    dimension_meta <- metadata$dimensions[[dimension_id]]
    units_df <- dimension_meta$units

    if (is.null(units_df) || nrow(units_df) == 0) {
      next
    }

    code_match <- match(units_df$code, dimension_meta$codes)
    labels <- dimension_meta$labels[code_match]

    rows[[length(rows) + 1L]] <- data.frame(
      table_id = metadata$table_id,
      dimension = dimension_id,
      dimension_label = dimension_meta$label,
      code = units_df$code,
      label = labels,
      unit_base = units_df$unit_base,
      unit_decimals = units_df$unit_decimals,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    return(data.frame(
      table_id = character(0),
      dimension = character(0),
      dimension_label = character(0),
      code = character(0),
      label = character(0),
      unit_base = character(0),
      unit_decimals = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.flatten_dimension_codelists <- function(metadata, dimension = NULL) {
  if (!is.null(dimension)) {
    if (!dimension %in% names(metadata$dimensions)) {
      rlang::abort(
        paste0(
          "Unknown dimension '",
          dimension,
          "'. Valid dimensions are: ",
          paste(names(metadata$dimensions), collapse = ", "),
          "."
        )
      )
    }
    dimension_ids <- dimension
  } else {
    dimension_ids <- names(metadata$dimensions)
  }

  rows <- list()

  for (dimension_id in dimension_ids) {
    dimension_meta <- metadata$dimensions[[dimension_id]]
    dimension_codelists <- dimension_meta$codelists

    if (is.null(dimension_codelists) || length(dimension_codelists) == 0) {
      next
    }

    for (codelist in dimension_codelists) {
      codelist_type <- as.character(codelist$type)
      supports_output_values <- identical(codelist_type, "Aggregation")

      rows[[length(rows) + 1L]] <- data.frame(
        table_id = metadata$table_id,
        dimension = dimension_id,
        dimension_label = dimension_meta$label,
        codelist_id = as.character(codelist$id),
        codelist_label = as.character(codelist$label),
        codelist_type = codelist_type,
        supports_output_values = supports_output_values,
        suggested_output_values = if (supports_output_values) "aggregated,single" else NA_character_,
        codelist_href = as.character(codelist$href),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      table_id = character(0),
      dimension = character(0),
      dimension_label = character(0),
      codelist_id = character(0),
      codelist_label = character(0),
      codelist_type = character(0),
      supports_output_values = logical(0),
      suggested_output_values = character(0),
      codelist_href = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Search SSB tables by keyword
#'
#' @description
#' Searches the SSB Statistikkbank table catalogue and returns a summary
#' of matching tables.  Use this as the starting point when you do not
#' yet know the table identifier you need.
#'
#' @param query Optional character.  A keyword to search on.  The search
#'   matches table titles.  Pass `NULL` (the default) to list all
#'   available tables without filtering.
#' @param language Character.  Language for table titles and labels.
#'   Use `"no"` for Norwegian (the default) or `"en"` for English.
#' @param page Integer.  Which page of results to return.  Defaults to
#'   `1` (the first page).  SSB paginates search results; use
#'   `fetch_all = TRUE` if you want all pages at once.
#' @param page_size Integer.  Number of tables per page.  Must be
#'   between 1 and 1000.  Defaults to `20`.
#' @param fetch_all Logical.  If `TRUE`, all pages are fetched
#'   automatically and combined into a single result.  Defaults to
#'   `FALSE`.
#' @param as_tibble Logical.  If `TRUE` (the default), the result is
#'   returned as a [tibble][tibble::tibble].
#'
#' @details
#' Each row in the result describes one table and contains the following
#' columns:
#' \describe{
#'   \item{`id`}{The table identifier to pass to [get_ssb_data()].}
#'   \item{`label`}{The full title of the table.}
#'   \item{`description`}{A short description (may be `NA`).}
#'   \item{`updated`}{Date and time the table was last updated.}
#'   \item{`first_period`, `last_period`}{The earliest and latest time
#'     periods available.}
#'   \item{`category`}{Thematic category (e.g. `"befolkning"`).
#'     }
#'   \item{`source`}{The SSB unit that publishes the table.}
#'   \item{`variables`}{Comma-separated dimension names, excluding the
#'     measure dimension which is present in every table.}
#' }
#'
#' The result also carries four attributes describing the current page:
#' `page_number`, `page_size`, `total_elements`, and `total_pages`.
#' When `fetch_all = TRUE` the attributes reflect the last fetched page.
#'
#' @return A [tibble][tibble::tibble] (or `data.frame` when
#'   `as_tibble = FALSE`) with one row per matching table.
#'
#' @seealso
#' [ssb_describe()] to inspect the dimensions of a specific table.
#' [get_ssb_data()] to download data from a table.
#'
#' @examples
#' \dontrun{
#' # Search for tables about population
#' ssb_search("befolkning")
#'
#' # List all tables in English, 50 at a time
#' ssb_search(language = "en", page_size = 50)
#'
#' # Retrieve all pages of results for a broad keyword
#' ssb_search("arbeid", fetch_all = TRUE)
#' }
#' @export
ssb_search <- function(
  query = NULL,
  language = "no",
  page = 1L,
  page_size = 20L,
  fetch_all = FALSE,
  as_tibble = TRUE
) {
  if (!is.null(query) && (!rlang::is_string(query) || trimws(query) == "")) {
    rlang::abort("`query` must be NULL or a single non-empty character string.")
  }

  if (!language %in% c("no", "en")) {
    rlang::abort("`language` must be one of \"no\" or \"en\".")
  }

  page <- as.integer(page)
  page_size <- as.integer(page_size)

  if (is.na(page) || page < 1L) {
    rlang::abort("`page` must be a positive integer.")
  }

  if (is.na(page_size) || page_size < 1L || page_size > 1000L) {
    rlang::abort("`page_size` must be an integer between 1 and 1000.")
  }

  if (!rlang::is_bool(fetch_all)) {
    rlang::abort("`fetch_all` must be TRUE or FALSE.")
  }

  if (!rlang::is_bool(as_tibble)) {
    rlang::abort("`as_tibble` must be TRUE or FALSE.")
  }

  request_spec <- .build_tables_request_spec(query, language, page, page_size)
  resp <- .execute_request_spec(request_spec)
  parsed <- .parse_response_json(resp)
  result <- .parse_tables_response(parsed)

  all_data <- result$data
  last_page_info <- result$page_info

  if (fetch_all && last_page_info$total_pages > page) {
    remaining_pages <- seq(page + 1L, last_page_info$total_pages)
    for (p in remaining_pages) {
      req_spec <- .build_tables_request_spec(query, language, p, page_size)
      pg_resp <- .execute_request_spec(req_spec)
      pg_parsed <- .parse_response_json(pg_resp)
      pg_result <- .parse_tables_response(pg_parsed)
      all_data <- rbind(all_data, pg_result$data)
      last_page_info <- pg_result$page_info
    }
  }

  rownames(all_data) <- NULL
  attr(all_data, "page_number") <- last_page_info$page_number
  attr(all_data, "page_size") <- last_page_info$page_size
  attr(all_data, "total_elements") <- last_page_info$total_elements
  attr(all_data, "total_pages") <- last_page_info$total_pages

  if (as_tibble) {
    return(dplyr::as_tibble(all_data))
  }

  all_data
}

#' List valid codes for one dimension of an SSB table
#'
#' @description
#' Returns every code that is accepted as a filter value for one
#' dimension of the given table, together with the human-readable label
#' for each code.  Use this before calling [get_ssb_data()] to verify
#' that the codes you want to filter on are valid.
#'
#' @param table_id Character.  The SSB table identifier, for example
#'   `"07459"`.
#' @param dimension Character.  The dimension identifier, exactly as
#'   shown in the `dimension` column of [ssb_describe()].  For example
#'   `"Region"` or `"Tid"`.
#' @param language Character.  Language for labels.  `"no"` (default)
#'   or `"en"`.
#' @param cache Logical.  If `TRUE` (the default), table metadata is
#'   stored in memory for the session so that repeated calls for the
#'   same table are fast.
#' @param refresh_metadata Logical.  If `TRUE`, discard any cached
#'   metadata and fetch fresh information from SSB.  Defaults to
#'   `FALSE`.
#'
#' @return A `data.frame` with four columns: `table_id`, `dimension`,
#'   `code` (the value to use in a filter), and `label` (the
#'   human-readable name).
#'
#' @seealso
#' [ssb_describe()] to list all dimensions first.
#' [get_ssb_data()] to download data using the codes as filters.
#'
#' @examples
#' \dontrun{
#' # Inspect all dimensions of table 07459
#' ssb_describe("07459")
#'
#' # List every valid code for the Region dimension
#' ssb_codes("07459", "Region")
#'
#' # Same in English
#' ssb_codes("07459", "Region", language = "en")
#' }
#' @export
#'
ssb_codes <- function(
  table_id,
  dimension,
  language = "no",
  cache = TRUE,
  refresh_metadata = FALSE
) {
  .validate_table_request_args(table_id, language)
  .validate_metadata_cache_args(cache, refresh_metadata)

  metadata <- .get_table_metadata(
    table_id,
    language,
    cache = cache,
    refresh_metadata = refresh_metadata
  )

  if (!dimension %in% names(metadata$dimensions)) {
    rlang::abort(
      paste0(
        "Unknown dimension '",
        dimension,
        "'. Valid dimensions are: ",
        paste(names(metadata$dimensions), collapse = ", "),
        "."
      )
    )
  }

  codes <- metadata$dimensions[[dimension]]$codes
  labels <- metadata$dimensions[[dimension]]$labels

  data.frame(
    table_id = table_id,
    dimension = dimension,
    code = codes,
    label = labels,
    stringsAsFactors = FALSE
  )
}

#' Describe an SSB table and its dimensions
#'
#' @description
#' Returns a compact summary of a table's structure: its title, when it
#' was last updated, and a row for each dimension showing how many codes
#' are available and what role the dimension plays.  This is the
#' recommended first call when working with a new table.
#'
#' @param table_id Character.  The SSB table identifier, for example
#'   `"07459"`.
#' @param language Character.  Language for labels.  `"no"` (default)
#'   or `"en"`.
#' @param cache Logical.  If `TRUE` (the default), table metadata is
#'   stored in memory for the session so that repeated calls are fast.
#' @param refresh_metadata Logical.  If `TRUE`, discard any cached
#'   metadata and fetch fresh information from SSB.  Defaults to
#'   `FALSE`.
#'
#' @details
#' The function returns a named list with two elements:
#' \describe{
#'   \item{`$table`}{A list with `table_id`, `table_label`, `updated`
#'     (ISO timestamp), and `full_table_estimated_rows` (the total
#'     number of cells if you were to download the whole table).}
#'   \item{`$dimensions`}{A `data.frame` with one row per dimension,
#'     containing the dimension `id`, its `label`, its `role`, the
#'     number of available codes (`n_codes`), and a logical column
#'     `has_units` indicating whether unit metadata is available.}
#' }
#'
#' The `role` column classifies each dimension:
#' \describe{
#'   \item{`"time"`}{The primary time dimension.}
#'   \item{`"geo"`}{A geographic dimension such as municipality or
#'     county.}
#'   \item{`"contents"`}{The measure dimension (what is being counted
#'     or measured).}
#'   \item{`"other"`}{Any other classification variable.}
#' }
#'
#' @return A named list with elements `$table` and `$dimensions`.
#'   See Details for the exact structure.
#'
#' @seealso
#' [ssb_search()] to find table identifiers.
#' [ssb_codes()] to list valid filter values for a dimension.
#' [ssb_units()] to inspect measurement units.
#'
#' @examples
#' \dontrun{
#' desc <- ssb_describe("07459")
#'
#' # Print the table-level summary
#' desc$table
#'
#' # View the dimension list — use the 'dimension' column as
#' # the names of filter arguments in get_ssb_data()
#' desc$dimensions
#'
#' # Same table with English labels
#' ssb_describe("07459", language = "en")
#' }
#' @export
ssb_describe <- function(
  table_id,
  language = "no",
  cache = TRUE,
  refresh_metadata = FALSE
) {
  .validate_table_request_args(table_id, language)
  .validate_metadata_cache_args(cache, refresh_metadata)

  metadata <- .get_table_metadata(
    table_id,
    language,
    cache = cache,
    refresh_metadata = refresh_metadata
  )

  dimension_ids <- names(metadata$dimensions)

  role_map <- setNames(rep("other", length(dimension_ids)), dimension_ids)
  if (!is.null(metadata$roles) && length(metadata$roles) > 0) {
    for (role_name in names(metadata$roles)) {
      role_dims <- metadata$roles[[role_name]]
      role_dims <- as.character(unlist(role_dims, use.names = FALSE))
      role_map[intersect(role_dims, names(role_map))] <- role_name
    }
  }

  dimensions_df <- data.frame(
    dimension = dimension_ids,
    label = vapply(metadata$dimensions, function(x) x$label, character(1)),
    role = unname(role_map[dimension_ids]),
    n_codes = vapply(metadata$dimensions, function(x) x$code_count, integer(1)),
    has_units = vapply(metadata$dimensions, function(x) {
      !is.null(x$units) && nrow(x$units) > 0
    }, logical(1)),
    stringsAsFactors = FALSE
  )

  list(
    table = list(
      table_id = metadata$table_id,
      table_label = metadata$table_label,
      updated = metadata$updated,
      full_table_estimated_rows = metadata$full_table_estimated_rows
    ),
    dimensions = dimensions_df
  )
}

#' List measurement units for an SSB table
#'
#' @description
#' Returns unit information (such as `"persons"`, `"NOK"`, or
#' `"per cent"`) for the codes in a table's measure dimension.  Most
#' tables expose units on the contents/measure dimension; geographic and
#' time dimensions typically have none.
#'
#' @param table_id Character.  The SSB table identifier, for example
#'   `"07459"`.
#' @param dimension Optional character.  The dimension identifier to
#'   filter results to a single dimension.  If `NULL` (the default),
#'   units for all dimensions are returned.
#' @param language Character.  Language for labels.  `"no"` (default)
#'   or `"en"`.
#' @param cache Logical.  If `TRUE` (the default), table metadata is
#'   stored in memory for the session.
#' @param refresh_metadata Logical.  If `TRUE`, discard any cached
#'   metadata and fetch fresh information from SSB.  Defaults to
#'   `FALSE`.
#'
#' @details
#' Not every code in a measure dimension has a unit; codes without
#' unit metadata are excluded from the result.  If a table has no unit
#' metadata at all, an empty `data.frame` with the correct column
#' structure is returned rather than an error.
#'
#' @return A `data.frame` with columns `table_id`, `dimension`,
#'   `dimension_label`, `code`, `label`, `unit_base` (the unit name,
#'   e.g. `"persons"`), and `unit_decimals` (the number of decimal
#'   places SSB recommends for display).
#'
#' @seealso
#' [ssb_describe()] to check whether a dimension has units before
#' calling this function (the `has_units` column).
#'
#' @examples
#' \dontrun{
#' # List all units for table 07459
#' ssb_units("07459")
#'
#' # Filter to the contents dimension only
#' ssb_units("07459", dimension = "ContentsCode")
#' }
#' @export
ssb_units <- function(
  table_id,
  dimension = NULL,
  language = "no",
  cache = TRUE,
  refresh_metadata = FALSE
) {
  .validate_table_request_args(table_id, language)
  .validate_metadata_cache_args(cache, refresh_metadata)

  if (!is.null(dimension) && (!rlang::is_string(dimension) || dimension == "")) {
    rlang::abort("`dimension` must be NULL or a single non-empty character string.")
  }

  metadata <- .get_table_metadata(
    table_id,
    language,
    cache = cache,
    refresh_metadata = refresh_metadata
  )

  .flatten_dimension_units(metadata, dimension = dimension)
}

#' List available codelists for an SSB table
#'
#' @description
#' Returns the aggregation codelists that SSB provides for each
#' dimension of a table.  A codelist lets you request data at a
#' different grouping level without manually remapping codes — for
#' example, using a historical municipality boundary instead of current
#' municipality codes.  Use this to discover which codelists exist
#' before passing one to [get_ssb_data()] via the `codelists` argument.
#'
#' @param table_id Character.  The SSB table identifier.
#' @param dimension Optional character.  If supplied, only codelists for
#'   that dimension are returned.  Must match a dimension identifier
#'   exactly as shown by [ssb_describe()].
#' @param language Character.  Language for labels.  `"no"` (default)
#'   or `"en"`.
#' @param cache Logical.  If `TRUE` (the default), table metadata is
#'   stored in memory for the session.
#' @param refresh_metadata Logical.  If `TRUE`, discard any cached
#'   metadata and fetch fresh information from SSB.  Defaults to
#'   `FALSE`.
#'
#' @details
#' The `supports_output_values` column indicates whether the codelist
#' supports the `output_values` argument in [get_ssb_data()].  When
#' `TRUE`, you can pass `"aggregated"` to receive one row per group or
#' `"single"` to receive one row per underlying member code.  The
#' `suggested_output_values` column shows the accepted values as a
#' convenience reminder.
#'
#' @return A `data.frame` with one row per codelist, containing columns
#'   `table_id`, `dimension`, `dimension_label`, `codelist_id`,
#'   `codelist_label`, `codelist_type`, `supports_output_values`,
#'   `suggested_output_values`, and `codelist_href`.
#'   If no codelists exist, an empty `data.frame` with the same
#'   structure is returned.
#'
#' @seealso
#' [ssb_codelist_details()] to inspect the individual codes inside a
#' specific codelist.
#' [ssb_get_by_codelist()] to download data using a codelist without
#' specifying the dimension manually.
#' [get_ssb_data()] for the `codelists` and `output_values` arguments.
#'
#' @examples
#' \dontrun{
#' # List all codelists for table 07459
#' ssb_codelists("07459")
#'
#' # Filter to a specific geographic dimension
#' ssb_codelists("07459", dimension = "Region")
#' }
#' @export
ssb_codelists <- function(
  table_id,
  dimension = NULL,
  language = "no",
  cache = TRUE,
  refresh_metadata = FALSE
) {
  .validate_table_request_args(table_id, language)
  .validate_metadata_cache_args(cache, refresh_metadata)

  if (!is.null(dimension) && (!rlang::is_string(dimension) || dimension == "")) {
    rlang::abort("`dimension` must be NULL or a single non-empty character string.")
  }

  metadata <- .get_table_metadata(
    table_id,
    language,
    cache = cache,
    refresh_metadata = refresh_metadata
  )

  .flatten_dimension_codelists(metadata, dimension = dimension)
}

#' Inspect the members of an SSB codelist
#'
#' @description
#' Downloads the full definition of a single codelist — the group codes
#' it defines and which underlying member codes each group contains.
#' Use this to understand exactly how a codelist maps individual codes
#' to higher-level groups before using it in [get_ssb_data()].
#'
#' @param codelist_id Character.  The codelist identifier, for example
#'   `"agg_KommSummer"`.  Find available identifiers with
#'   [ssb_codelists()].
#' @param language Character.  Language for labels.  `"no"` (default)
#'   or `"en"`.
#' @param as_tibble Logical.  If `TRUE` (the default), the `$values`
#'   element of the returned list is a [tibble][tibble::tibble].
#'
#' @details
#' A codelist groups individual codes (for example, municipality codes)
#' into aggregate units (for example, historical municipality boundaries
#' or counties).  This function lets you inspect those groupings
#' directly.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{`$codelist`}{A list with metadata about the codelist:
#'     `id`, `label`, `type` (e.g. `"Aggregation"`), `language`,
#'     `elimination` (logical), and `elimination_value_code`.}
#'   \item{`$values`}{A `data.frame` or tibble with one row per group
#'     code, containing `code` (the group code), `label` (the group
#'     name), `mapped_values` (a comma-separated string of the member
#'     codes that belong to this group), and `mapped_count` (how many
#'     members the group contains).}
#' }
#'
#' @seealso
#' [ssb_codelists()] to list which codelists are available for a table.
#' [ssb_expand_codelist_mapping()] to join the member codes back onto
#' a data frame you have already downloaded.
#'
#' @examples
#' \dontrun{
#' # Inspect the summer municipality boundary codelist
#' cl <- ssb_codelist_details("agg_KommSummer")
#'
#' # See top-level metadata
#' cl$codelist
#'
#' # View the group-to-member mapping
#' cl$values
#' }
#' @export
ssb_codelist_details <- function(codelist_id, language = "no", as_tibble = TRUE) {
  if (!rlang::is_string(codelist_id) || trimws(codelist_id) == "") {
    rlang::abort("`codelist_id` must be a single non-empty character string.")
  }

  if (!language %in% c("no", "en")) {
    rlang::abort("`language` must be one of \"no\" or \"en\".")
  }

  if (!rlang::is_bool(as_tibble)) {
    rlang::abort("`as_tibble` must be TRUE or FALSE.")
  }

  request_spec <- .build_codelist_request_spec(trimws(codelist_id), language)
  resp <- .execute_request_spec(request_spec)
  parsed <- .parse_response_json(resp)
  out <- .parse_codelist_response(parsed)

  if (as_tibble) {
    out$values <- dplyr::as_tibble(out$values)
  }

  out
}

#' Expand grouped codelist codes to individual member codes
#'
#' @description
#' Takes a data frame that contains grouped codelist codes (for example,
#' the aggregated region codes returned when using
#' `output_values = "aggregated"` in [get_ssb_data()]) and expands each
#' row into one row per individual member code.  This makes it easy to
#' join SSB aggregate data onto a dataset that uses the underlying
#' individual codes.
#'
#' @param data A `data.frame` or tibble that contains a column of
#'   grouped codelist codes.
#' @param code_col Character.  The name of the column in `data` that
#'   holds the grouped codes, for example `"Region"`.
#' @param codelist_id Character.  The codelist identifier that defines
#'   the grouping, for example `"agg_KommSummer"`.
#' @param language Character.  Language used when fetching codelist
#'   details.  `"no"` (default) or `"en"`.
#' @param member_col Character.  The name of the new column to add to
#'   the result that will contain the expanded member codes.  Defaults
#'   to `"member_code"`.  Must not already exist in `data`.
#' @param keep_unmatched Logical.  If `TRUE` (the default), rows whose
#'   grouped code is not found in the codelist are kept in the result
#'   with `NA` in `member_col`.  If `FALSE`, those rows are silently
#'   dropped.
#' @param as_tibble Logical.  If `TRUE` (the default), the result is
#'   returned as a [tibble][tibble::tibble].
#'
#' @details
#' Each row in `data` may expand into multiple rows if its grouped code
#' maps to several member codes.  All original columns are duplicated
#' accordingly.  If a grouped code maps to no members and
#' `keep_unmatched = TRUE`, the row is kept with `NA` in `member_col`;
#' with `keep_unmatched = FALSE` it is dropped.
#'
#' @return A [tibble][tibble::tibble] (or `data.frame` when
#'   `as_tibble = FALSE`) with the same columns as `data` plus one new
#'   column named by `member_col`.  The number of rows may be larger
#'   than the input when groups contain multiple members.
#'
#' @seealso
#' [ssb_codelist_details()] to inspect the codelist mapping.
#' [ssb_codelists()] to discover available codelists for a table.
#' [get_ssb_data()] for the `codelists` and `output_values` arguments
#' that produce the grouped codes this function expands.
#'
#' @examples
#' \dontrun{
#' # Download population aggregated by summer municipality boundary
#' pop_agg <- get_ssb_data(
#'     "07459",
#'     Tid           = "top(1)",
#'     codelists     = list(Region = "agg_KommSummer"),
#'     output_values = list(Region = "aggregated")
#' )
#'
#' # Expand each group row to one row per member municipality code
#' pop_expanded <- ssb_expand_codelist_mapping(
#'     data        = pop_agg,
#'     code_col    = "Region",
#'     codelist_id = "agg_KommSummer"
#' )
#'
#' # Rows whose group code has no members are kept with NA
#' # by default; set keep_unmatched = FALSE to drop them
#' pop_expanded_clean <- ssb_expand_codelist_mapping(
#'     data            = pop_agg,
#'     code_col        = "Region",
#'     codelist_id     = "agg_KommSummer",
#'     keep_unmatched  = FALSE
#' )
#' }
#' @export
ssb_expand_codelist_mapping <- function(
  data,
  code_col,
  codelist_id,
  language = "no",
  member_col = "member_code",
  keep_unmatched = TRUE,
  as_tibble = TRUE
) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data.frame or tibble.")
  }

  if (!rlang::is_string(code_col) || code_col == "") {
    rlang::abort("`code_col` must be a single non-empty character string.")
  }

  if (!code_col %in% names(data)) {
    rlang::abort(
      paste0(
        "Column '",
        code_col,
        "' was not found in `data`. Available columns are: ",
        paste(names(data), collapse = ", "),
        "."
      )
    )
  }

  if (!rlang::is_string(codelist_id) || trimws(codelist_id) == "") {
    rlang::abort("`codelist_id` must be a single non-empty character string.")
  }

  if (!rlang::is_string(member_col) || member_col == "") {
    rlang::abort("`member_col` must be a single non-empty character string.")
  }

  if (member_col %in% names(data)) {
    rlang::abort(paste0("`member_col` ('", member_col, "') already exists in `data`."))
  }

  if (!rlang::is_bool(keep_unmatched)) {
    rlang::abort("`keep_unmatched` must be TRUE or FALSE.")
  }

  if (!rlang::is_bool(as_tibble)) {
    rlang::abort("`as_tibble` must be TRUE or FALSE.")
  }

  code_values <- as.character(data[[code_col]])
  mapping <- ssb_codelist_details(
    codelist_id = trimws(codelist_id),
    language = language,
    as_tibble = FALSE
  )$values

  mapping_lookup <- setNames(mapping$mapped_values, mapping$code)
  row_indices <- vector("list", nrow(data))
  expanded_codes <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    grouped_code <- code_values[[i]]

    if (is.na(grouped_code) || !grouped_code %in% names(mapping_lookup)) {
      if (keep_unmatched) {
        row_indices[[i]] <- i
        expanded_codes[[i]] <- NA_character_
      } else {
        row_indices[[i]] <- integer(0)
        expanded_codes[[i]] <- character(0)
      }
      next
    }

    mapped_values <- mapping_lookup[[grouped_code]]
    members <- if (is.na(mapped_values) || mapped_values == "") {
      character(0)
    } else {
      strsplit(mapped_values, ",", fixed = TRUE)[[1]]
    }

    if (length(members) == 0) {
      if (keep_unmatched) {
        row_indices[[i]] <- i
        expanded_codes[[i]] <- NA_character_
      } else {
        row_indices[[i]] <- integer(0)
        expanded_codes[[i]] <- character(0)
      }
      next
    }

    row_indices[[i]] <- rep.int(i, length(members))
    expanded_codes[[i]] <- members
  }

  expanded_row_index <- unlist(row_indices, use.names = FALSE)
  expanded_member_codes <- unlist(expanded_codes, use.names = FALSE)

  if (length(expanded_row_index) == 0) {
    out <- data[0, , drop = FALSE]
    out[[member_col]] <- character(0)
    if (as_tibble) {
      return(dplyr::as_tibble(out))
    }
    return(out)
  }

  out <- data[expanded_row_index, , drop = FALSE]
  rownames(out) <- NULL
  out[[member_col]] <- expanded_member_codes

  if (as_tibble) {
    return(dplyr::as_tibble(out))
  }

  out
}

#' Download SSB data using a codelist without specifying the dimension
#'
#' @description
#' A convenience wrapper around [get_ssb_data()] that automatically
#' looks up which dimension a codelist belongs to and applies it for
#' you.  Use this instead of [get_ssb_data()] when you know the
#' codelist identifier you want to use but do not want to manually
#' identify the matching dimension.
#'
#' @param table_id Character.  The SSB table identifier, for example
#'   `"07459"`.
#' @param codelist_id Character.  The codelist identifier to apply,
#'   for example `"agg_KommSummer"`.  Use [ssb_codelists()] to find
#'   available identifiers for a table.
#' @param ... Filters for table dimensions, passed directly to
#'   [get_ssb_data()].  Each filter is a named argument whose name is
#'   a dimension identifier and whose value is a character vector of
#'   codes or a server-side expression such as `"top(3)"`.
#' @param dimension Optional character.  Only needed if the same
#'   codelist identifier is used on more than one dimension in the
#'   table.  Specify the dimension identifier explicitly to resolve the
#'   ambiguity.
#' @param output_value Optional character.  Controls how the codelist
#'   dimension appears in the result.  `"aggregated"` (default when
#'   supported) returns one row per group defined in the codelist.
#'   `"single"` returns one row per underlying member code.  Leave
#'   `NULL` to use the SSB default.  Only applicable when the codelist
#'   supports output values (see `supports_output_values` in
#'   [ssb_codelists()]).
#' @param language Character.  Language for labels.  `"no"` (default)
#'   or `"en"`.
#' @param as_tibble Logical.  If `TRUE` (the default), the result is
#'   returned as a [tibble][tibble::tibble].
#' @param override_large_query Logical.  If `TRUE`, bypass the cell
#'   limit guard.  See [get_ssb_data()] for details.
#' @param table_format Character.  `"wide"` (default) or `"long"`.
#'   See [get_ssb_data()] for details.
#' @param include_singleton_dims Logical.  Only used with
#'   `table_format = "long"`.  See [get_ssb_data()] for details.
#' @param cache Logical.  If `TRUE` (the default), table metadata is
#'   cached in memory for the session.
#' @param refresh_metadata Logical.  If `TRUE`, discard cached
#'   metadata and re-fetch from SSB.  Defaults to `FALSE`.
#' @param max_get_query_chars Numeric.  Internal tuning parameter;
#'   see [get_ssb_data()] for details.  Defaults to `1500L`.
#' @param show_chunk_progress Logical.  Deprecated.  Has no effect.
#'
#' @details
#' The function calls [ssb_codelists()] internally to confirm that the
#' codelist identifier exists for the table and to determine which
#' dimension it belongs to.  It then calls [get_ssb_data()] with the
#' correct `codelists` and `output_values` arguments filled in
#' automatically.
#'
#' If the codelist identifier is present on more than one dimension,
#' the function stops with a message listing the affected dimensions
#' so you can pass the correct one via `dimension`.
#'
#' @return A [tibble][tibble::tibble] (or `data.frame` when
#'   `as_tibble = FALSE`) as described in [get_ssb_data()].
#'
#' @seealso
#' [get_ssb_data()] for the full set of download options.
#' [ssb_codelists()] to discover available codelists for a table.
#' [ssb_codelist_details()] to inspect the group-to-member mapping.
#' [ssb_expand_codelist_mapping()] to expand aggregated result rows
#' to individual member codes after downloading.
#'
#' @examples
#' \dontrun{
#' # Download population aggregated by summer municipality boundary.
#' # The function detects that "agg_KommSummer" belongs to Region.
#' pop <- ssb_get_by_codelist(
#'     "07459",
#'     "agg_KommSummer",
#'     Tid          = "top(3)",
#'     output_value = "aggregated"
#' )
#'
#' # Request individual member codes instead of aggregated groups
#' pop_single <- ssb_get_by_codelist(
#'     "07459",
#'     "agg_KommSummer",
#'     Tid          = "top(3)",
#'     output_value = "single"
#' )
#'
#' # When the codelist exists on multiple dimensions, specify which one
#' pop_explicit <- ssb_get_by_codelist(
#'     "07459",
#'     "agg_KommSummer",
#'     Tid          = "top(3)",
#'     dimension    = "Region",
#'     output_value = "aggregated"
#' )
#' }
#' @export
ssb_get_by_codelist <- function(
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
) {
  .validate_table_request_args(table_id, language)
  .validate_metadata_cache_args(cache, refresh_metadata)

  if (!rlang::is_string(codelist_id) || trimws(codelist_id) == "") {
    rlang::abort("`codelist_id` must be a single non-empty character string.")
  }

  codelist_id <- trimws(codelist_id)

  if (!is.null(dimension) && (!rlang::is_string(dimension) || trimws(dimension) == "")) {
    rlang::abort("`dimension` must be NULL or a single non-empty character string.")
  }

  if (!is.null(output_value)) {
    if (!rlang::is_string(output_value) || trimws(output_value) == "") {
      rlang::abort("`output_value` must be NULL or a single non-empty character string.")
    }

    output_value <- tolower(trimws(output_value))
    if (!output_value %in% c("aggregated", "single")) {
      rlang::abort("`output_value` must be either 'aggregated' or 'single'.")
    }
  }

  codelists_df <- ssb_codelists(
    table_id = table_id,
    dimension = dimension,
    language = language,
    cache = cache,
    refresh_metadata = refresh_metadata
  )

  if (nrow(codelists_df) == 0) {
    rlang::abort(paste0("No codelists available for table '", table_id, "'."))
  }

  matches <- codelists_df[codelists_df$codelist_id == codelist_id, , drop = FALSE]

  if (nrow(matches) == 0) {
    available_ids <- unique(codelists_df$codelist_id)
    sample_ids <- paste(utils::head(available_ids, 8), collapse = ", ")
    if (length(available_ids) > 8) {
      sample_ids <- paste0(sample_ids, ", ...")
    }

    rlang::abort(
      paste0(
        "Codelist id '",
        codelist_id,
        "' was not found for table '",
        table_id,
        "'. Available examples: ",
        sample_ids,
        ". Use ssb_codelists(\"",
        table_id,
        "\") to inspect all options."
      )
    )
  }

  if (nrow(matches) > 1 && is.null(dimension)) {
    ambiguous_dims <- paste(unique(matches$dimension), collapse = ", ")
    rlang::abort(
      paste0(
        "Codelist id '",
        codelist_id,
        "' exists on multiple dimensions: ",
        ambiguous_dims,
        ". Specify `dimension` explicitly."
      )
    )
  }

  target_dimension <- matches$dimension[[1]]
  target_supports_output_values <- matches$supports_output_values[[1]]

  codelists_arg <- list()
  codelists_arg[[target_dimension]] <- codelist_id

  output_values_arg <- NULL
  if (!is.null(output_value)) {
    if (!isTRUE(target_supports_output_values)) {
      rlang::abort(
        paste0(
          "Codelist id '",
          codelist_id,
          "' on dimension '",
          target_dimension,
          "' does not support `output_value`."
        )
      )
    }

    output_values_arg <- list()
    output_values_arg[[target_dimension]] <- output_value
  }

  request_args <- c(
    list(table_id = table_id),
    rlang::list2(...),
    list(
      language = language,
      as_tibble = as_tibble,
      override_large_query = override_large_query,
      table_format = table_format,
      include_singleton_dims = include_singleton_dims,
      codelists = codelists_arg,
      output_values = output_values_arg,
      cache = cache,
      refresh_metadata = refresh_metadata,
      max_get_query_chars = max_get_query_chars,
      show_chunk_progress = show_chunk_progress
    )
  )

  do.call(get_ssb_data, request_args)
}
