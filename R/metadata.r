.default_api_config <- function() {
  list(
    api_version = NA_character_,
    app_version = NA_character_,
    default_language = "no",
    default_data_format = "json-stat2",
    max_data_cells = 100000L,
    max_calls_per_time_window = NA_integer_,
    time_window = NA_integer_,
    data_formats = character(0),
    source = "fallback"
  )
}

.fetch_api_config <- function() {
  request_spec <- .build_config_request_spec()
  resp <- .execute_request_spec(request_spec)
  .parse_response_json(resp)
}

.parse_api_config <- function(raw_config) {
  default_config <- .default_api_config()

  max_data_cells <- suppressWarnings(as.integer(raw_config$maxDataCells))
  if (is.na(max_data_cells) || max_data_cells <= 0L) {
    max_data_cells <- default_config$max_data_cells
  }

  max_calls_per_time_window <- suppressWarnings(as.integer(raw_config$maxCallsPerTimeWindow))
  if (length(max_calls_per_time_window) == 0 || is.na(max_calls_per_time_window) || max_calls_per_time_window <= 0L) {
    max_calls_per_time_window <- NA_integer_
  }

  time_window <- suppressWarnings(as.integer(raw_config$timeWindow))
  if (length(time_window) == 0 || is.na(time_window) || time_window <= 0L) {
    time_window <- NA_integer_
  }

  list(
    api_version = if (is.null(raw_config$apiVersion)) NA_character_ else as.character(raw_config$apiVersion),
    app_version = if (is.null(raw_config$appVersion)) NA_character_ else as.character(raw_config$appVersion),
    default_language = if (is.null(raw_config$defaultLanguage)) default_config$default_language else as.character(raw_config$defaultLanguage),
    default_data_format = if (is.null(raw_config$defaultDataFormat)) default_config$default_data_format else as.character(raw_config$defaultDataFormat),
    max_data_cells = max_data_cells,
    max_calls_per_time_window = max_calls_per_time_window,
    time_window = time_window,
    data_formats = if (is.null(raw_config$dataFormats)) character(0) else as.character(unlist(raw_config$dataFormats, use.names = FALSE)),
    source = "api"
  )
}

.get_api_config <- function(cache = TRUE, refresh_metadata = FALSE) {
  .validate_metadata_cache_args(cache, refresh_metadata)

  cache_key <- "config"
  if (
    cache &&
      !refresh_metadata &&
      exists(cache_key, envir = .config_cache, inherits = FALSE)
  ) {
    return(get(cache_key, envir = .config_cache, inherits = FALSE))
  }

  parsed_config <- tryCatch(
    .parse_api_config(.fetch_api_config()),
    error = function(...) .default_api_config()
  )

  if (cache) {
    assign(cache_key, parsed_config, envir = .config_cache)
  }

  parsed_config
}

.fetch_table_metadata <- function(table_id, language) {
  request_spec <- .build_metadata_request_spec(
    table_id = table_id,
    language = language
  )

  resp <- .execute_request_spec(request_spec)

  .parse_response_json(resp)
}

.parse_table_metadata <- function(raw_metadata) {
  dimension_ids <- unlist(raw_metadata$id, use.names = FALSE)
  dimension_sizes <- as.integer(unlist(raw_metadata$size, use.names = FALSE))
  raw_dimensions <- raw_metadata$dimension

  dimensions <- setNames(vector("list", length(dimension_ids)), dimension_ids)

  for (i in seq_along(dimension_ids)) {
    dimension_id <- dimension_ids[[i]]
    raw_dimension <- raw_dimensions[[dimension_id]]
    category_index <- raw_dimension$category$index
    category_labels <- raw_dimension$category$label
    codes <- names(category_index)

    labels <- vapply(
      codes,
      function(code) {
        category_labels[[code]]
      },
      character(1)
    )

    raw_codelists <- raw_dimension$extension$codelists
    parsed_codelists <- list()
    raw_units <- raw_dimension$category$unit
    parsed_units <- data.frame(
      code = character(0),
      unit_base = character(0),
      unit_decimals = integer(0),
      stringsAsFactors = FALSE
    )

    if (!is.null(raw_codelists) && length(raw_codelists) > 0) {
      parsed_codelists <- lapply(raw_codelists, function(raw_codelist) {
        links <- raw_codelist$links
        href <- NA_character_

        if (!is.null(links) && length(links) > 0) {
          href <- as.character(links[[1]]$href)
        }

        list(
          id = as.character(raw_codelist$id),
          label = as.character(raw_codelist$label),
          type = as.character(raw_codelist$type),
          href = href
        )
      })
    }

    if (!is.null(raw_units) && length(raw_units) > 0) {
      unit_codes <- names(raw_units)
      parsed_units <- do.call(rbind, lapply(unit_codes, function(unit_code) {
        unit_info <- raw_units[[unit_code]]

        data.frame(
          code = as.character(unit_code),
          unit_base = if (is.null(unit_info$base)) NA_character_ else as.character(unit_info$base),
          unit_decimals = if (is.null(unit_info$decimals)) NA_integer_ else as.integer(unit_info$decimals),
          stringsAsFactors = FALSE
        )
      }))
      rownames(parsed_units) <- NULL
    }

    dimensions[[dimension_id]] <- list(
      id = dimension_id,
      label = raw_dimension$label,
      codes = codes,
      labels = labels,
      code_count = length(codes),
      size = dimension_sizes[[i]],
      codelists = parsed_codelists,
      units = parsed_units
    )
  }

  list(
    table_id = raw_metadata$extension$px$tableid,
    table_label = raw_metadata$label,
    updated = raw_metadata$updated,
    roles = raw_metadata$role,
    dimensions = dimensions,
    full_table_estimated_rows = prod(dimension_sizes)
  )
}

.build_metadata_cache_key <- function(table_id, language) {
  paste0(table_id, "::", language)
}

.get_table_metadata <- function(
  table_id,
  language,
  cache = TRUE,
  refresh_metadata = FALSE
) {
  .validate_metadata_cache_args(cache, refresh_metadata)

  cache_key <- .build_metadata_cache_key(table_id, language)

  if (
    cache &&
      !refresh_metadata &&
      exists(cache_key, envir = .metadata_cache, inherits = FALSE)
  ) {
    return(get(cache_key, envir = .metadata_cache, inherits = FALSE))
  }

  raw_metadata <- .fetch_table_metadata(
    table_id = table_id,
    language = language
  )

  parsed_metadata <- .parse_table_metadata(raw_metadata)

  if (cache) {
    assign(cache_key, parsed_metadata, envir = .metadata_cache)
  }

  parsed_metadata
}

.auto_complete_filters <- function(metadata, normalised_filters, normalised_codelists = list()) {
  completed_filters <- vector("list", length(metadata$dimensions))
  names(completed_filters) <- names(metadata$dimensions)

  for (dimension_id in names(metadata$dimensions)) {
    if (dimension_id %in% names(normalised_filters)) {
      completed_filters[[dimension_id]] <- normalised_filters[[dimension_id]]
    } else if (dimension_id %in% names(normalised_codelists)) {
      completed_filters[[dimension_id]] <- "*"
    } else {
      completed_filters[[dimension_id]] <- metadata$dimensions[[
        dimension_id
      ]]$codes
    }
  }

  completed_filters
}

.estimate_row_count <- function(metadata, completed_filters) {
  dimension_ids <- names(metadata$dimensions)
  selection_sizes <- vapply(
    dimension_ids,
    function(dimension_id) {
      values <- completed_filters[[dimension_id]]
      if (any(.is_api_expression(values))) {
        metadata$dimensions[[dimension_id]]$code_count
      } else {
        length(values)
      }
    },
    integer(1)
  )

  prod(selection_sizes)
}

.is_api_expression <- function(x) {
  grepl("^[*]$|^[?][*]$|^(top|bottom|from|to|range)[(]", x, ignore.case = TRUE)
}
