.validate_table_request_args <- function(table_id, language) {
  if (!rlang::is_string(table_id) || table_id == "") {
    rlang::abort("`table_id` must be a single non-empty character string.")
  }

  if (!language %in% c("no", "en")) {
    rlang::abort("`language` must be one of \"no\" or \"en\".")
  }
}

.validate_metadata_cache_args <- function(cache, refresh_metadata) {
  if (!rlang::is_bool(cache)) {
    rlang::abort("`cache` must be TRUE or FALSE.")
  }

  if (!rlang::is_bool(refresh_metadata)) {
    rlang::abort("`refresh_metadata` must be TRUE or FALSE.")
  }
}

.validate_main_args <- function(
  table_id,
  language,
  as_tibble,
  override_large_query,
  table_format,
  include_singleton_dims,
  include_status,
  character_as_factor,
  quarter_as
) {
  .validate_table_request_args(table_id, language)

  if (!rlang::is_bool(as_tibble)) {
    rlang::abort("`as_tibble` must be TRUE or FALSE.")
  }

  if (!rlang::is_bool(override_large_query)) {
    rlang::abort("`override_large_query` must be TRUE or FALSE.")
  }

  if (!rlang::is_string(table_format) || !table_format %in% c("wide", "long")) {
    rlang::abort("`table_format` must be either \"wide\" or \"long\".")
  }

  if (!rlang::is_bool(include_singleton_dims)) {
    rlang::abort("`include_singleton_dims` must be TRUE or FALSE.")
  }

  if (!rlang::is_bool(include_status)) {
    rlang::abort("`include_status` must be TRUE or FALSE.")
  }

  if (!rlang::is_bool(character_as_factor)) {
    rlang::abort("`character_as_factor` must be TRUE or FALSE.")
  }

  if (!rlang::is_string(quarter_as) || trimws(quarter_as) == "") {
    rlang::abort("`quarter_as` must be a single non-empty character string.")
  }

  quarter_as_normalized <- tolower(trimws(quarter_as))
  if (!quarter_as_normalized %in% c("year_quarter", "character")) {
    rlang::abort("`quarter_as` must be one of 'year_quarter' or 'character'.")
  }
}

.validate_chunking_args <- function(max_get_query_chars, show_chunk_progress) {
  if (
    length(max_get_query_chars) != 1 ||
      !is.numeric(max_get_query_chars) ||
      is.na(max_get_query_chars)
  ) {
    rlang::abort("`max_get_query_chars` must be a single numeric value.")
  }

  if (max_get_query_chars <= 0) {
    rlang::abort("`max_get_query_chars` must be greater than 0.")
  }

  if (!rlang::is_bool(show_chunk_progress)) {
    rlang::abort("`show_chunk_progress` must be TRUE or FALSE.")
  }
}

.validate_filters <- function(filters) {
  if (length(filters) == 0) {
    return(invisible(NULL))
  }

  filter_names <- names(filters)

  if (is.null(filter_names) || any(filter_names == "")) {
    rlang::abort("All filters in `...` must be named.")
  }

  if (anyDuplicated(filter_names)) {
    rlang::abort("Filter names in `...` must be unique.")
  }

  for (nm in filter_names) {
    value <- filters[[nm]]

    if (is.null(value)) {
      rlang::abort(paste0("Filter '", nm, "' cannot be NULL."))
    }

    if (!is.atomic(value)) {
      rlang::abort(paste0("Filter '", nm, "' must be an atomic vector."))
    }

    if (length(value) == 0) {
      rlang::abort(paste0("Filter '", nm, "' must contain at least one value."))
    }

    if (any(is.na(value))) {
      rlang::abort(paste0("Filter '", nm, "' cannot contain missing values."))
    }

    if (is.character(value) && any(trimws(value) == "")) {
      rlang::abort(paste0("Filter '", nm, "' cannot contain empty strings."))
    }

    if (typeof(value) == "logical") {
      rlang::abort(paste0("Filter '", nm, "' cannot contain TRUE or FALSE."))
    }
  }
}

.validate_codelists <- function(codelists) {
  if (is.null(codelists) || length(codelists) == 0) {
    return(invisible(NULL))
  }

  if (!is.list(codelists) && !is.atomic(codelists)) {
    rlang::abort("`codelists` must be a named list or named atomic vector.")
  }

  codelist_names <- names(codelists)
  if (is.null(codelist_names) || any(codelist_names == "")) {
    rlang::abort("All entries in `codelists` must be named by dimension id.")
  }

  if (anyDuplicated(codelist_names)) {
    rlang::abort("Dimension names in `codelists` must be unique.")
  }

  for (nm in codelist_names) {
    value <- codelists[[nm]]
    if (length(value) != 1 || is.na(value)) {
      rlang::abort(paste0("Codelist for dimension '", nm, "' must be a single non-missing value."))
    }

    value_chr <- trimws(as.character(value))
    if (value_chr == "") {
      rlang::abort(paste0("Codelist for dimension '", nm, "' cannot be empty."))
    }
  }
}

.normalise_codelists <- function(codelists) {
  if (is.null(codelists) || length(codelists) == 0) {
    return(list())
  }

  normalised_codelists <- list()
  for (nm in names(codelists)) {
    normalised_codelists[[nm]] <- trimws(as.character(codelists[[nm]][[1]]))
  }

  normalised_codelists
}

.validate_output_values <- function(output_values) {
  if (is.null(output_values) || length(output_values) == 0) {
    return(invisible(NULL))
  }

  if (!is.list(output_values) && !is.atomic(output_values)) {
    rlang::abort("`output_values` must be a named list or named atomic vector.")
  }

  output_value_names <- names(output_values)
  if (is.null(output_value_names) || any(output_value_names == "")) {
    rlang::abort("All entries in `output_values` must be named by dimension id.")
  }

  if (anyDuplicated(output_value_names)) {
    rlang::abort("Dimension names in `output_values` must be unique.")
  }

  for (nm in output_value_names) {
    value <- output_values[[nm]]
    if (length(value) != 1 || is.na(value)) {
      rlang::abort(paste0("Output value mode for dimension '", nm, "' must be a single non-missing value."))
    }

    value_chr <- trimws(tolower(as.character(value)))
    if (!value_chr %in% c("aggregated", "single")) {
      rlang::abort(
        paste0(
          "Invalid `output_values` mode for dimension '",
          nm,
          "': ",
          as.character(value),
          ". Allowed values are 'aggregated' or 'single'."
        )
      )
    }
  }
}

.normalise_output_values <- function(output_values) {
  if (is.null(output_values) || length(output_values) == 0) {
    return(list())
  }

  normalised_output_values <- list()
  for (nm in names(output_values)) {
    normalised_output_values[[nm]] <- trimws(tolower(as.character(output_values[[nm]][[1]])))
  }

  normalised_output_values
}

.normalise_quarter_as <- function(quarter_as) {
  tolower(trimws(quarter_as))
}

.normalise_filters <- function(filters) {
  normalised_filters <- list()

  for (nm in names(filters)) {
    value <- filters[[nm]]

    value_character <- as.character(value)

    value_trimmed <- trimws(value_character)

    value_unique <- unique(value_trimmed)

    normalised_filters[[nm]] <- value_unique
  }
  return(normalised_filters)
}

.validate_filter_names_against_metadata <- function(
  normalised_filters,
  metadata
) {
  if (length(normalised_filters) == 0) {
    return(invisible(NULL))
  }

  valid_dimension_names <- names(metadata$dimensions)
  invalid_filter_names <- setdiff(
    names(normalised_filters),
    valid_dimension_names
  )

  if (length(invalid_filter_names) > 0) {
    rlang::abort(
      paste0(
        "Unknown filter name(s): ",
        paste(invalid_filter_names, collapse = ", "),
        ". Valid dimensions are: ",
        paste(valid_dimension_names, collapse = ", "),
        "."
      )
    )
  }
}

.validate_codelist_names_against_metadata <- function(normalised_codelists, metadata) {
  if (length(normalised_codelists) == 0) {
    return(invisible(NULL))
  }

  valid_dimension_names <- names(metadata$dimensions)
  invalid_codelist_names <- setdiff(names(normalised_codelists), valid_dimension_names)

  if (length(invalid_codelist_names) > 0) {
    rlang::abort(
      paste0(
        "Unknown codelist dimension name(s): ",
        paste(invalid_codelist_names, collapse = ", "),
        ". Valid dimensions are: ",
        paste(valid_dimension_names, collapse = ", "),
        "."
      )
    )
  }
}

.validate_output_value_names_against_metadata <- function(normalised_output_values, metadata) {
  if (length(normalised_output_values) == 0) {
    return(invisible(NULL))
  }

  valid_dimension_names <- names(metadata$dimensions)
  invalid_output_value_names <- setdiff(names(normalised_output_values), valid_dimension_names)

  if (length(invalid_output_value_names) > 0) {
    rlang::abort(
      paste0(
        "Unknown output_values dimension name(s): ",
        paste(invalid_output_value_names, collapse = ", "),
        ". Valid dimensions are: ",
        paste(valid_dimension_names, collapse = ", "),
        "."
      )
    )
  }
}

.validate_output_values_against_codelists <- function(normalised_output_values, normalised_codelists) {
  if (length(normalised_output_values) == 0) {
    return(invisible(NULL))
  }

  output_value_dims_without_codelist <- setdiff(names(normalised_output_values), names(normalised_codelists))

  if (length(output_value_dims_without_codelist) > 0) {
    rlang::abort(
      paste0(
        "`output_values` requires matching entries in `codelists` for dimension(s): ",
        paste(output_value_dims_without_codelist, collapse = ", "),
        "."
      )
    )
  }
}

.validate_filter_values_against_metadata <- function(
  normalised_filters,
  metadata,
  normalised_codelists = list()
) {
  for (filter_name in names(normalised_filters)) {
    if (filter_name %in% names(normalised_codelists)) {
      next
    }

    filter_values <- normalised_filters[[filter_name]]
    valid_codes <- metadata$dimensions[[filter_name]]$codes
    literal_values <- filter_values[!.is_api_expression(filter_values)]
    invalid_values <- setdiff(literal_values, valid_codes)

    if (length(invalid_values) > 0) {
      n_valid <- length(valid_codes)
      sample_codes <- head(valid_codes, 5)
      sample_hint <- paste(sample_codes, collapse = ", ")
      if (n_valid > 5) {
        sample_hint <- paste0(sample_hint, ", ... (", n_valid, " codes total)")
      }

      rlang::abort(
        paste0(
          "Invalid value(s) for dimension '",
          filter_name,
          "': ",
          paste(invalid_values, collapse = ", "),
          ".\n",
          "First valid codes: ",
          sample_hint,
          ".\n",
          "Use ssb_codes(\"",
          metadata$table_id,
          "\", \"",
          filter_name,
          "\") to see all valid codes."
        )
      )
    }
  }
}
