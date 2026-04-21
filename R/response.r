.parse_response_json <- function(resp) {
  response_text <- httr2::resp_body_string(resp)

  tryCatch(
    jsonlite::fromJSON(response_text, simplifyVector = FALSE),
    error = function(cnd) {
      rlang::abort(
        paste0("Failed to parse API response as JSON: ", conditionMessage(cnd))
      )
    }
  )
}

.parse_codelist_response <- function(parsed_codelist) {
  values <- parsed_codelist$values

  if (is.null(values) || length(values) == 0) {
    values_df <- data.frame(
      code = character(0),
      label = character(0),
      mapped_values = character(0),
      mapped_count = integer(0),
      stringsAsFactors = FALSE
    )
  } else {
    value_rows <- lapply(values, function(x) {
      mapped <- x$valueMap
      if (is.null(mapped)) {
        mapped <- character(0)
      }

      data.frame(
        code = as.character(x$code),
        label = as.character(x$label),
        mapped_values = paste(as.character(mapped), collapse = ","),
        mapped_count = length(mapped),
        stringsAsFactors = FALSE
      )
    })

    values_df <- do.call(rbind, value_rows)
    rownames(values_df) <- NULL
  }

  list(
    codelist = list(
      id = as.character(parsed_codelist$id),
      label = as.character(parsed_codelist$label),
      type = as.character(parsed_codelist$type),
      language = as.character(parsed_codelist$language),
      elimination = isTRUE(parsed_codelist$elimination),
      elimination_value_code = if (is.null(parsed_codelist$eliminationValueCode)) {
        NA_character_
      } else {
        as.character(parsed_codelist$eliminationValueCode)
      }
    ),
    values = values_df
  )
}

.parse_tables_response <- function(parsed) {
  .metric_dimension_labels <- c("statistikkvariabel", "contents")

  tables <- parsed$tables
  if (is.null(tables) || length(tables) == 0) {
    df <- data.frame(
      id = character(0),
      label = character(0),
      description = character(0),
      updated = character(0),
      first_period = character(0),
      last_period = character(0),
      category = character(0),
      source = character(0),
      variables = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    rows <- lapply(tables, function(t) {
      variable_names <- if (is.null(t$variableNames)) character(0) else unlist(t$variableNames, use.names = FALSE)
      variable_names <- variable_names[!variable_names %in% .metric_dimension_labels]
      data.frame(
        id = as.character(t$id),
        label = as.character(t$label),
        description = if (is.null(t$description) || length(t$description) == 0) NA_character_ else as.character(t$description),
        updated = if (is.null(t$updated) || length(t$updated) == 0) NA_character_ else as.character(t$updated),
        first_period = if (is.null(t$firstPeriod) || length(t$firstPeriod) == 0) NA_character_ else as.character(t$firstPeriod),
        last_period = if (is.null(t$lastPeriod) || length(t$lastPeriod) == 0) NA_character_ else as.character(t$lastPeriod),
        category = if (is.null(t$category) || length(t$category) == 0) NA_character_ else as.character(t$category),
        source = if (is.null(t$source) || length(t$source) == 0) NA_character_ else as.character(t$source),
        variables = paste(variable_names, collapse = ", "),
        stringsAsFactors = FALSE
      )
    })

    df <- do.call(rbind, rows)
    rownames(df) <- NULL
  }

  pg <- parsed$page
  page_info <- list(
    page_number = as.integer(pg$pageNumber),
    page_size = as.integer(pg$pageSize),
    total_elements = as.integer(pg$totalElements),
    total_pages = as.integer(pg$totalPages)
  )

  list(data = df, page_info = page_info)
}

.extract_observation_status <- function(parsed_json, n_observations) {
  status_values <- rep(NA_character_, n_observations)
  raw_status <- parsed_json$status

  if (is.null(raw_status) || length(raw_status) == 0) {
    return(status_values)
  }

  status_entries <- unlist(raw_status, use.names = TRUE)
  if (length(status_entries) == 0) {
    return(status_values)
  }

  entry_names <- names(status_entries)
  status_entries <- as.character(status_entries)

  if (is.null(entry_names) || any(entry_names == "")) {
    if (length(status_entries) == n_observations) {
      status_values[] <- status_entries
    }
    return(status_values)
  }

  status_index <- suppressWarnings(as.integer(sub("^.*\\.", "", entry_names)))
  valid_index <- !is.na(status_index) & status_index >= 0L & status_index < n_observations

  if (any(valid_index)) {
    status_values[status_index[valid_index] + 1L] <- status_entries[valid_index]
  }

  status_values
}

.dimension_code_col_name <- function(dimension_id) {
  if (grepl("Code$", dimension_id)) {
    return(dimension_id)
  }

  paste0(dimension_id, "_code")
}

.dimension_label_col_name <- function(dimension_id) {
  paste0(dimension_id, "_label")
}

.coerce_time_dimensions <- function(
  grid,
  dimension_ids,
  table_format,
  metadata
) {
  if (
    is.null(metadata) || is.null(metadata$roles) || is.null(metadata$roles$time)
  ) {
    return(grid)
  }

  time_dims <- unlist(metadata$roles$time, use.names = FALSE)
  if (length(time_dims) == 0) {
    return(grid)
  }

  if (table_format == "wide") {
    for (time_dim in time_dims) {
      label_col <- paste0(time_dim, "_label")
      if (label_col %in% names(grid)) {
        coerced <- suppressWarnings(as.numeric(grid[[label_col]]))
        if (!all(is.na(coerced))) {
          grid[[label_col]] <- coerced
        }
      }
    }
  }

  grid
}

.is_quarter_string <- function(x) {
  grepl("^[0-9]{4}[KkQq][1-4]$", x)
}

.is_quarter_column <- function(x) {
  if (!is.character(x)) {
    return(FALSE)
  }

  non_missing <- x[!is.na(x)]
  if (length(non_missing) == 0) {
    return(FALSE)
  }

  all(.is_quarter_string(non_missing))
}

.is_month_string <- function(x) {
  grepl("^[0-9]{4}[Mm](0[1-9]|1[0-2])$", x)
}

.format_as_year_quarter <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    paste0(substr(x, 1, 4), "-Q", substr(x, nchar(x), nchar(x)))
  )
}

.is_month_column <- function(x) {
  if (!is.character(x)) {
    return(FALSE)
  }

  non_missing <- x[!is.na(x)]
  if (length(non_missing) == 0) {
    return(FALSE)
  }

  all(.is_month_string(non_missing))
}

.convert_to_yearqtr <- function(x) {
  if (requireNamespace("zoo", quietly = TRUE)) {
    formatted <- .format_as_year_quarter(x)
    zoo::as.yearqtr(formatted, format = "%Y-Q%q")
  } else {
    rlang::warn("zoo package required for yearqtr conversion; returning original values")
    x
  }
}

.convert_to_yearmon <- function(x) {
  if (requireNamespace("zoo", quietly = TRUE)) {
    formatted <- ifelse(is.na(x), NA_character_, paste0(substr(x, 1, 4), "-", substr(x, 6, 7)))
    zoo::as.yearmon(formatted, format = "%Y-%m")
  } else {
    rlang::warn("zoo package required for yearmon conversion; returning original values")
    x
  }
}

.apply_output_types <- function(grid, character_as_factor, convert_quarter_to_yearqtr = TRUE, convert_month_to_yearmon = TRUE) {
  if (is.character(convert_quarter_to_yearqtr)) {
    legacy_mode <- tolower(trimws(convert_quarter_to_yearqtr))
    convert_quarter_to_yearqtr <- identical(legacy_mode, "year_quarter")
    convert_month_to_yearmon <- FALSE
  }

  character_columns <- names(grid)[vapply(grid, is.character, logical(1))]
  if (length(character_columns) == 0) {
    return(grid)
  }

  quarter_columns <- character_columns[vapply(
    character_columns,
    function(col_name) .is_quarter_column(grid[[col_name]]),
    logical(1)
  )]

  month_columns <- character_columns[vapply(
    character_columns,
    function(col_name) .is_month_column(grid[[col_name]]),
    logical(1)
  )]

  if (convert_quarter_to_yearqtr && length(quarter_columns) > 0) {
    for (col_name in quarter_columns) {
      grid[[col_name]] <- .convert_to_yearqtr(grid[[col_name]])
    }
  }

  if (convert_month_to_yearmon && length(month_columns) > 0) {
    for (col_name in month_columns) {
      grid[[col_name]] <- .convert_to_yearmon(grid[[col_name]])
    }
  }

  non_temporal_factor_columns <- setdiff(
    character_columns,
    if (convert_quarter_to_yearqtr) quarter_columns else character(0)
  )
  non_temporal_factor_columns <- setdiff(
    non_temporal_factor_columns,
    if (convert_month_to_yearmon) month_columns else character(0)
  )

  if (character_as_factor && length(non_temporal_factor_columns) > 0) {
    for (col_name in non_temporal_factor_columns) {
      grid[[col_name]] <- factor(grid[[col_name]])
    }
  }

  grid
}

.tidy_response_json <- function(
  parsed_json,
  as_tibble,
  table_format,
  include_singleton_dims = FALSE,
  metadata = NULL,
  include_status = FALSE,
  character_as_factor = TRUE,
  convert_quarter_to_yearqtr = TRUE,
  convert_month_to_yearmon = TRUE
) {
  dimension_ids <- unlist(parsed_json$id, use.names = FALSE)
  raw_dimensions <- parsed_json$dimension

  code_values <- setNames(vector("list", length(dimension_ids)), dimension_ids)
  label_values <- setNames(vector("list", length(dimension_ids)), dimension_ids)

  for (dimension_id in dimension_ids) {
    raw_dimension <- raw_dimensions[[dimension_id]]
    category_index <- unlist(raw_dimension$category$index, use.names = TRUE)
    codes <- names(sort(category_index))
    labels <- vapply(
      codes,
      function(code) {
        raw_dimension$category$label[[code]]
      },
      character(1)
    )

    code_values[[dimension_id]] <- codes
    label_values[[dimension_id]] <- labels
  }

  grid_dimension_ids <- rev(dimension_ids)

  code_grid <- expand.grid(
    code_values[grid_dimension_ids],
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  code_grid <- code_grid[dimension_ids]
  names(code_grid) <- vapply(dimension_ids, .dimension_code_col_name, character(1))

  label_grid <- expand.grid(
    label_values[grid_dimension_ids],
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  label_grid <- label_grid[dimension_ids]
  names(label_grid) <- vapply(dimension_ids, .dimension_label_col_name, character(1))

  grid <- cbind(code_grid, label_grid, stringsAsFactors = FALSE)

  values <- vapply(
    parsed_json$value,
    function(x) {
      if (is.null(x)) {
        return(NA_real_)
      }

      as.numeric(x)
    },
    numeric(1)
  )
  if (length(values) != nrow(grid)) {
    rlang::abort(
      paste0(
        "Parsed value count (",
        length(values),
        ") does not match observation grid size (",
        nrow(grid),
        ")."
      )
    )
  }

  grid$value <- values

  if (include_status) {
    grid$status <- .extract_observation_status(parsed_json, nrow(grid))
  }

  if (table_format == "long") {
    observation_id <- seq_len(nrow(grid))

    dims_to_use <- dimension_ids
    if (!include_singleton_dims) {
      singleton_dims <- sapply(dimension_ids, function(dim_id) {
        length(code_values[[dim_id]]) == 1
      })
      dims_to_use <- dimension_ids[!singleton_dims]
    }

    long_rows <- lapply(dims_to_use, function(dimension_id) {
      row_data <- data.frame(
        observation_id = observation_id,
        dimension = dimension_id,
        code = grid[[.dimension_code_col_name(dimension_id)]],
        label = grid[[.dimension_label_col_name(dimension_id)]],
        value = grid$value,
        stringsAsFactors = FALSE
      )

      if (include_status) {
        row_data$status <- grid$status
      }

      row_data
    })

    grid <- do.call(rbind, long_rows)
    rownames(grid) <- NULL
  }

  grid <- .coerce_time_dimensions(grid, dimension_ids, table_format, metadata)
  grid <- .apply_output_types(grid, character_as_factor, convert_quarter_to_yearqtr, convert_month_to_yearmon)

  if (as_tibble) {
    return(dplyr::as_tibble(grid))
  }

  grid
}

.wide_to_long_grid <- function(
  wide_grid,
  dimension_ids,
  completed_filters,
  include_singleton_dims
) {
  dims_to_use <- dimension_ids

  if (!include_singleton_dims) {
    dims_to_use <- dimension_ids[
      vapply(
        dimension_ids,
        function(dimension_id) {
          length(completed_filters[[dimension_id]]) > 1
        },
        logical(1)
      )
    ]
  }

  if (length(dims_to_use) == 0) {
    return(data.frame(
      observation_id = integer(0),
      dimension = character(0),
      code = character(0),
      label = character(0),
      value = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  observation_id <- seq_len(nrow(wide_grid))

  long_rows <- lapply(dims_to_use, function(dimension_id) {
    data.frame(
      observation_id = observation_id,
      dimension = dimension_id,
      code = wide_grid[[.dimension_code_col_name(dimension_id)]],
      label = wide_grid[[.dimension_label_col_name(dimension_id)]],
      value = wide_grid$value,
      stringsAsFactors = FALSE
    )
  })

  long_grid <- do.call(rbind, long_rows)
  rownames(long_grid) <- NULL
  long_grid
}
