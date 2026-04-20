.build_query_params <- function(
  language,
  normalised_filters,
  codelists = list(),
  output_values = list()
) {
  query_params <- list(
    lang = language,
    outputFormat = "json-stat2"
  )

  if (length(normalised_filters) == 0) {
    return(query_params)
  }

  for (nm in names(normalised_filters)) {
    value <- normalised_filters[[nm]]
    param_name <- paste0("valueCodes[", nm, "]")
    query_params[[param_name]] <- paste(value, collapse = ",")
  }

  if (length(codelists) > 0) {
    for (nm in names(codelists)) {
      param_name <- paste0("codelist[", nm, "]")
      query_params[[param_name]] <- codelists[[nm]]
    }
  }

  if (length(output_values) > 0) {
    for (nm in names(output_values)) {
      param_name <- paste0("outputValues[", nm, "]")
      query_params[[param_name]] <- output_values[[nm]]
    }
  }

  query_params
}

.build_request_spec <- function(
  table_id,
  language,
  normalised_filters,
  codelists = list(),
  output_values = list()
) {
  query_params <- .build_query_params(
    language,
    normalised_filters,
    codelists = codelists,
    output_values = output_values
  )

  request_spec <- list(
    table_id = table_id,
    endpoint = .build_data_endpoint(table_id),
    params = query_params,
    method = "GET"
  )

  request_spec
}

.build_post_request_spec <- function(
  table_id,
  language,
  completed_filters,
  codelists = list(),
  output_values = list()
) {
  selection <- lapply(names(completed_filters), function(dim) {
    dim_selection <- list(
      variableCode = dim,
      valueCodes = as.list(completed_filters[[dim]])
    )

    if (dim %in% names(codelists)) {
      dim_selection$codelist <- codelists[[dim]]
    }

    dim_selection
  })

  post_query_params <- list(
    lang = language,
    outputFormat = "json-stat2"
  )

  if (length(output_values) > 0) {
    for (nm in names(output_values)) {
      param_name <- paste0("outputValues[", nm, "]")
      post_query_params[[param_name]] <- output_values[[nm]]
    }
  }

  request_body <- list(selection = selection)

  list(
    table_id = table_id,
    endpoint = .build_data_endpoint(table_id),
    params = post_query_params,
    body = request_body,
    method = "POST"
  )
}

.build_metadata_request_spec <- function(table_id, language) {
  list(
    table_id = table_id,
    endpoint = paste0(
      "https://data.ssb.no/api/pxwebapi/v2/tables/",
      table_id,
      "/metadata"
    ),
    params = list(lang = language)
  )
}

.build_codelist_request_spec <- function(codelist_id, language) {
  list(
    endpoint = paste0(
      "https://data.ssb.no/api/pxwebapi/v2/codelists/",
      codelist_id
    ),
    params = list(lang = language),
    method = "GET"
  )
}

.build_config_request_spec <- function() {
  list(
    endpoint = "https://data.ssb.no/api/pxwebapi/v2/config",
    params = list(),
    method = "GET"
  )
}

.build_tables_request_spec <- function(query, language, page, page_size) {
  params <- list(
    lang = language,
    pageNumber = as.integer(page),
    pageSize = as.integer(page_size)
  )

  if (!is.null(query)) {
    params$query <- trimws(query)
  }

  list(
    endpoint = "https://data.ssb.no/api/pxwebapi/v2/tables",
    params = params,
    method = "GET"
  )
}

.execute_request_spec <- function(request_spec) {
  req <- httr2::request(request_spec$endpoint)

  method <- if (!is.null(request_spec$method)) request_spec$method else "GET"
  method <- toupper(method)

  if (method == "POST") {
    req <- httr2::req_method(req, "POST")
  }

  if (!is.null(request_spec$params) && length(request_spec$params) > 0) {
    req <- httr2::req_url_query(req, !!!request_spec$params)
  }

  if (
    method == "POST" &&
      !is.null(request_spec$body) &&
      length(request_spec$body) > 0
  ) {
    req <- httr2::req_body_json(req, request_spec$body)
  }

  req <- httr2::req_retry(
    req,
    max_tries = 3,
    retry_on_failure = TRUE,
    is_transient = .is_transient_ssb_response,
    after = .retry_after_seconds
  )

  query_summary <- .format_query_summary(request_spec$params)
  query_size <- .estimate_query_size(request_spec$params)

  tryCatch(
    httr2::req_perform(req),
    httr2_http = function(cnd) {
      status <- if (!is.null(cnd$status)) cnd$status else NA_integer_
      body <- if (!is.null(cnd$resp)) {
        .safe_response_body(cnd$resp)
      } else {
        "(no response body)"
      }

      large_url_hint <- ""
      if (
        method == "GET" &&
          !is.na(status) &&
          status == 404 &&
          grepl(
            "File or directory not found|resource you are looking",
            body,
            ignore.case = TRUE
          ) &&
          query_size > 1500
      ) {
        large_url_hint <- paste0(
          "\nPossible cause: query string is very large (about ",
          query_size,
          " bytes) and may exceed server/proxy URL limits."
        )
      }

      rlang::abort(
        paste0(
          "Request to SSB API failed.\n",
          "Status: ",
          status,
          "\n",
          "Method: ",
          method,
          "\n",
          "Endpoint: ",
          request_spec$endpoint,
          "\n",
          "Query: ",
          query_summary,
          "\n",
          "Response body: ",
          body,
          large_url_hint
        )
      )
    },
    httr2_failure = function(cnd) {
      rlang::abort(
        paste0(
          "Request to SSB API failed before receiving a response.\n",
          "Method: ",
          method,
          "\n",
          "Endpoint: ",
          request_spec$endpoint,
          "\n",
          "Query: ",
          query_summary,
          "\n",
          "Error: ",
          conditionMessage(cnd)
        )
      )
    },
    error = function(cnd) {
      if (inherits(cnd, "rlang_error")) {
        rlang::cnd_signal(cnd)
      }

      rlang::abort(
        paste0(
          "Unexpected request error.\n",
          "Method: ",
          method,
          "\n",
          "Endpoint: ",
          request_spec$endpoint,
          "\n",
          "Query: ",
          query_summary,
          "\n",
          "Error: ",
          conditionMessage(cnd)
        )
      )
    }
  )
}

.count_chunked_data_requests <- function(
  language,
  completed_filters,
  max_query_chars = 1500L
) {
  query_params <- .build_query_params(language, completed_filters)
  current_query_size <- .estimate_query_size(query_params)

  if (current_query_size <= max_query_chars) {
    return(1L)
  }

  splittable_dims <- names(completed_filters)[
    vapply(completed_filters, length, integer(1)) > 1L
  ]

  if (length(splittable_dims) == 0) {
    return(1L)
  }

  split_dim <- splittable_dims[[which.max(vapply(
    completed_filters[splittable_dims],
    length,
    integer(1)
  ))]]
  dim_values <- completed_filters[[split_dim]]
  n_values <- length(dim_values)
  split_at <- ceiling(n_values / 2)

  left_filters <- completed_filters
  right_filters <- completed_filters

  left_filters[[split_dim]] <- dim_values[seq_len(split_at)]
  right_filters[[split_dim]] <- dim_values[(split_at + 1L):n_values]

  .count_chunked_data_requests(
    language,
    left_filters,
    max_query_chars = max_query_chars
  ) +
    .count_chunked_data_requests(
      language,
      right_filters,
      max_query_chars = max_query_chars
    )
}

.execute_chunked_data_requests <- function(
  table_id,
  language,
  completed_filters,
  max_query_chars = 1500L,
  progress_env = NULL
) {
  query_params <- .build_query_params(language, completed_filters)
  current_query_size <- .estimate_query_size(query_params)

  if (current_query_size <= max_query_chars) {
    request_spec <- .build_request_spec(table_id, language, completed_filters)
    resp <- .execute_request_spec(request_spec)

    if (!is.null(progress_env)) {
      progress_env$done <- progress_env$done + 1L
      utils::setTxtProgressBar(progress_env$bar, progress_env$done)
    }

    return(list(.parse_response_json(resp)))
  }

  splittable_dims <- names(completed_filters)[
    vapply(completed_filters, length, integer(1)) > 1L
  ]

  if (length(splittable_dims) == 0) {
    rlang::abort(
      paste0(
        "Query parameters are still too large for a GET request and cannot be split further. ",
        "Please provide narrower filters manually."
      )
    )
  }

  split_dim <- splittable_dims[[which.max(vapply(
    completed_filters[splittable_dims],
    length,
    integer(1)
  ))]]
  dim_values <- completed_filters[[split_dim]]
  n_values <- length(dim_values)
  split_at <- ceiling(n_values / 2)

  left_filters <- completed_filters
  right_filters <- completed_filters

  left_filters[[split_dim]] <- dim_values[seq_len(split_at)]
  right_filters[[split_dim]] <- dim_values[(split_at + 1L):n_values]

  c(
    .execute_chunked_data_requests(
      table_id,
      language,
      left_filters,
      max_query_chars = max_query_chars,
      progress_env = progress_env
    ),
    .execute_chunked_data_requests(
      table_id,
      language,
      right_filters,
      max_query_chars = max_query_chars,
      progress_env = progress_env
    )
  )
}
