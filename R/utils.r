.metadata_cache <- new.env(parent = emptyenv())
.config_cache <- new.env(parent = emptyenv())

.build_data_endpoint <- function(table_id) {
  paste0(
    "https://data.ssb.no/api/pxwebapi/v2/tables/",
    table_id,
    "/data"
  )
}

.estimate_query_size <- function(params) {
  if (length(params) == 0) {
    return(0L)
  }

  encoded_names <- utils::URLencode(names(params), reserved = TRUE)
  encoded_values <- vapply(
    params,
    function(value) {
      value_chr <- paste(as.character(value), collapse = ",")
      utils::URLencode(value_chr, reserved = TRUE)
    },
    character(1)
  )

  pieces <- paste0(encoded_names, "=", encoded_values)
  as.integer(nchar(paste(pieces, collapse = "&"), type = "bytes"))
}

.format_query_summary <- function(params, max_chars = 80) {
  if (length(params) == 0) {
    return(invisible(NULL))
  }
  pieces <- vapply(
    names(params),
    function(nm) {
      value <- paste(as.character(params[[nm]]), collapse = ",")
      if (nchar(value) > max_chars) {
        value <- paste0(substr(value, 1, max_chars - 3), "...")
      }
      paste0(nm, "=", value)
    },
    character(1)
  )

  paste(pieces, collapse = "&")
}

.safe_response_body <- function(resp) {
  tryCatch(
    httr2::resp_body_string(resp),
    error = function(...) "(unable to read response body)"
  )
}

.is_transient_ssb_response <- function(resp) {
  status <- tryCatch(httr2::resp_status(resp), error = function(...) NA_integer_)
  status %in% c(429L, 503L)
}

.retry_after_seconds <- function(resp) {
  retry_after <- tryCatch(
    httr2::resp_header(resp, "Retry-After"),
    error = function(...) NULL
  )

  if (is.null(retry_after) || length(retry_after) == 0 || is.na(retry_after)) {
    return(NULL)
  }

  retry_after_numeric <- suppressWarnings(as.numeric(retry_after))
  if (is.na(retry_after_numeric) || retry_after_numeric < 0) {
    return(NULL)
  }

  retry_after_numeric
}
