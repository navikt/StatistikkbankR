make_api_config_stub <- function() {
  list(
    max_data_cells = 100000L,
    max_calls_per_time_window = NA_integer_,
    time_window = NA_integer_,
    default_data_format = "json-stat2",
    data_formats = c("json-stat2", "csv")
  )
}

make_metadata_stub <- function() {
  list(
    table_id = "07459",
    dimensions = list(
      Tid = list(codes = c("2024K1", "2024K2"), code_count = 2L),
      Region = list(codes = c("0301"), code_count = 1L)
    )
  )
}

make_common_get_ssb_data_mocks <- function() {
  list(
    .validate_main_args = function(...) NULL,
    .validate_metadata_cache_args = function(...) NULL,
    .validate_chunking_args = function(...) NULL,
    .validate_filters = function(...) NULL,
    .validate_codelists = function(...) NULL,
    .validate_output_values = function(...) NULL,
    .validate_filter_names_against_metadata = function(...) NULL,
    .validate_codelist_names_against_metadata = function(...) NULL,
    .validate_output_value_names_against_metadata = function(...) NULL,
    .validate_output_values_against_codelists = function(...) NULL,
    .validate_filter_values_against_metadata = function(...) NULL,
    .normalise_filters = function(filters) filters,
    .normalise_quarter_as = function(x) tolower(trimws(x)),
    .normalise_codelists = function(x) if (is.null(x)) list() else x,
    .normalise_output_values = function(x) if (is.null(x)) list() else x,
    .get_api_config = function(...) make_api_config_stub(),
    .get_table_metadata = function(...) make_metadata_stub(),
    .auto_complete_filters = function(metadata, normalised_filters, normalised_codelists = list()) {
      list(Tid = c("2024K1", "2024K2"), Region = c("0301"))
    }
  )
}

run_with_common_get_ssb_data_mocks <- function(code, overrides = list()) {
  common <- make_common_get_ssb_data_mocks()
  args <- c(list(code = substitute(code), .package = "StatistikkbankR"), common, overrides)
  do.call(testthat::with_mocked_bindings, args)
}

test_that("get_ssb_data uses POST request path when query exceeds max_get_query_chars", {
  run_with_common_get_ssb_data_mocks(
    {
      out <- StatistikkbankR::get_ssb_data("07459", max_get_query_chars = 100L)
      expect_equal(out, "POST")
    },
    overrides = list(
      .estimate_row_count = function(...) 2L,
      .build_query_params = function(...) list(lang = "no", `valueCodes[Tid]` = paste(rep("x", 200), collapse = ",")),
      .estimate_query_size = function(...) 5000L,
      .build_post_request_spec = function(...) {
        list(method = "POST")
      },
      .execute_request_spec = function(request_spec) request_spec,
      .parse_response_json = function(resp) resp,
      .tidy_response_json = function(parsed_json, ...) parsed_json$method,
      .build_request_spec = function(...) stop("GET path should not be called")
    )
  )
})

test_that("get_ssb_data always returns tidy output via GET path", {
  run_with_common_get_ssb_data_mocks(
    {
      out <- StatistikkbankR::get_ssb_data("07459", max_get_query_chars = 99999L)
      expect_equal(out, "tidy")
    },
    overrides = list(
      .estimate_row_count = function(...) 2L,
      .build_query_params = function(...) list(lang = "no"),
      .estimate_query_size = function(...) 10L,
      .build_request_spec = function(...) list(method = "GET"),
      .execute_request_spec = function(request_spec) request_spec,
      .parse_response_json = function(resp) resp,
      .tidy_response_json = function(parsed_json, ...) "tidy",
      .build_post_request_spec = function(...) stop("POST path should not be called")
    )
  )
})

test_that("get_ssb_data aborts when estimated rows exceed API limit", {
  run_with_common_get_ssb_data_mocks(
    {
      expect_error(
        StatistikkbankR::get_ssb_data("07459"),
        "exceeds the API-reported limit"
      )
    },
    overrides = list(
      .get_api_config = function(...) {
        list(
          max_data_cells = 10L,
          max_calls_per_time_window = 100L,
          time_window = 60L
        )
      },
      .estimate_row_count = function(...) 50L,
      .build_query_params = function(...) stop("request path should not be reached")
    )
  )
})

test_that("get_ssb_data bypasses large-query guard when override_large_query is TRUE", {
  run_with_common_get_ssb_data_mocks(
    {
      out <- StatistikkbankR::get_ssb_data("07459", override_large_query = TRUE)
      expect_equal(out, "GET")
    },
    overrides = list(
      .get_api_config = function(...) {
        list(
          max_data_cells = 10L,
          max_calls_per_time_window = NA_integer_,
          time_window = NA_integer_
        )
      },
      .estimate_row_count = function(...) 50L,
      .build_query_params = function(...) list(lang = "no"),
      .estimate_query_size = function(...) 10L,
      .build_request_spec = function(...) list(method = "GET"),
      .execute_request_spec = function(request_spec) request_spec,
      .parse_response_json = function(resp) resp,
      .tidy_response_json = function(parsed_json, ...) parsed_json$method,
      .build_post_request_spec = function(...) stop("POST path should not be called")
    )
  )
})

test_that("get_ssb_data forwards codelists and output_values to GET request builder", {
  run_with_common_get_ssb_data_mocks(
    {
      out <- StatistikkbankR::get_ssb_data(
        "07459",
        codelists = list(Region = "agg_KommSummer"),
        output_values = list(Region = "aggregated"),
        max_get_query_chars = 99999L
      )
      expect_equal(out$codelists$Region, "agg_KommSummer")
      expect_equal(out$output_values$Region, "aggregated")
    },
    overrides = list(
      .estimate_row_count = function(...) 2L,
      .build_query_params = function(...) list(lang = "no"),
      .estimate_query_size = function(...) 10L,
      .build_request_spec = function(
        table_id,
        language,
        normalised_filters,
        codelists = list(),
        output_values = list()
      ) {
        list(
          method = "GET",
          codelists = codelists,
          output_values = output_values
        )
      },
      .execute_request_spec = function(request_spec) request_spec,
      .parse_response_json = function(resp) resp,
      .tidy_response_json = function(parsed_json, ...) parsed_json,
      .build_post_request_spec = function(...) stop("POST path should not be called")
    )
  )
})

test_that("get_ssb_data forwards codelists and output_values to POST request builder", {
  run_with_common_get_ssb_data_mocks(
    {
      out <- StatistikkbankR::get_ssb_data(
        "07459",
        codelists = list(Region = "agg_KommSummer"),
        output_values = list(Region = "single"),
        max_get_query_chars = 10L
      )
      expect_equal(out$codelists$Region, "agg_KommSummer")
      expect_equal(out$output_values$Region, "single")
    },
    overrides = list(
      .estimate_row_count = function(...) 2L,
      .build_query_params = function(...) list(lang = "no"),
      .estimate_query_size = function(...) 5000L,
      .build_post_request_spec = function(
        table_id,
        language,
        completed_filters,
        codelists = list(),
        output_values = list()
      ) {
        list(
          method = "POST",
          codelists = codelists,
          output_values = output_values
        )
      },
      .execute_request_spec = function(request_spec) request_spec,
      .parse_response_json = function(resp) resp,
      .tidy_response_json = function(parsed_json, ...) parsed_json,
      .build_request_spec = function(...) stop("GET path should not be called")
    )
  )
})
