make_get_ssb_api_config_stub <- function() {
  list(
    max_data_cells = 100000L,
    max_calls_per_time_window = NA_integer_,
    time_window = NA_integer_,
    default_data_format = "json-stat2",
    data_formats = c("json-stat2", "csv")
  )
}

make_get_ssb_metadata_stub <- function() {
  list(
    table_id = "07459",
    dimensions = list(
      Tid = list(codes = c("2024K1", "2024K2"), code_count = 2L),
      Region = list(codes = c("0301"), code_count = 1L)
    )
  )
}

make_get_ssb_response_metadata_stub <- function() {
  list(
    table_id = "12452",
    roles = list(time = list("Tid")),
    dimensions = list(
      Yrke = list(codes = c("0-9", "1"), code_count = 2L),
      Tid = list(codes = c("2015K1", "2015K2"), code_count = 2L)
    )
  )
}

make_get_ssb_parsed_response_stub <- function() {
  list(
    id = list("Yrke", "Tid"),
    dimension = list(
      Yrke = list(
        category = list(
          index = list(`0-9` = 0, `1` = 1),
          label = list(`0-9` = "Alle yrker", `1` = "Ledere")
        )
      ),
      Tid = list(
        category = list(
          index = list(`2015K1` = 0, `2015K2` = 1),
          label = list(`2015K1` = "2015K1", `2015K2` = "2015K2")
        )
      )
    ),
    value = list(5.3, 4.8, 3.5, 3.3)
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
    .normalise_codelists = function(x) if (is.null(x)) list() else x,
    .normalise_output_values = function(x) if (is.null(x)) list() else x,
    .get_api_config = function(...) make_get_ssb_api_config_stub(),
    .get_table_metadata = function(...) make_get_ssb_metadata_stub()
  )
}

run_with_common_get_ssb_data_mocks <- function(code, overrides = list()) {
  ns <- asNamespace("StatistikkbankR")
  replacements <- make_common_get_ssb_data_mocks()
  if (length(overrides) > 0) {
    replacements[names(overrides)] <- overrides
  }
  replacement_names <- names(replacements)

  originals <- lapply(replacement_names, function(nm) get(nm, envir = ns, inherits = FALSE))
  names(originals) <- replacement_names
  was_locked <- vapply(replacement_names, function(nm) bindingIsLocked(nm, ns), logical(1))

  on.exit({
    for (nm in rev(replacement_names)) {
      if (bindingIsLocked(nm, ns)) {
        unlockBinding(nm, ns)
      }
      assign(nm, originals[[nm]], envir = ns)
      if (was_locked[[nm]]) {
        lockBinding(nm, ns)
      }
    }
  }, add = TRUE)

  for (nm in replacement_names) {
    if (bindingIsLocked(nm, ns)) {
      unlockBinding(nm, ns)
    }
    assign(nm, replacements[[nm]], envir = ns)
    if (was_locked[[nm]]) {
      lockBinding(nm, ns)
    }
  }

  eval(substitute(code), parent.frame())
}

expect_namespace_bindings_restored <- function(binding_names, expr) {
  ns <- asNamespace("StatistikkbankR")
  before <- lapply(binding_names, function(nm) get(nm, envir = ns, inherits = FALSE))
  names(before) <- binding_names

  eval.parent(substitute(expr))

  after <- lapply(binding_names, function(nm) get(nm, envir = ns, inherits = FALSE))
  names(after) <- binding_names

  for (nm in binding_names) {
    testthat::expect_identical(after[[nm]], before[[nm]])
  }
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

test_that("get_ssb_data preserves JSON-stat observation order in tidy output", {
  run_with_common_get_ssb_data_mocks(
    {
      out <- StatistikkbankR::get_ssb_data(
        "12452",
        as_tibble = FALSE,
        character_as_factor = FALSE,
        convert_quarter_to_yearqtr = FALSE
      )

      expect_equal(out$Yrke_code, c("0-9", "0-9", "1", "1"))
      expect_equal(out$Tid_code, c("2015K1", "2015K2", "2015K1", "2015K2"))
      expect_equal(out$value, c(5.3, 4.8, 3.5, 3.3))
    },
    overrides = list(
      .get_table_metadata = function(...) make_get_ssb_response_metadata_stub(),
      .estimate_row_count = function(...) 4L,
      .build_query_params = function(...) list(lang = "no"),
      .estimate_query_size = function(...) 10L,
      .build_request_spec = function(...) list(method = "GET"),
      .execute_request_spec = function(request_spec) request_spec,
      .parse_response_json = function(resp) make_get_ssb_parsed_response_stub()
    )
  )
})

test_that("mock harness restores namespace bindings after errors", {
  tracked_bindings <- c(
    ".build_query_params",
    ".get_api_config",
    ".get_table_metadata"
  )

  expect_namespace_bindings_restored(tracked_bindings, {
    expect_error(
      run_with_common_get_ssb_data_mocks(
        {
          stop("boom")
        },
        overrides = list(
          .build_query_params = function(...) list(lang = "no")
        )
      ),
      "boom"
    )
  })
})
