test_that(".build_query_params includes lang and outputFormat as json-stat2", {
  result <- .build_query_params("no", list())
  expect_equal(result$lang, "no")
  expect_equal(result$outputFormat, "json-stat2")
})

test_that(".build_query_params encodes value codes correctly", {
  result <- .build_query_params("no", list(Tid = c("2020", "2021")))
  expect_equal(result[["valueCodes[Tid]"]], "2020,2021")
})

test_that(".build_query_params encodes codelists", {
  result <- .build_query_params("no", list(Tid = "2024"), codelists = list(Region = "agg_foo"))
  expect_equal(result[["codelist[Region]"]], "agg_foo")
})

test_that(".build_query_params encodes output_values", {
  result <- .build_query_params("no", list(Tid = "2024"), output_values = list(Region = "aggregated"))
  expect_equal(result[["outputValues[Region]"]], "aggregated")
})

test_that(".estimate_query_size returns 0 for empty params", {
  expect_equal(.estimate_query_size(list()), 0L)
})

test_that(".estimate_query_size returns positive integer for non-empty params", {
  result <- .estimate_query_size(list(lang = "no", outputFormat = "json-stat2"))
  expect_true(result > 0L)
  expect_type(result, "integer")
})

test_that(".estimate_query_size increases with more filters", {
  small <- .estimate_query_size(list(lang = "no"))
  large <- .estimate_query_size(list(lang = "no", `valueCodes[Tid]` = paste(1:100, collapse = ",")))
  expect_true(large > small)
})

test_that(".build_request_spec produces GET spec with correct structure", {
  spec <- .build_request_spec("07459", "no", list(Tid = c("2020", "2021")))
  expect_equal(spec$method, "GET")
  expect_true(grepl("07459/data", spec$endpoint))
  expect_equal(spec$params$lang, "no")
  expect_equal(spec$params[["valueCodes[Tid]"]], "2020,2021")
})

test_that(".build_post_request_spec produces POST spec with body", {
  spec <- .build_post_request_spec(
    "07459", "no",
    list(Tid = c("2020", "2021"), Region = c("0301"))
  )
  expect_equal(spec$method, "POST")
  expect_true(grepl("07459/data", spec$endpoint))
  expect_true(!is.null(spec$body$selection))
  selection_dims <- vapply(spec$body$selection, function(x) x$variableCode, character(1))
  expect_true("Tid" %in% selection_dims)
  expect_true("Region" %in% selection_dims)
})

test_that(".build_post_request_spec omits placement (placement removed)", {
  spec <- .build_post_request_spec("07459", "no", list(Tid = "2020"))
  expect_null(spec$body$placement)
})

test_that(".build_post_request_spec includes codelist in selection entry", {
  spec <- .build_post_request_spec(
    "07459", "no",
    list(Region = "*"),
    codelists = list(Region = "agg_KommSummer")
  )
  region_entry <- Filter(function(x) x$variableCode == "Region", spec$body$selection)[[1]]
  expect_equal(region_entry$codelist, "agg_KommSummer")
})

test_that(".build_post_request_spec encodes output_values in query params", {
  spec <- .build_post_request_spec(
    "07459", "no",
    list(Region = c("0301", "0302")),
    output_values = list(Region = "aggregated")
  )

  expect_equal(spec$params[["outputValues[Region]"]], "aggregated")
  expect_equal(spec$params$outputFormat, "json-stat2")
})

test_that(".build_data_endpoint constructs correct URL", {
  expect_equal(
    .build_data_endpoint("07459"),
    "https://data.ssb.no/api/pxwebapi/v2/tables/07459/data"
  )
})

test_that(".build_tables_request_spec builds correct endpoint and params", {
  spec <- .build_tables_request_spec("befolkning", "no", 1L, 20L)
  expect_equal(spec$endpoint, "https://data.ssb.no/api/pxwebapi/v2/tables")
  expect_equal(spec$params$query, "befolkning")
  expect_equal(spec$params$lang, "no")
  expect_equal(spec$params$pageNumber, 1L)
  expect_equal(spec$params$pageSize, 20L)
})

test_that(".build_tables_request_spec omits query param when NULL", {
  spec <- .build_tables_request_spec(NULL, "no", 1L, 20L)
  expect_null(spec$params$query)
})

test_that(".count_chunked_data_requests returns 1 when query size is below limit", {
  out <- testthat::with_mocked_bindings(
    .count_chunked_data_requests(
      "no",
      list(Tid = c("2020", "2021")),
      max_query_chars = 100L
    ),
    .build_query_params = function(...) list(lang = "no"),
    .estimate_query_size = function(...) 10L,
    .package = "StatistikkbankR"
  )

  expect_equal(out, 1L)
})

test_that(".count_chunked_data_requests splits recursively on largest dimension", {
  out <- testthat::with_mocked_bindings(
    .count_chunked_data_requests(
      "no",
      list(Tid = c("2020", "2021", "2022", "2023")),
      max_query_chars = 100L
    ),
    .build_query_params = function(language, completed_filters, ...) completed_filters,
    .estimate_query_size = function(query_params) {
      if (length(query_params$Tid) > 1L) {
        return(9999L)
      }
      10L
    },
    .package = "StatistikkbankR"
  )

  expect_equal(out, 4L)
})

test_that(".execute_chunked_data_requests aborts when query is too large and unsplittable", {
  expect_error(
    testthat::with_mocked_bindings(
      .execute_chunked_data_requests(
        "07459",
        "no",
        list(Tid = "2020"),
        max_query_chars = 100L
      ),
      .build_query_params = function(language, completed_filters, ...) completed_filters,
      .estimate_query_size = function(...) 9999L,
      .package = "StatistikkbankR"
    ),
    "cannot be split further"
  )
})

test_that(".execute_chunked_data_requests returns parsed chunks from recursive splits", {
  out <- testthat::with_mocked_bindings(
    .execute_chunked_data_requests(
      "07459",
      "no",
      list(Tid = c("2020", "2021", "2022", "2023")),
      max_query_chars = 100L
    ),
    .build_query_params = function(language, completed_filters, ...) completed_filters,
    .estimate_query_size = function(query_params) {
      if (length(query_params$Tid) > 1L) {
        return(9999L)
      }
      10L
    },
    .build_request_spec = function(table_id, language, completed_filters, ...) {
      list(method = "GET", filters = completed_filters)
    },
    .execute_request_spec = function(request_spec) request_spec,
    .parse_response_json = function(resp) list(values = resp$filters$Tid),
    .package = "StatistikkbankR"
  )

  expect_length(out, 4L)
  expect_true(all(vapply(out, function(x) length(x$values), integer(1)) == 1L))
})
