test_that("ssb_search rejects empty string query", {
  expect_error(ssb_search(""), "non-empty")
})

test_that("ssb_search rejects whitespace-only query", {
  expect_error(ssb_search("   "), "non-empty")
})

test_that("ssb_search rejects invalid language", {
  expect_error(ssb_search(language = "fr"), "\"no\" or \"en\"")
})

test_that("ssb_search rejects non-positive page", {
  expect_error(ssb_search(page = 0L), "positive integer")
  expect_error(ssb_search(page = -1L), "positive integer")
})

test_that("ssb_search rejects out-of-range page_size", {
  expect_error(ssb_search(page_size = 0L), "1 and 1000")
  expect_error(ssb_search(page_size = 1001L), "1 and 1000")
})

test_that("ssb_search rejects non-bool fetch_all", {
  expect_error(ssb_search(fetch_all = "yes"), "TRUE or FALSE")
})

test_that("ssb_search rejects non-bool as_tibble", {
  expect_error(ssb_search(as_tibble = "yes"), "TRUE or FALSE")
})

test_that("ssb_search accepts valid min page_size", {
  run_with_exported_helper_mocks(
    {
      result <- StatistikkbankR::ssb_search(page_size = 1L, as_tibble = FALSE)
      expect_s3_class(result, "data.frame")
    },
    overrides = list(
      .execute_request_spec = function(request_spec) list(),
      .parse_response_json = function(resp) list(
        tables = list(),
        page = list(pageNumber = 1L, pageSize = 1L, totalElements = 0L, totalPages = 0L)
      )
    )
  )
})

test_that("ssb_search returns data.frame when as_tibble=FALSE", {
  run_with_exported_helper_mocks(
    {
      result <- StatistikkbankR::ssb_search(as_tibble = FALSE)
      expect_s3_class(result, "data.frame")
      expect_false(inherits(result, "tbl_df"))
    },
    overrides = list(
      .execute_request_spec = function(request_spec) list(),
      .parse_response_json = function(resp) list(
        tables = list(),
        page = list(pageNumber = 1L, pageSize = 20L, totalElements = 0L, totalPages = 0L)
      )
    )
  )
})

test_that("ssb_search returns tibble by default", {
  run_with_exported_helper_mocks(
    {
      result <- StatistikkbankR::ssb_search()
      expect_true(inherits(result, "tbl_df"))
    },
    overrides = list(
      .execute_request_spec = function(request_spec) list(),
      .parse_response_json = function(resp) list(
        tables = list(),
        page = list(pageNumber = 1L, pageSize = 20L, totalElements = 0L, totalPages = 0L)
      )
    )
  )
})

test_that("ssb_search sets page attributes on result", {
  run_with_exported_helper_mocks(
    {
      result <- StatistikkbankR::ssb_search()
      expect_equal(attr(result, "page_number"), 1L)
      expect_equal(attr(result, "total_elements"), 0L)
    },
    overrides = list(
      .execute_request_spec = function(request_spec) list(),
      .parse_response_json = function(resp) list(
        tables = list(),
        page = list(pageNumber = 1L, pageSize = 20L, totalElements = 0L, totalPages = 0L)
      )
    )
  )
})
