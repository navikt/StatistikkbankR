test_that(".build_data_endpoint constructs correct URL", {
  result <- StatistikkbankR:::.build_data_endpoint("12452")
  expect_equal(result, "https://data.ssb.no/api/pxwebapi/v2/tables/12452/data")
})

test_that(".build_data_endpoint handles various table IDs", {
  result <- StatistikkbankR:::.build_data_endpoint("00000")
  expect_equal(result, "https://data.ssb.no/api/pxwebapi/v2/tables/00000/data")
  
  result <- StatistikkbankR:::.build_data_endpoint("99999")
  expect_equal(result, "https://data.ssb.no/api/pxwebapi/v2/tables/99999/data")
})

test_that(".estimate_query_size returns 0 for empty params", {
  result <- StatistikkbankR:::.estimate_query_size(list())
  expect_equal(result, 0L)
})

test_that(".estimate_query_size encodes single parameter", {
  params <- list(Region = c("Region_1"))
  result <- StatistikkbankR:::.estimate_query_size(params)
  expect_true(is.integer(result))
  expect_true(result > 0L)
})

test_that(".estimate_query_size encodes multiple parameters", {
  params <- list(Region = c("Region_1", "Region_2"), Tid = c("2020Q1", "2021Q1"))
  result <- StatistikkbankR:::.estimate_query_size(params)
  expect_true(is.integer(result))
  expect_true(result > 0L)
})

test_that(".estimate_query_size returns byte count", {
  params <- list(Region = c("Region_1"))
  result <- StatistikkbankR:::.estimate_query_size(params)
  expect_equal(class(result), "integer")
})

test_that(".estimate_query_size handles special characters requiring encoding", {
  params <- list(Navn = c("Børre & Co."))
  result <- StatistikkbankR:::.estimate_query_size(params)
  expect_true(result > 0L)
})

test_that(".format_query_summary returns NULL for empty params", {
  result <- StatistikkbankR:::.format_query_summary(list())
  expect_null(result)
})

test_that(".format_query_summary formats single parameter", {
  params <- list(Region = c("Region_1"))
  result <- StatistikkbankR:::.format_query_summary(params)
  expect_equal(result, "Region=Region_1")
})

test_that(".format_query_summary formats multiple parameters", {
  params <- list(Region = c("Region_1"), Tid = c("2020Q1"))
  result <- StatistikkbankR:::.format_query_summary(params)
  expect_match(result, "Region=Region_1")
  expect_match(result, "Tid=2020Q1")
})

test_that(".format_query_summary collapses multiple values", {
  params <- list(Region = c("Region_1", "Region_2", "Region_3"))
  result <- StatistikkbankR:::.format_query_summary(params)
  expect_equal(result, "Region=Region_1,Region_2,Region_3")
})

test_that(".format_query_summary truncates long values", {
  params <- list(LongParam = paste0(rep("x", 150), collapse = ""))
  result <- StatistikkbankR:::.format_query_summary(params, max_chars = 80)
  expect_match(result, "LongParam=.*\\.\\.\\.")
  expect_true(nchar(result) < 100)
})

test_that(".format_query_summary respects max_chars parameter", {
  params <- list(Region = c("Region_1", "Region_2"))
  result_80 <- StatistikkbankR:::.format_query_summary(params, max_chars = 80)
  result_20 <- StatistikkbankR:::.format_query_summary(params, max_chars = 20)
  expect_true(nchar(result_20) <= nchar(result_80))
})


