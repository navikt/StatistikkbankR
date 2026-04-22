test_that(".build_query_params includes language and outputFormat", {
  result <- StatistikkbankR:::.build_query_params("no", list())
  expect_equal(result$lang, "no")
  expect_equal(result$outputFormat, "json-stat2")
})

test_that(".build_query_params includes filters", {
  filters <- list(Region = c("01", "02"), Tid = "2023")
  result <- StatistikkbankR:::.build_query_params("no", filters)
  expect_equal(result$`valueCodes[Region]`, "01,02")
  expect_equal(result$`valueCodes[Tid]`, "2023")
})

test_that(".build_query_params collapses multiple filter values", {
  filters <- list(Region = c("01", "02", "03"))
  result <- StatistikkbankR:::.build_query_params("no", filters)
  expect_equal(result$`valueCodes[Region]`, "01,02,03")
})

test_that(".build_query_params includes codelists with filters", {
  filters <- list(Region = c("01"))
  codelists <- list(Tid = "codelist_123")
  result <- StatistikkbankR:::.build_query_params("no", filters, codelists = codelists)
  expect_equal(result$`codelist[Tid]`, "codelist_123")
})

test_that(".build_query_params includes output_values with filters", {
  filters <- list(Region = c("01"))
  output_values <- list(Tid = "single")
  result <- StatistikkbankR:::.build_query_params("no", filters, output_values = output_values)
  expect_equal(result$`outputValues[Tid]`, "single")
})

test_that(".build_query_params handles language variants", {
  result_no <- StatistikkbankR:::.build_query_params("no", list())
  result_en <- StatistikkbankR:::.build_query_params("en", list())
  expect_equal(result_no$lang, "no")
  expect_equal(result_en$lang, "en")
})

test_that(".build_request_spec builds correct structure", {
  result <- StatistikkbankR:::.build_request_spec("12452", "no", list())
  expect_named(result, c("table_id", "endpoint", "params", "method"))
  expect_equal(result$table_id, "12452")
  expect_match(result$endpoint, "12452")
  expect_equal(result$method, "GET")
})

test_that(".build_metadata_request_spec includes metadata endpoint", {
  result <- StatistikkbankR:::.build_metadata_request_spec("12452", "no")
  expect_match(result$endpoint, "metadata")
  expect_equal(result$params$lang, "no")
})

test_that(".build_codelist_request_spec builds codelist endpoint", {
  result <- StatistikkbankR:::.build_codelist_request_spec("CODELIST123", "no")
  expect_match(result$endpoint, "CODELIST123")
  expect_match(result$endpoint, "codelists")
  expect_equal(result$params$lang, "no")
})

test_that(".build_config_request_spec returns config endpoint", {
  result <- StatistikkbankR:::.build_config_request_spec()
  expect_match(result$endpoint, "config")
  expect_equal(result$method, "GET")
})

test_that(".build_tables_request_spec includes query parameter", {
  result <- StatistikkbankR:::.build_tables_request_spec("employment", "no", 1, 25)
  expect_equal(result$params$query, "employment")
  expect_equal(result$params$pageNumber, 1L)
  expect_equal(result$params$pageSize, 25L)
})

test_that(".build_tables_request_spec trims query whitespace", {
  result <- StatistikkbankR:::.build_tables_request_spec("  employment  ", "no", 1, 25)
  expect_equal(result$params$query, "employment")
})

test_that(".build_tables_request_spec handles NULL query", {
  result <- StatistikkbankR:::.build_tables_request_spec(NULL, "no", 1, 25)
  expect_null(result$params$query)
})

test_that(".build_tables_request_spec coerces page numbers to integers", {
  result <- StatistikkbankR:::.build_tables_request_spec(NULL, "no", 1.9, 25.8)
  expect_equal(result$params$pageNumber, 1L)
  expect_equal(result$params$pageSize, 25L)
})

test_that(".build_post_request_spec builds POST structure", {
  filters <- list(Region = c("01", "02"))
  result <- StatistikkbankR:::.build_post_request_spec("12452", "no", filters)
  expect_named(result, c("table_id", "endpoint", "params", "body", "method"))
  expect_equal(result$method, "POST")
})

test_that(".build_post_request_spec creates selection from filters", {
  filters <- list(Region = c("01", "02"), Tid = "2023")
  result <- StatistikkbankR:::.build_post_request_spec("12452", "no", filters)
  expect_equal(length(result$body$selection), 2)
  expect_equal(result$body$selection[[1]]$variableCode, "Region")
  expect_equal(result$body$selection[[1]]$valueCodes, list("01", "02"))
})

test_that(".build_post_request_spec includes codelists in selection", {
  filters <- list(Region = c("01"))
  codelists <- list(Region = "CODELIST123")
  result <- StatistikkbankR:::.build_post_request_spec("12452", "no", filters, codelists)
  expect_equal(result$body$selection[[1]]$codelist, "CODELIST123")
})

test_that(".build_post_request_spec includes output_values in params", {
  filters <- list(Region = c("01"))
  output_values <- list(Region = "single")
  result <- StatistikkbankR:::.build_post_request_spec("12452", "no", filters, output_values = output_values)
  expect_equal(result$params$`outputValues[Region]`, "single")
})

test_that(".build_query_params with all parameters", {
  filters <- list(Region = c("01", "02"))
  codelists <- list(Tid = "TID123")
  output_values <- list(Region = "single")
  result <- StatistikkbankR:::.build_query_params("en", filters, codelists, output_values)
  
  expect_equal(result$lang, "en")
  expect_equal(result$`valueCodes[Region]`, "01,02")
  expect_equal(result$`codelist[Tid]`, "TID123")
  expect_equal(result$`outputValues[Region]`, "single")
})

test_that(".build_request_spec passes through all parameters", {
  filters <- list(Region = c("01"))
  codelists <- list(Tid = "TID123")
  result <- StatistikkbankR:::.build_request_spec("12452", "no", filters, codelists)
  
  expect_equal(result$params$`valueCodes[Region]`, "01")
  expect_equal(result$params$`codelist[Tid]`, "TID123")
})

test_that(".build_post_request_spec converts values to lists", {
  filters <- list(Region = c("01", "02", "03"))
  result <- StatistikkbankR:::.build_post_request_spec("12452", "no", filters)
  
  expect_true(is.list(result$body$selection[[1]]$valueCodes))
  expect_equal(length(result$body$selection[[1]]$valueCodes), 3)
})

test_that(".build_tables_request_spec with empty query string", {
  result <- StatistikkbankR:::.build_tables_request_spec("", "no", 1, 25)
  expect_equal(result$params$query, "")
})

test_that(".build_tables_request_spec with whitespace-only query", {
  result <- StatistikkbankR:::.build_tables_request_spec("   ", "no", 1, 25)
  expect_equal(result$params$query, "")
})
