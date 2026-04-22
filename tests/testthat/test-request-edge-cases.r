test_that(".build_query_params with multiple codelists", {
  filters <- list(Region = c("01"))
  codelists <- list(Region = "CODELIST_A", Tid = "CODELIST_B")
  result <- StatistikkbankR:::.build_query_params("no", filters, codelists = codelists)
  expect_equal(result$`codelist[Region]`, "CODELIST_A")
  expect_equal(result$`codelist[Tid]`, "CODELIST_B")
})

test_that(".build_query_params with multiple output_values", {
  filters <- list(Region = c("01"))
  output_values <- list(Region = "single", Tid = "aggregated")
  result <- StatistikkbankR:::.build_query_params("no", filters, output_values = output_values)
  expect_equal(result$`outputValues[Region]`, "single")
  expect_equal(result$`outputValues[Tid]`, "aggregated")
})

test_that(".build_post_request_spec with no codelists for non-codelist dimension", {
  filters <- list(Region = c("01"), Tid = "2023")
  codelists <- list(Tid = "CODELIST123")
  result <- StatistikkbankR:::.build_post_request_spec("12452", "no", filters, codelists)
  
  region_sel <- Filter(function(s) s$variableCode == "Region", result$body$selection)
  expect_null(region_sel[[1]]$codelist)
  
  tid_sel <- Filter(function(s) s$variableCode == "Tid", result$body$selection)
  expect_equal(tid_sel[[1]]$codelist, "CODELIST123")
})

test_that(".build_metadata_request_spec with English language", {
  result <- StatistikkbankR:::.build_metadata_request_spec("12452", "en")
  expect_equal(result$params$lang, "en")
})

test_that(".build_codelist_request_spec with English language", {
  result <- StatistikkbankR:::.build_codelist_request_spec("CODELIST_NO_FYLKE", "en")
  expect_equal(result$params$lang, "en")
  expect_equal(result$method, "GET")
})

test_that(".build_tables_request_spec returns GET method", {
  result <- StatistikkbankR:::.build_tables_request_spec("query", "no", 1, 25)
  expect_equal(result$method, "GET")
})

test_that(".build_tables_request_spec sets the correct endpoint", {
  result <- StatistikkbankR:::.build_tables_request_spec("test", "no", 1, 25)
  expect_match(result$endpoint, "tables$")
})

test_that(".build_request_spec with English language", {
  filters <- list(Region = "01")
  result <- StatistikkbankR:::.build_request_spec("12452", "en", filters)
  expect_equal(result$params$lang, "en")
})

test_that(".build_request_spec handles many dimension filters", {
  filters <- list(
    Region = c("01", "02", "03"),
    Kjonn = c("1", "2"),
    Alder = c("10", "20"),
    Tid = c("2020", "2021")
  )
  result <- StatistikkbankR:::.build_request_spec("12452", "no", filters)
  expect_equal(result$params$`valueCodes[Region]`, "01,02,03")
  expect_equal(result$params$`valueCodes[Kjonn]`, "1,2")
  expect_equal(result$params$`valueCodes[Alder]`, "10,20")
  expect_equal(result$params$`valueCodes[Tid]`, "2020,2021")
})

test_that(".build_post_request_spec with many dimensions", {
  filters <- list(
    Region = c("01", "02"),
    Kjonn = c("1", "2"),
    Tid = c("2020", "2021")
  )
  result <- StatistikkbankR:::.build_post_request_spec("12452", "no", filters)
  expect_equal(length(result$body$selection), 3)
})

test_that(".build_config_request_spec has empty params", {
  result <- StatistikkbankR:::.build_config_request_spec()
  expect_equal(length(result$params), 0)
})

test_that(".build_tables_request_spec with large page size", {
  result <- StatistikkbankR:::.build_tables_request_spec("test", "no", 10, 100)
  expect_equal(result$params$pageNumber, 10L)
  expect_equal(result$params$pageSize, 100L)
})

test_that(".build_post_request_spec list conversion", {
  filters <- list(Region = "01")
  result <- StatistikkbankR:::.build_post_request_spec("12452", "no", filters)
  expect_true(is.list(result$body$selection[[1]]$valueCodes))
})
