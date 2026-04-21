test_that(".default_api_config returns complete config", {
  result <- StatistikkbankR:::.default_api_config()
  expect_named(result, c("api_version", "app_version", "default_language", "default_data_format", 
                          "max_data_cells", "max_calls_per_time_window", "time_window", 
                          "data_formats", "source"))
  expect_equal(result$default_language, "no")
  expect_equal(result$source, "fallback")
})

test_that(".build_metadata_cache_key builds correct key", {
  result <- StatistikkbankR:::.build_metadata_cache_key("12452", "no")
  expect_equal(result, "12452::no")
})

test_that(".build_metadata_cache_key includes language", {
  key_no <- StatistikkbankR:::.build_metadata_cache_key("12452", "no")
  key_en <- StatistikkbankR:::.build_metadata_cache_key("12452", "en")
  expect_true(key_no != key_en)
})

test_that(".is_api_expression detects asterisk", {
  expect_true(StatistikkbankR:::.is_api_expression("*"))
  expect_false(StatistikkbankR:::.is_api_expression("Region_1"))
})

test_that(".is_api_expression detects question mark asterisk", {
  expect_true(StatistikkbankR:::.is_api_expression("?*"))
  expect_true(StatistikkbankR:::.is_api_expression("?*"))
})

test_that(".is_api_expression detects top function", {
  expect_true(StatistikkbankR:::.is_api_expression("top(5)"))
  expect_true(StatistikkbankR:::.is_api_expression("TOP(5)"))
})

test_that(".is_api_expression detects bottom function", {
  expect_true(StatistikkbankR:::.is_api_expression("bottom(5)"))
})

test_that(".is_api_expression detects from function", {
  expect_true(StatistikkbankR:::.is_api_expression("from(2020)"))
})

test_that(".is_api_expression detects to function", {
  expect_true(StatistikkbankR:::.is_api_expression("to(2023)"))
})

test_that(".is_api_expression detects range function", {
  expect_true(StatistikkbankR:::.is_api_expression("range(2020,2023)"))
})

test_that(".is_api_expression rejects regular codes", {
  expect_false(StatistikkbankR:::.is_api_expression("2020"))
  expect_false(StatistikkbankR:::.is_api_expression("Region_1"))
  expect_false(StatistikkbankR:::.is_api_expression("Oslo"))
})

test_that(".is_api_expression handles multiple values vector", {
  values <- c("*", "Region_1", "Region_2")
  result <- StatistikkbankR:::.is_api_expression(values)
  expect_equal(result, c(TRUE, FALSE, FALSE))
})

test_that(".parse_api_config handles missing fields", {
  raw_config <- list(
    apiVersion = "1.0",
    appVersion = "1.0",
    maxDataCells = 50000,
    dataFormats = list("json-stat2", "csv")
  )
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_equal(result$api_version, "1.0")
  expect_equal(result$max_data_cells, 50000L)
  expect_equal(length(result$data_formats), 2)
})

test_that(".parse_api_config sets default language when missing", {
  raw_config <- list(maxDataCells = 50000)
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_equal(result$default_language, "no")
})

test_that(".parse_api_config handles invalid max_data_cells", {
  raw_config <- list(maxDataCells = "invalid")
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_equal(result$max_data_cells, 100000L)
})

test_that(".parse_api_config handles zero max_data_cells", {
  raw_config <- list(maxDataCells = 0)
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_equal(result$max_data_cells, 100000L)
})

test_that(".parse_api_config handles negative max_calls_per_time_window", {
  raw_config <- list(maxDataCells = 50000, maxCallsPerTimeWindow = -5)
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_true(is.na(result$max_calls_per_time_window))
})

test_that(".parse_api_config converts empty arrays properly", {
  raw_config <- list(
    maxDataCells = 50000,
    maxCallsPerTimeWindow = list(),
    timeWindow = list()
  )
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_true(is.na(result$max_calls_per_time_window))
  expect_true(is.na(result$time_window))
})

test_that(".parse_api_config sets source to api", {
  raw_config <- list(maxDataCells = 50000)
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_equal(result$source, "api")
})

test_that(".estimate_row_count multiplies dimension sizes", {
  metadata <- list(
    dimensions = list(
      Region = list(code_count = 5L),
      Tid = list(code_count = 10L)
    )
  )
  completed_filters <- list(
    Region = c("01", "02"),
    Tid = c("2020", "2021", "2022")
  )
  result <- StatistikkbankR:::.estimate_row_count(metadata, completed_filters)
  expect_equal(result, 6L)
})

test_that(".estimate_row_count uses code_count for wildcard", {
  metadata <- list(
    dimensions = list(
      Region = list(code_count = 5L),
      Tid = list(code_count = 10L)
    )
  )
  completed_filters <- list(
    Region = "*",
    Tid = c("2020", "2021")
  )
  result <- StatistikkbankR:::.estimate_row_count(metadata, completed_filters)
  expect_equal(result, 10L)
})

test_that(".estimate_row_count handles api functions", {
  metadata <- list(
    dimensions = list(
      Tid = list(code_count = 50L)
    )
  )
  completed_filters <- list(
    Tid = "top(5)"
  )
  result <- StatistikkbankR:::.estimate_row_count(metadata, completed_filters)
  expect_equal(result, 50L)
})

test_that(".estimate_row_count handles mixed api expressions and codes", {
  metadata <- list(
    dimensions = list(
      Region = list(code_count = 20L),
      Tid = list(code_count = 50L)
    )
  )
  completed_filters <- list(
    Region = c("01", "02", "03"),
    Tid = "from(2020)"
  )
  result <- StatistikkbankR:::.estimate_row_count(metadata, completed_filters)
  expect_equal(result, 150L)
})

test_that(".auto_complete_filters includes user filters", {
  metadata <- list(
    dimensions = list(
      Region = list(codes = c("01", "02", "03")),
      Tid = list(codes = c("2020", "2021"))
    )
  )
  normalised_filters <- list(Region = c("01"))
  result <- StatistikkbankR:::.auto_complete_filters(metadata, normalised_filters)
  expect_equal(result$Region, "01")
})

test_that(".auto_complete_filters fills in all dimensions when missing", {
  metadata <- list(
    dimensions = list(
      Region = list(codes = c("01", "02")),
      Tid = list(codes = c("2020", "2021"))
    )
  )
  normalised_filters <- list()
  result <- StatistikkbankR:::.auto_complete_filters(metadata, normalised_filters)
  expect_equal(result$Region, c("01", "02"))
  expect_equal(result$Tid, c("2020", "2021"))
})

test_that(".auto_complete_filters names result correctly", {
  metadata <- list(
    dimensions = list(
      Region = list(codes = c("01")),
      Tid = list(codes = c("2020"))
    )
  )
  result <- StatistikkbankR:::.auto_complete_filters(metadata, list())
  expect_named(result, c("Region", "Tid"))
})

test_that(".auto_complete_filters handles codelists parameter", {
  metadata <- list(
    dimensions = list(
      Region = list(codes = c("01", "02")),
      Codelist = list(codes = c("A"))
    )
  )
  normalised_filters <- list()
  normalised_codelists <- list(Codelist = "codelist_id")
  result <- StatistikkbankR:::.auto_complete_filters(metadata, normalised_filters, normalised_codelists)
  expect_equal(result$Codelist, "*")
  expect_equal(result$Region, c("01", "02"))
})
