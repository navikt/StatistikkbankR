make_raw_metadata_with_codelists <- function(
  table_id = "12452",
  dim_id = "Region",
  n = 2
) {
  codes <- paste0(dim_id, "_code_", seq_len(n))
  labels_list <- setNames(as.list(paste0(dim_id, "_label_", seq_len(n))), codes)
  index_list <- setNames(as.list(seq(0, n - 1)), codes)
  
  list(
    id = list(dim_id),
    size = list(n),
    dimension = setNames(list(list(
      label = paste0(dim_id, "_label"),
      category = list(index = index_list, label = labels_list),
      extension = list(codelists = list(
        list(
          id = "CODELIST123",
          label = "Aggregation by region",
          type = "codelist",
          links = list(
            list(href = "https://data.ssb.no/api/pxwebapi/v2/codelists/CODELIST123")
          )
        )
      ))
    )), dim_id),
    label = "Test table",
    updated = "2024-01-01T00:00:00Z",
    role = list(time = list()),
    extension = list(px = list(tableid = table_id))
  )
}

make_raw_metadata_with_units <- function(
  table_id = "12452",
  dim_id = "Verdi",
  n = 2
) {
  codes <- paste0(dim_id, "_code_", seq_len(n))
  labels_list <- setNames(as.list(paste0(dim_id, "_label_", seq_len(n))), codes)
  index_list <- setNames(as.list(seq(0, n - 1)), codes)
  
  units_list <- setNames(
    lapply(codes, function(code) list(base = "Antall", decimals = 0L)),
    codes
  )
  
  list(
    id = list(dim_id),
    size = list(n),
    dimension = setNames(list(list(
      label = paste0(dim_id, "_label"),
      category = list(
        index = index_list,
        label = labels_list,
        unit = units_list
      )
    )), dim_id),
    label = "Test table",
    updated = "2024-01-01T00:00:00Z",
    role = list(time = list()),
    extension = list(px = list(tableid = table_id))
  )
}

test_that(".parse_table_metadata extracts codelists", {
  raw <- make_raw_metadata_with_codelists()
  result <- StatistikkbankR:::.parse_table_metadata(raw)
  
  expect_true(length(result$dimensions$Region$codelists) > 0)
  expect_equal(result$dimensions$Region$codelists[[1]]$id, "CODELIST123")
  expect_equal(result$dimensions$Region$codelists[[1]]$label, "Aggregation by region")
  expect_match(result$dimensions$Region$codelists[[1]]$href, "CODELIST123")
})

test_that(".parse_table_metadata handles codelist with no links", {
  raw <- make_raw_metadata_with_codelists()
  raw$dimension$Region$extension$codelists[[1]]$links <- NULL
  result <- StatistikkbankR:::.parse_table_metadata(raw)
  
  expect_true(is.na(result$dimensions$Region$codelists[[1]]$href))
})

test_that(".parse_table_metadata extracts unit information", {
  raw <- make_raw_metadata_with_units()
  result <- StatistikkbankR:::.parse_table_metadata(raw)
  
  dim <- result$dimensions$Verdi
  expect_equal(nrow(dim$units), 2)
  expect_equal(dim$units$unit_base[1], "Antall")
  expect_equal(dim$units$unit_decimals[1], 0L)
})

test_that(".parse_table_metadata handles missing unit fields", {
  raw <- make_raw_metadata_with_units()
  raw$dimension$Verdi$category$unit$Verdi_code_1 <- list(base = NULL, decimals = NULL)
  result <- StatistikkbankR:::.parse_table_metadata(raw)
  
  expect_true(is.na(result$dimensions$Verdi$units$unit_base[1]))
  expect_true(is.na(result$dimensions$Verdi$units$unit_decimals[1]))
})

test_that(".parse_table_metadata has empty units when no unit data", {
  raw <- make_raw_metadata_with_codelists()
  result <- StatistikkbankR:::.parse_table_metadata(raw)
  
  expect_equal(nrow(result$dimensions$Region$units), 0)
  expect_named(result$dimensions$Region$units, c("code", "unit_base", "unit_decimals"))
})

test_that(".parse_table_metadata has empty codelists when none present", {
  raw <- list(
    id = list("Region"),
    size = list(2L),
    dimension = list(Region = list(
      label = "Region",
      category = list(
        index = list(R1 = 0, R2 = 1),
        label = list(R1 = "Region 1", R2 = "Region 2")
      )
    )),
    label = "Test",
    updated = "2024-01-01",
    role = list(time = list()),
    extension = list(px = list(tableid = "12452"))
  )
  result <- StatistikkbankR:::.parse_table_metadata(raw)
  expect_equal(length(result$dimensions$Region$codelists), 0)
})

test_that(".parse_api_config handles fractional max_data_cells", {
  raw_config <- list(maxDataCells = 75000.5)
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_equal(result$max_data_cells, 75000L)
})

test_that(".parse_api_config extracts both language and data format", {
  raw_config <- list(
    maxDataCells = 50000,
    defaultLanguage = "en",
    defaultDataFormat = "csv"
  )
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_equal(result$default_language, "en")
  expect_equal(result$default_data_format, "csv")
})

test_that(".is_api_expression handles empty string", {
  expect_false(StatistikkbankR:::.is_api_expression(""))
})

test_that(".is_api_expression is case insensitive for top/bottom", {
  expect_true(StatistikkbankR:::.is_api_expression("TOP(5)"))
  expect_true(StatistikkbankR:::.is_api_expression("BOTTOM(10)"))
  expect_true(StatistikkbankR:::.is_api_expression("Top(3)"))
})

test_that(".parse_api_config with null api and app versions", {
  raw_config <- list(
    maxDataCells = 50000,
    apiVersion = NULL,
    appVersion = NULL
  )
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_true(is.na(result$api_version))
  expect_true(is.na(result$app_version))
})

test_that(".parse_api_config handles valid time window", {
  raw_config <- list(
    maxDataCells = 50000,
    maxCallsPerTimeWindow = 10,
    timeWindow = 60
  )
  result <- StatistikkbankR:::.parse_api_config(raw_config)
  expect_equal(result$max_calls_per_time_window, 10L)
  expect_equal(result$time_window, 60L)
})
