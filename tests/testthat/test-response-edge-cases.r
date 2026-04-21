test_that(".apply_output_types handles all NA column", {
  grid <- data.frame(value = c(1, 2), notes = c(NA_character_, NA_character_))
  result <- StatistikkbankR:::.apply_output_types(grid, TRUE, "year_quarter")
  expect_true(is.factor(result$notes))
})

test_that(".apply_output_types with mixed types", {
  skip_if_not_installed("zoo")
  grid <- data.frame(
    Region_label = c("Oslo", "Bergen"),
    Tid_label = c("2023K1", "2023K2"),
    value = c(100, 200),
    id = c(1L, 2L)
  )
  result <- StatistikkbankR:::.apply_output_types(grid, TRUE, "year_quarter")
  expect_true(is.factor(result$Region_label))
  expect_true("yearqtr" %in% class(result$Tid_label))
  expect_true(is.numeric(result$value))
  expect_true(is.integer(result$id))
})

test_that(".extract_observation_status handles single status value", {
  parsed_json <- list(status = list(`0` = "A", `1` = "B"))
  result <- StatistikkbankR:::.extract_observation_status(parsed_json, 2)
  expect_equal(result, c("A", "B"))
})

test_that(".extract_observation_status with sparse status entries", {
  parsed_json <- list(status = list(`0` = "A", `2` = "C"))
  result <- StatistikkbankR:::.extract_observation_status(parsed_json, 3)
  expect_equal(result, c("A", NA_character_, "C"))
})

test_that(".extract_observation_status with out-of-range indices", {
  parsed_json <- list(status = list(`10` = "X", `20` = "Y"))
  result <- StatistikkbankR:::.extract_observation_status(parsed_json, 5)
  expect_true(all(is.na(result)))
})

test_that(".extract_observation_status with negative indices", {
  parsed_json <- list(status = list(`-1` = "X", `0` = "A"))
  result <- StatistikkbankR:::.extract_observation_status(parsed_json, 2)
  expect_equal(result, c("A", NA_character_))
})

test_that(".format_as_year_quarter with mixed Norwegian and English format", {
  quarters <- c("2023K1", "2023Q2", "2024K3", NA_character_)
  result <- StatistikkbankR:::.format_as_year_quarter(quarters)
  expect_equal(result, c("2023-Q1", "2023-Q2", "2024-Q3", NA_character_))
})

test_that(".is_quarter_column returns FALSE for partial quarters", {
  expect_false(StatistikkbankR:::.is_quarter_column(c("2023K1", "2023", "2023K2")))
})

test_that(".is_quarter_column handles single quarter", {
  expect_true(StatistikkbankR:::.is_quarter_column("2023K1"))
})

test_that(".parse_codelist_response handles complex mapping", {
  parsed <- list(
    id = "codes",
    label = "Code List",
    type = "codelist",
    language = "no",
    elimination = FALSE,
    values = list(
      list(code = "01", label = "Region 1", valueMap = c("A", "B", "C")),
      list(code = "02", label = "Region 2", valueMap = character(0)),
      list(code = "03", label = "Region 3", valueMap = NULL)
    )
  )
  result <- StatistikkbankR:::.parse_codelist_response(parsed)
  expect_equal(result$values$mapped_count, c(3L, 0L, 0L))
  expect_equal(result$values$mapped_values, c("A,B,C", "", ""))
})

test_that(".coerce_time_dimensions converts numeric time labels in wide format", {
  grid <- data.frame(
    Region_code = c("01", "01"),
    Tid_label = c("2023", "2024"),
    value = c(100, 200)
  )
  metadata <- list(roles = list(time = c("Tid")))
  result <- StatistikkbankR:::.coerce_time_dimensions(grid, c("Region", "Tid"), "wide", metadata)
  expect_true(is.numeric(result$Tid_label))
  expect_equal(result$Tid_label, c(2023, 2024))
})

test_that(".coerce_time_dimensions handles non-numeric time values", {
  grid <- data.frame(
    Tid_label = c("Q1", "Q2", "Q3"),
    value = c(1, 2, 3)
  )
  metadata <- list(roles = list(time = c("Tid")))
  result <- StatistikkbankR:::.coerce_time_dimensions(grid, c("Tid"), "wide", metadata)
  expect_true(is.character(result$Tid_label))
})

test_that(".parse_tables_response handles multiple table entries", {
  parsed <- list(
    tables = list(
      list(
        id = "12452",
        label = "Employment",
        description = "Employment statistics",
        updated = "2024-01-01",
        firstPeriod = "2020",
        lastPeriod = "2024",
        category = "Labour",
        source = "SSB",
        variableNames = list("Region", "Tid", "statistikkvariabel")
      ),
      list(
        id = "12453",
        label = "Unemployment",
        description = NULL,
        updated = NULL,
        firstPeriod = NULL,
        lastPeriod = NULL,
        category = NULL,
        source = NULL,
        variableNames = list("Region", "Tid")
      )
    ),
    page = list(pageNumber = 1, pageSize = 2, totalElements = 10, totalPages = 5)
  )
  result <- StatistikkbankR:::.parse_tables_response(parsed)
  expect_equal(nrow(result$data), 2)
  expect_equal(result$data$id[1], "12452")
  expect_true(is.na(result$data$description[2]))
  expect_equal(result$data$variables[1], "Region, Tid")
})

test_that(".dimension_code_col_name preserves code suffix variants", {
  expect_equal(StatistikkbankR:::.dimension_code_col_name("RegionCode"), "RegionCode")
  expect_equal(StatistikkbankR:::.dimension_code_col_name("Region_Code"), "Region_Code")
  expect_equal(StatistikkbankR:::.dimension_code_col_name("Tid"), "Tid_code")
})

test_that(".apply_output_types doesn't affect non-character columns", {
  grid <- data.frame(
    int_col = c(1L, 2L),
    num_col = c(1.5, 2.5),
    log_col = c(TRUE, FALSE)
  )
  result <- StatistikkbankR:::.apply_output_types(grid, TRUE, "year_quarter")
  expect_true(is.integer(result$int_col))
  expect_true(is.numeric(result$num_col))
  expect_true(is.logical(result$log_col))
})

test_that(".is_quarter_column handles different case variations", {
  expect_true(StatistikkbankR:::.is_quarter_column(c("2023K1", "2024K2", "2025K3")))
  expect_true(StatistikkbankR:::.is_quarter_column(c("2023Q1", "2024Q2", "2025Q4")))
})

test_that(".format_as_year_quarter with single-digit quarter", {
  result <- StatistikkbankR:::.format_as_year_quarter("2023K1")
  expect_equal(result, "2023-Q1")
})

test_that(".extract_observation_status handles string indices", {
  parsed_json <- list(status = list(a = "X", b = "Y"))
  result <- StatistikkbankR:::.extract_observation_status(parsed_json, 2)
  expect_true(all(is.na(result)))
})

test_that(".parse_codelist_response with single value", {
  parsed <- list(
    id = "single",
    label = "Single Code",
    type = "codelist",
    language = "no",
    elimination = FALSE,
    values = list(
      list(code = "only", label = "Only Code", valueMap = NULL)
    )
  )
  result <- StatistikkbankR:::.parse_codelist_response(parsed)
  expect_equal(nrow(result$values), 1)
  expect_equal(result$values$code[1], "only")
})
