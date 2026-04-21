test_that(".dimension_code_col_name adds _code suffix", {
  result <- StatistikkbankR:::.dimension_code_col_name("Region")
  expect_equal(result, "Region_code")
})

test_that(".dimension_code_col_name preserves existing Code suffix", {
  result <- StatistikkbankR:::.dimension_code_col_name("RegionCode")
  expect_equal(result, "RegionCode")
})

test_that(".dimension_label_col_name adds _label suffix", {
  result <- StatistikkbankR:::.dimension_label_col_name("Region")
  expect_equal(result, "Region_label")
})

test_that(".is_quarter_string detects Norwegian quarter format", {
  expect_true(StatistikkbankR:::.is_quarter_string("2023K1"))
  expect_true(StatistikkbankR:::.is_quarter_string("2023K4"))
})

test_that(".is_quarter_string detects English quarter format", {
  expect_true(StatistikkbankR:::.is_quarter_string("2023Q1"))
  expect_true(StatistikkbankR:::.is_quarter_string("2023Q4"))
})

test_that(".is_quarter_string rejects invalid formats", {
  expect_false(StatistikkbankR:::.is_quarter_string("2023"))
  expect_false(StatistikkbankR:::.is_quarter_string("2023K5"))
  expect_false(StatistikkbankR:::.is_quarter_string("2023K0"))
  expect_false(StatistikkbankR:::.is_quarter_string("K12023"))
  expect_false(StatistikkbankR:::.is_quarter_string(""))
})

test_that(".is_quarter_column returns FALSE for non-character", {
  expect_false(StatistikkbankR:::.is_quarter_column(c(1, 2, 3)))
  expect_false(StatistikkbankR:::.is_quarter_column(logical(0)))
})

test_that(".is_quarter_column returns FALSE for empty vector", {
  expect_false(StatistikkbankR:::.is_quarter_column(character(0)))
})

test_that(".is_quarter_column returns FALSE for all-NA", {
  expect_false(StatistikkbankR:::.is_quarter_column(c(NA_character_, NA_character_)))
})

test_that(".is_quarter_column returns FALSE for mixed format", {
  expect_false(StatistikkbankR:::.is_quarter_column(c("2023K1", "2023", "2023K2")))
})

test_that(".is_quarter_column returns TRUE for Norwegian quarters", {
  expect_true(StatistikkbankR:::.is_quarter_column(c("2023K1", "2023K2", "2023K3")))
})

test_that(".is_quarter_column returns TRUE for English quarters", {
  expect_true(StatistikkbankR:::.is_quarter_column(c("2023Q1", "2023Q2", "2023Q3")))
})

test_that(".is_quarter_column handles NA with valid quarters", {
  expect_true(StatistikkbankR:::.is_quarter_column(c("2023K1", NA_character_, "2023K2")))
})

test_that(".format_as_year_quarter converts Norwegian format", {
  result <- StatistikkbankR:::.format_as_year_quarter("2023K1")
  expect_equal(result, "2023-Q1")
  
  result <- StatistikkbankR:::.format_as_year_quarter("2023K4")
  expect_equal(result, "2023-Q4")
})

test_that(".format_as_year_quarter converts English format", {
  result <- StatistikkbankR:::.format_as_year_quarter("2023Q1")
  expect_equal(result, "2023-Q1")
})

test_that(".format_as_year_quarter handles NA", {
  result <- StatistikkbankR:::.format_as_year_quarter(NA_character_)
  expect_true(is.na(result))
})

test_that(".format_as_year_quarter vectorizes", {
  result <- StatistikkbankR:::.format_as_year_quarter(c("2023K1", "2023K2", NA_character_))
  expect_equal(result, c("2023-Q1", "2023-Q2", NA_character_))
})

test_that(".extract_observation_status returns NA for missing status", {
  parsed_json <- list(status = NULL)
  result <- StatistikkbankR:::.extract_observation_status(parsed_json, 3)
  expect_true(all(is.na(result)))
  expect_length(result, 3)
})

test_that(".extract_observation_status returns NA for empty status", {
  parsed_json <- list(status = list())
  result <- StatistikkbankR:::.extract_observation_status(parsed_json, 3)
  expect_true(all(is.na(result)))
  expect_length(result, 3)
})

test_that(".extract_observation_status returns all NA vector of correct length", {
  parsed_json <- list(status = list())
  result <- StatistikkbankR:::.extract_observation_status(parsed_json, 5)
  expect_length(result, 5)
})

test_that(".parse_codelist_response handles empty values", {
  parsed <- list(
    id = "codes",
    label = "Code List",
    type = "codelist",
    language = "no",
    elimination = FALSE,
    values = NULL
  )
  result <- StatistikkbankR:::.parse_codelist_response(parsed)
  expect_named(result, c("codelist", "values"))
  expect_equal(nrow(result$values), 0)
  expect_named(result$values, c("code", "label", "mapped_values", "mapped_count"))
})

test_that(".parse_codelist_response extracts codelist metadata", {
  parsed <- list(
    id = "codes",
    label = "Code List",
    type = "codelist",
    language = "no",
    elimination = TRUE,
    eliminationValueCode = "NA"
  )
  result <- StatistikkbankR:::.parse_codelist_response(parsed)
  expect_equal(result$codelist$id, "codes")
  expect_equal(result$codelist$label, "Code List")
  expect_equal(result$codelist$elimination, TRUE)
  expect_equal(result$codelist$elimination_value_code, "NA")
})

test_that(".parse_codelist_response handles NULL elimination value code", {
  parsed <- list(
    id = "codes",
    label = "Code List",
    type = "codelist",
    language = "no",
    elimination = FALSE,
    eliminationValueCode = NULL
  )
  result <- StatistikkbankR:::.parse_codelist_response(parsed)
  expect_true(is.na(result$codelist$elimination_value_code))
})

test_that(".parse_codelist_response builds values dataframe", {
  parsed <- list(
    id = "codes",
    label = "Code List",
    type = "codelist",
    language = "no",
    elimination = FALSE,
    values = list(
      list(code = "01", label = "Region 1", valueMap = NULL),
      list(code = "02", label = "Region 2", valueMap = c("A", "B"))
    )
  )
  result <- StatistikkbankR:::.parse_codelist_response(parsed)
  expect_equal(nrow(result$values), 2)
  expect_equal(result$values$code, c("01", "02"))
  expect_equal(result$values$label, c("Region 1", "Region 2"))
  expect_equal(result$values$mapped_count, c(0L, 2L))
})

test_that(".parse_tables_response handles empty tables", {
  parsed <- list(
    tables = NULL,
    page = list(pageNumber = 1, pageSize = 25, totalElements = 0, totalPages = 0)
  )
  result <- StatistikkbankR:::.parse_tables_response(parsed)
  expect_equal(nrow(result$data), 0)
  expect_equal(result$page_info$total_elements, 0)
})

test_that(".parse_tables_response extracts page info", {
  parsed <- list(
    tables = list(),
    page = list(pageNumber = 2, pageSize = 50, totalElements = 100, totalPages = 2)
  )
  result <- StatistikkbankR:::.parse_tables_response(parsed)
  expect_equal(result$page_info$page_number, 2)
  expect_equal(result$page_info$page_size, 50)
  expect_equal(result$page_info$total_elements, 100)
})

test_that(".coerce_time_dimensions returns grid unchanged when no time dims", {
  grid <- data.frame(Region_code = c("01", "02"), value = c(1, 2))
  metadata <- list(roles = list(time = NULL))
  result <- StatistikkbankR:::.coerce_time_dimensions(grid, "Region", "wide", metadata)
  expect_equal(result, grid)
})

test_that(".coerce_time_dimensions returns grid unchanged when metadata NULL", {
  grid <- data.frame(Region_code = c("01", "02"), value = c(1, 2))
  result <- StatistikkbankR:::.coerce_time_dimensions(grid, "Region", "wide", NULL)
  expect_equal(result, grid)
})

test_that(".coerce_time_dimensions returns grid for long format", {
  grid <- data.frame(dimension = "Tid", code = "2023", value = c(1, 2))
  metadata <- list(roles = list(time = c("Tid")))
  result <- StatistikkbankR:::.coerce_time_dimensions(grid, c("Region", "Tid"), "long", metadata)
  expect_equal(result, grid)
})

test_that(".apply_output_types converts quarters to year-quarter format", {
  skip_if_not_installed("zoo")
  grid <- data.frame(Tid_label = c("2023K1", "2023K2"), value = c(1, 2))
  result <- StatistikkbankR:::.apply_output_types(grid, FALSE, TRUE, FALSE)
  expect_true("yearqtr" %in% class(result$Tid_label))
})

test_that(".apply_output_types preserves quarters when conversion disabled", {
  grid <- data.frame(Tid_label = c("2023K1", "2023K2"), value = c(1, 2))
  result <- StatistikkbankR:::.apply_output_types(grid, FALSE, FALSE, FALSE)
  expect_equal(result$Tid_label, c("2023K1", "2023K2"))
})

test_that(".apply_output_types converts characters to factors", {
  grid <- data.frame(Region_label = c("Oslo", "Bergen"), value = c(1, 2))
  result <- StatistikkbankR:::.apply_output_types(grid, TRUE, FALSE, FALSE)
  expect_true(is.factor(result$Region_label))
})

test_that(".apply_output_types handles empty dataframe", {
  grid <- data.frame()
  result <- StatistikkbankR:::.apply_output_types(grid, TRUE, TRUE, TRUE)
  expect_equal(nrow(result), 0)
})

test_that(".apply_output_types preserves numeric columns", {
  grid <- data.frame(Region_label = "Oslo", value = c(1.5, 2.5))
  result <- StatistikkbankR:::.apply_output_types(grid, TRUE, FALSE, FALSE)
  expect_true(is.numeric(result$value))
})
