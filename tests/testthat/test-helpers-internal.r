make_helpers_metadata_stub <- function() {
  list(
    table_id = "07459",
    table_label = "Test table",
    updated = "2024-01-01T00:00:00Z",
    dimensions = list(
      Region = list(
        label = "Region label",
        codes = c("01", "02"),
        labels = c("Oslo", "Bergen"),
        code_count = 2L,
        units = data.frame(
          code = character(0),
          unit_base = character(0),
          unit_decimals = integer(0),
          stringsAsFactors = FALSE
        ),
        codelists = list()
      ),
      Verdi = list(
        label = "Values label",
        codes = c("A", "B"),
        labels = c("Value A", "Value B"),
        code_count = 2L,
        units = data.frame(
          code = c("A", "B"),
          unit_base = c("NOK", "NOK"),
          unit_decimals = c(0L, 0L),
          stringsAsFactors = FALSE
        ),
        codelists = list()
      )
    )
  )
}

test_that(".flatten_dimension_units returns empty df when no units", {
  metadata <- make_helpers_metadata_stub()
  result <- StatistikkbankR:::.flatten_dimension_units(metadata)
  
  region_rows <- result[result$dimension == "Region", ]
  expect_equal(nrow(region_rows), 0)
})

test_that(".flatten_dimension_units returns unit rows when units present", {
  metadata <- make_helpers_metadata_stub()
  result <- StatistikkbankR:::.flatten_dimension_units(metadata)
  
  verdi_rows <- result[result$dimension == "Verdi", ]
  expect_equal(nrow(verdi_rows), 2)
  expect_equal(verdi_rows$unit_base, c("NOK", "NOK"))
})

test_that(".flatten_dimension_units filters to specified dimension", {
  metadata <- make_helpers_metadata_stub()
  result <- StatistikkbankR:::.flatten_dimension_units(metadata, "Verdi")
  
  expect_equal(nrow(result), 2)
  expect_equal(unique(result$dimension), "Verdi")
})

test_that(".flatten_dimension_units rejects unknown dimension", {
  metadata <- make_helpers_metadata_stub()
  expect_error(
    StatistikkbankR:::.flatten_dimension_units(metadata, "UnknownDim"),
    "Unknown dimension"
  )
})

test_that(".flatten_dimension_units includes labels in output", {
  metadata <- make_helpers_metadata_stub()
  result <- StatistikkbankR:::.flatten_dimension_units(metadata, "Verdi")
  
  expect_equal(result$label, c("Value A", "Value B"))
})

test_that(".flatten_dimension_codelists returns empty df when no codelists", {
  metadata <- make_helpers_metadata_stub()
  result <- StatistikkbankR:::.flatten_dimension_codelists(metadata)
  
  expect_equal(nrow(result), 0)
  expect_named(result, c("table_id", "dimension", "dimension_label", "codelist_id", 
                          "codelist_label", "codelist_type", "supports_output_values",
                          "suggested_output_values", "codelist_href"))
})

test_that(".flatten_dimension_codelists returns codelist rows", {
  metadata <- make_helpers_metadata_stub()
  metadata$dimensions$Region$codelists <- list(
    list(
      id = "CODELIST_A",
      label = "Aggregation A",
      type = "Aggregation",
      href = "https://example.test/A"
    )
  )
  
  result <- StatistikkbankR:::.flatten_dimension_codelists(metadata)
  expect_equal(nrow(result), 1)
  expect_equal(result$codelist_id, "CODELIST_A")
  expect_equal(result$supports_output_values, TRUE)
  expect_equal(result$suggested_output_values, "aggregated,single")
})

test_that(".flatten_dimension_codelists handles non-aggregation type", {
  metadata <- make_helpers_metadata_stub()
  metadata$dimensions$Region$codelists <- list(
    list(
      id = "CODELIST_B",
      label = "Classification B",
      type = "Classification",
      href = "https://example.test/B"
    )
  )
  
  result <- StatistikkbankR:::.flatten_dimension_codelists(metadata)
  expect_equal(result$supports_output_values, FALSE)
  expect_true(is.na(result$suggested_output_values))
})

test_that(".flatten_dimension_codelists filters to specified dimension", {
  metadata <- make_helpers_metadata_stub()
  metadata$dimensions$Region$codelists <- list(
    list(id = "CODELIST_A", label = "A", type = "Aggregation", href = "http://example.com/A")
  )
  metadata$dimensions$Verdi$codelists <- list(
    list(id = "CODELIST_B", label = "B", type = "Classification", href = "http://example.com/B")
  )
  
  result <- StatistikkbankR:::.flatten_dimension_codelists(metadata, "Region")
  expect_equal(nrow(result), 1)
  expect_equal(unique(result$dimension), "Region")
})

test_that(".flatten_dimension_codelists rejects unknown dimension", {
  metadata <- make_helpers_metadata_stub()
  expect_error(
    StatistikkbankR:::.flatten_dimension_codelists(metadata, "UnknownDim"),
    "Unknown dimension"
  )
})

test_that(".flatten_dimension_codelists handles multiple codelists in one dimension", {
  metadata <- make_helpers_metadata_stub()
  metadata$dimensions$Region$codelists <- list(
    list(id = "CODELIST_A", label = "A", type = "Aggregation", href = "http://example.com/A"),
    list(id = "CODELIST_B", label = "B", type = "Classification", href = "http://example.com/B")
  )
  
  result <- StatistikkbankR:::.flatten_dimension_codelists(metadata, "Region")
  expect_equal(nrow(result), 2)
})

test_that(".flatten_dimension_units includes all required columns", {
  metadata <- make_helpers_metadata_stub()
  result <- StatistikkbankR:::.flatten_dimension_units(metadata, "Verdi")
  
  expect_named(result, c("table_id", "dimension", "dimension_label", "code", "label", "unit_base", "unit_decimals"))
})

test_that(".flatten_dimension_units includes table_id", {
  metadata <- make_helpers_metadata_stub()
  result <- StatistikkbankR:::.flatten_dimension_units(metadata, "Verdi")
  
  expect_equal(unique(result$table_id), "07459")
})

test_that(".flatten_dimension_codelists includes dimension_label", {
  metadata <- make_helpers_metadata_stub()
  metadata$dimensions$Region$codelists <- list(
    list(id = "CODELIST_A", label = "A", type = "Aggregation", href = "http://example.com")
  )
  
  result <- StatistikkbankR:::.flatten_dimension_codelists(metadata, "Region")
  expect_equal(result$dimension_label, "Region label")
})
