test_that(".validate_codelist_names_against_metadata accepts empty codelists", {
  metadata <- list(dimensions = list(Tid = list(), Region = list()))
  expect_silent(.validate_codelist_names_against_metadata(list(), metadata))
})

test_that(".validate_codelist_names_against_metadata rejects unknown dimension", {
  metadata <- list(dimensions = list(Tid = list(), Region = list()))
  expect_error(
    .validate_codelist_names_against_metadata(list(UnknownDim = "codelist_1"), metadata),
    "Unknown codelist dimension"
  )
})

test_that(".validate_codelist_names_against_metadata accepts valid dimensions", {
  metadata <- list(dimensions = list(Tid = list(), Region = list()))
  expect_silent(.validate_codelist_names_against_metadata(list(Region = "codelist_1"), metadata))
})

test_that(".validate_codelist_names_against_metadata error mentions valid dimensions", {
  metadata <- list(dimensions = list(Tid = list(), Region = list()))
  expect_error(
    .validate_codelist_names_against_metadata(list(BadDim = "codelist_1"), metadata),
    "Tid|Region"
  )
})

test_that(".validate_output_value_names_against_metadata accepts empty output_values", {
  metadata <- list(dimensions = list(Tid = list(), Region = list()))
  expect_silent(.validate_output_value_names_against_metadata(list(), metadata))
})

test_that(".validate_output_value_names_against_metadata rejects unknown dimension", {
  metadata <- list(dimensions = list(Tid = list(), Region = list()))
  expect_error(
    .validate_output_value_names_against_metadata(list(UnknownDim = "aggregated"), metadata),
    "Unknown output_values dimension"
  )
})

test_that(".validate_output_value_names_against_metadata accepts valid dimensions", {
  metadata <- list(dimensions = list(Tid = list(), Region = list()))
  expect_silent(.validate_output_value_names_against_metadata(list(Region = "aggregated"), metadata))
})

test_that(".validate_output_values_against_codelists accepts empty output_values", {
  expect_silent(.validate_output_values_against_codelists(list(), list()))
})

test_that(".validate_output_values_against_codelists requires matching codelist", {
  output_values <- list(Region = "aggregated")
  codelists <- list()
  expect_error(
    .validate_output_values_against_codelists(output_values, codelists),
    "requires matching entries in `codelists`"
  )
})

test_that(".validate_output_values_against_codelists accepts matching codelist", {
  output_values <- list(Region = "aggregated")
  codelists <- list(Region = "CODELIST123")
  expect_silent(.validate_output_values_against_codelists(output_values, codelists))
})

test_that(".validate_output_values_against_codelists error mentions dimension", {
  output_values <- list(Region = "aggregated", Tid = "single")
  codelists <- list(Tid = "CODELIST123")
  expect_error(
    .validate_output_values_against_codelists(output_values, codelists),
    "Region"
  )
})

test_that(".validate_filters rejects non-atomic filter values", {
  expect_error(.validate_filters(list(Region = list("01", "02"))), "atomic")
})

test_that(".validate_filters accepts integer filter values", {
  expect_silent(.validate_filters(list(Kjonn = c(1L, 2L))))
})

test_that(".validate_filters accepts multiple character values", {
  expect_silent(.validate_filters(list(Region = c("01", "02", "03"))))
})

test_that(".validate_filters rejects empty length filter", {
  expect_error(.validate_filters(list(Region = character(0))), "at least one value")
})

test_that(".validate_codelists rejects NA single value", {
  expect_error(.validate_codelists(list(Region = NA_character_)), "single non-missing")
})

test_that(".validate_codelists rejects whitespace-only value", {
  expect_error(.validate_codelists(list(Region = "   ")), "empty")
})

test_that(".validate_output_values rejects unnamed entries", {
  expect_error(.validate_output_values(list("aggregated")), "named")
})

test_that(".validate_output_values rejects duplicate names", {
  expect_error(.validate_output_values(list(Region = "aggregated", Region = "single")), "unique")
})

test_that(".validate_output_values rejects NA values", {
  expect_error(.validate_output_values(list(Region = NA_character_)), "single non-missing")
})

test_that(".normalise_filters handles empty list", {
  result <- .normalise_filters(list())
  expect_equal(result, list())
})

test_that(".normalise_filters handles multiple dimensions", {
  filters <- list(Region = c("01", "02"), Tid = c("2020", "2021"))
  result <- .normalise_filters(filters)
  expect_equal(result$Region, c("01", "02"))
  expect_equal(result$Tid, c("2020", "2021"))
})
