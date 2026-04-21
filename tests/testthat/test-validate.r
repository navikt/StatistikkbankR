test_that(".validate_table_request_args rejects bad inputs", {
  expect_error(.validate_table_request_args("", "no"), "non-empty")
  expect_error(.validate_table_request_args(123, "no"), "non-empty")
  expect_error(.validate_table_request_args("07459", "fr"), "\"no\" or \"en\"")
  expect_silent(.validate_table_request_args("07459", "no"))
  expect_silent(.validate_table_request_args("07459", "en"))
})

test_that(".validate_metadata_cache_args rejects bad inputs", {
  expect_error(.validate_metadata_cache_args("yes", FALSE), "TRUE or FALSE")
  expect_error(.validate_metadata_cache_args(TRUE, "no"), "TRUE or FALSE")
  expect_silent(.validate_metadata_cache_args(TRUE, FALSE))
  expect_silent(.validate_metadata_cache_args(FALSE, TRUE))
})

test_that(".validate_filters accepts empty filters silently", {
  expect_silent(.validate_filters(list()))
})

test_that(".validate_filters rejects unnamed filters", {
  expect_error(.validate_filters(list("2020")), "named")
})

test_that(".validate_filters rejects duplicate filter names", {
  expect_error(.validate_filters(list(Tid = "2020", Tid = "2021")), "unique")
})

test_that(".validate_filters rejects NULL filter values", {
  expect_error(.validate_filters(list(Tid = NULL)), "NULL")
})

test_that(".validate_filters rejects NA filter values", {
  expect_error(.validate_filters(list(Tid = NA_character_)), "missing")
})

test_that(".validate_filters rejects empty string filter values", {
  expect_error(.validate_filters(list(Tid = "")), "empty strings")
})

test_that(".validate_filters rejects logical filter values", {
  expect_error(.validate_filters(list(Tid = TRUE)), "TRUE or FALSE")
})

test_that(".validate_filters accepts valid character and numeric filters", {
  expect_silent(.validate_filters(list(Tid = c("2020", "2021"), Kjonn = "1")))
  expect_silent(.validate_filters(list(Tid = 2020L)))
})

test_that(".validate_codelists accepts NULL/empty inputs silently", {
  expect_silent(.validate_codelists(NULL))
  expect_silent(.validate_codelists(list()))
})

test_that(".validate_codelists rejects unnamed entries", {
  expect_error(.validate_codelists(list("agg_foo")), "named")
})

test_that(".validate_codelists rejects duplicate dimension names", {
  expect_error(.validate_codelists(list(Region = "agg_a", Region = "agg_b")), "unique")
})

test_that(".validate_codelists rejects non-scalar values", {
  expect_error(.validate_codelists(list(Region = c("a", "b"))), "single non-missing")
})

test_that(".validate_codelists rejects empty string values", {
  expect_error(.validate_codelists(list(Region = "")), "empty")
})

test_that(".validate_codelists accepts valid entries", {
  expect_silent(.validate_codelists(list(Region = "agg_KommSummer")))
})

test_that(".validate_output_values accepts NULL/empty inputs silently", {
  expect_silent(.validate_output_values(NULL))
  expect_silent(.validate_output_values(list()))
})

test_that(".validate_output_values rejects invalid mode", {
  expect_error(.validate_output_values(list(Region = "all")), "aggregated.*single|single.*aggregated")
})

test_that(".validate_output_values accepts valid modes", {
  expect_silent(.validate_output_values(list(Region = "aggregated")))
  expect_silent(.validate_output_values(list(Region = "single")))
})

test_that(".validate_filter_names_against_metadata rejects unknown dimension", {
  metadata <- list(dimensions = list(Tid = list(), Region = list()))
  expect_error(
    .validate_filter_names_against_metadata(list(Foo = "bar"), metadata),
    "Unknown filter"
  )
})

test_that(".validate_filter_names_against_metadata passes with known dimensions", {
  metadata <- list(dimensions = list(Tid = list(), Region = list()))
  expect_silent(.validate_filter_names_against_metadata(list(Tid = "2020"), metadata))
  expect_silent(.validate_filter_names_against_metadata(list(), metadata))
})

test_that(".validate_main_args accepts valid args", {
  expect_silent(
    .validate_main_args(
      table_id = "07459",
      language = "no",
      as_tibble = TRUE,
      override_large_query = FALSE,
      table_format = "wide",
      include_singleton_dims = FALSE,
      include_status = FALSE,
      character_as_factor = TRUE,
      convert_quarter_to_yearqtr = TRUE,
      convert_month_to_yearmon = TRUE
    )
  )
})

test_that(".validate_main_args rejects non-boolean convert_quarter_to_yearqtr", {
  expect_error(
    .validate_main_args(
      table_id = "07459",
      language = "no",
      as_tibble = TRUE,
      override_large_query = FALSE,
      table_format = "wide",
      include_singleton_dims = FALSE,
      include_status = FALSE,
      character_as_factor = TRUE,
      convert_quarter_to_yearqtr = "yes",
      convert_month_to_yearmon = TRUE
    ),
    "convert_quarter_to_yearqtr"
  )
})

test_that(".validate_main_args rejects non-boolean character_as_factor", {
  expect_error(
    .validate_main_args(
      table_id = "07459",
      language = "no",
      as_tibble = TRUE,
      override_large_query = FALSE,
      table_format = "wide",
      include_singleton_dims = FALSE,
      include_status = FALSE,
      character_as_factor = "yes",
      convert_quarter_to_yearqtr = TRUE,
      convert_month_to_yearmon = TRUE
    ),
    "character_as_factor"
  )
})
