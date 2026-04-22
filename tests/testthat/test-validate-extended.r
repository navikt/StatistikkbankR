test_that(".validate_main_args rejects invalid table_id", {
  expect_error(.validate_main_args("", "no", TRUE, FALSE, "wide", FALSE, FALSE, TRUE, TRUE, TRUE), "non-empty")
})

test_that(".validate_main_args rejects invalid language", {
  expect_error(.validate_main_args("12452", "fr", TRUE, FALSE, "wide", FALSE, FALSE, TRUE, TRUE, TRUE), "\"no\" or \"en\"")
})

test_that(".validate_main_args rejects invalid as_tibble", {
  expect_error(.validate_main_args("12452", "no", "yes", FALSE, "wide", FALSE, FALSE, TRUE, TRUE, TRUE), "TRUE or FALSE")
})

test_that(".validate_main_args rejects invalid override_large_query", {
  expect_error(.validate_main_args("12452", "no", TRUE, "yes", "wide", FALSE, FALSE, TRUE, TRUE, TRUE), "TRUE or FALSE")
})

test_that(".validate_main_args rejects invalid table_format", {
  expect_error(.validate_main_args("12452", "no", TRUE, FALSE, "pivot", FALSE, FALSE, TRUE, TRUE, TRUE), "wide.*long|long.*wide")
})

test_that(".validate_main_args rejects empty table_format", {
  expect_error(.validate_main_args("12452", "no", TRUE, FALSE, "", FALSE, FALSE, TRUE, TRUE, TRUE), "wide.*long|long.*wide")
})

test_that(".validate_main_args rejects invalid include_singleton_dims", {
  expect_error(.validate_main_args("12452", "no", TRUE, FALSE, "wide", "maybe", FALSE, TRUE, TRUE, TRUE), "TRUE or FALSE")
})

test_that(".validate_main_args rejects invalid include_status", {
  expect_error(.validate_main_args("12452", "no", TRUE, FALSE, "wide", FALSE, "maybe", TRUE, TRUE, TRUE), "TRUE or FALSE")
})

test_that(".validate_main_args rejects invalid character_as_factor", {
  expect_error(.validate_main_args("12452", "no", TRUE, FALSE, "wide", FALSE, FALSE, "yes", TRUE, TRUE), "TRUE or FALSE")
})

test_that(".validate_main_args rejects invalid convert_quarter_to_yearqtr", {
  expect_error(.validate_main_args("12452", "no", TRUE, FALSE, "wide", FALSE, FALSE, TRUE, "ISO_week", TRUE), "convert_quarter_to_yearqtr")
})

test_that(".validate_main_args rejects invalid convert_month_to_yearmon", {
  expect_error(.validate_main_args("12452", "no", TRUE, FALSE, "wide", FALSE, FALSE, TRUE, TRUE, ""), "convert_month_to_yearmon")
})

test_that(".validate_main_args accepts valid long format", {
  expect_silent(.validate_main_args("12452", "no", TRUE, FALSE, "long", FALSE, FALSE, TRUE, TRUE, TRUE))
})

test_that(".validate_main_args accepts valid boolean conversion flags", {
  expect_silent(.validate_main_args("12452", "no", TRUE, FALSE, "wide", FALSE, FALSE, TRUE, FALSE, FALSE))
})

test_that(".validate_main_args with different language", {
  expect_silent(.validate_main_args("12452", "en", FALSE, TRUE, "long", TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that(".validate_chunking_args rejects non-numeric max_get_query_chars", {
  expect_error(.validate_chunking_args("5000", TRUE), "numeric")
})

test_that(".validate_chunking_args rejects NA max_get_query_chars", {
  expect_error(.validate_chunking_args(NA_real_, TRUE), "numeric")
})

test_that(".validate_chunking_args rejects zero max_get_query_chars", {
  expect_error(.validate_chunking_args(0, TRUE), "greater than 0")
})

test_that(".validate_chunking_args rejects negative max_get_query_chars", {
  expect_error(.validate_chunking_args(-1000, TRUE), "greater than 0")
})

test_that(".validate_chunking_args rejects invalid show_chunk_progress", {
  expect_error(.validate_chunking_args(5000, "yes"), "TRUE or FALSE")
})

test_that(".validate_chunking_args accepts valid positive integer", {
  expect_silent(.validate_chunking_args(5000, TRUE))
  expect_silent(.validate_chunking_args(5000, FALSE))
})

test_that(".validate_chunking_args accepts float max_get_query_chars", {
  expect_silent(.validate_chunking_args(5000.5, TRUE))
})

test_that(".normalise_codelists returns empty list for NULL", {
  result <- .normalise_codelists(NULL)
  expect_equal(result, list())
})

test_that(".normalise_codelists returns empty list for empty list", {
  result <- .normalise_codelists(list())
  expect_equal(result, list())
})

test_that(".normalise_codelists trims whitespace", {
  codelists <- list(Region = "  codelist_123  ")
  result <- .normalise_codelists(codelists)
  expect_equal(result$Region, "codelist_123")
})

test_that(".normalise_codelists handles multiple entries", {
  codelists <- list(Region = "agg_KommSummer", Tid = "agg_Year")
  result <- .normalise_codelists(codelists)
  expect_equal(result$Region, "agg_KommSummer")
  expect_equal(result$Tid, "agg_Year")
})

test_that(".normalise_codelists converts to character", {
  codelists <- list(Region = 123)
  result <- .normalise_codelists(codelists)
  expect_equal(result$Region, "123")
})

test_that(".normalise_output_values returns empty list for NULL", {
  result <- .normalise_output_values(NULL)
  expect_equal(result, list())
})

test_that(".normalise_output_values returns empty list for empty list", {
  result <- .normalise_output_values(list())
  expect_equal(result, list())
})

test_that(".normalise_output_values normalizes to lowercase", {
  output_values <- list(Region = "AGGREGATED")
  result <- .normalise_output_values(output_values)
  expect_equal(result$Region, "aggregated")
})

test_that(".normalise_output_values trims whitespace", {
  output_values <- list(Region = "  single  ")
  result <- .normalise_output_values(output_values)
  expect_equal(result$Region, "single")
})

test_that(".normalise_output_values handles multiple entries", {
  output_values <- list(Region = "aggregated", Tid = "SINGLE")
  result <- .normalise_output_values(output_values)
  expect_equal(result$Region, "aggregated")
  expect_equal(result$Tid, "single")
})
