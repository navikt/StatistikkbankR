test_that(".normalise_filters trims whitespace and deduplicates", {
  result <- .normalise_filters(list(Tid = c("  2020 ", "2021", "2020")))
  expect_equal(result$Tid, c("2020", "2021"))
})

test_that(".normalise_filters converts numeric to character", {
  result <- .normalise_filters(list(Tid = 2020L))
  expect_equal(result$Tid, "2020")
})

test_that(".normalise_filters returns empty list for empty input", {
  expect_equal(.normalise_filters(list()), list())
})

test_that(".normalise_codelists trims and converts to character", {
  result <- .normalise_codelists(list(Region = "  agg_KommSummer  "))
  expect_equal(result$Region, "agg_KommSummer")
})

test_that(".normalise_codelists returns empty list for NULL input", {
  expect_equal(.normalise_codelists(NULL), list())
  expect_equal(.normalise_codelists(list()), list())
})

test_that(".normalise_output_values lowercases and trims", {
  result <- .normalise_output_values(list(Region = "  Aggregated  "))
  expect_equal(result$Region, "aggregated")
})

test_that(".normalise_output_values returns empty list for NULL input", {
  expect_equal(.normalise_output_values(NULL), list())
})
