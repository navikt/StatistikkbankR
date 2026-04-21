minimal_jsonstat <- function(values = c(10, 20), status = NULL) {
  list(
    id = list("Region", "Tid"),
    size = list(1L, 2L),
    dimension = list(
      Region = list(
        category = list(
          index = list("0301" = 0L),
          label = list("0301" = "Oslo")
        )
      ),
      Tid = list(
        category = list(
          index = list("2023" = 0L, "2024" = 1L),
          label = list("2023" = "2023", "2024" = "2024")
        )
      )
    ),
    value = as.list(values),
    status = status
  )
}

test_that(".parse_response_json parses valid JSON", {
  resp <- httr2::response(status_code = 200, body = charToRaw('{"a":1}'))
  parsed <- StatistikkbankR:::.parse_response_json(resp)
  expect_equal(parsed$a, 1)
})

test_that(".parse_response_json raises clear error for invalid JSON", {
  resp <- httr2::response(status_code = 200, body = charToRaw('{bad json}'))
  expect_error(
    StatistikkbankR:::.parse_response_json(resp),
    "Failed to parse API response as JSON"
  )
})

test_that(".tidy_response_json errors when value count mismatches grid", {
  parsed <- minimal_jsonstat(values = c(10))
  expect_error(
    StatistikkbankR:::.tidy_response_json(parsed, as_tibble = FALSE, table_format = "wide"),
    "does not match observation grid size"
  )
})

test_that(".tidy_response_json long format includes status when requested", {
  parsed <- minimal_jsonstat(values = c(10, 20), status = list(`0` = "A", `1` = "B"))
  out <- StatistikkbankR:::.tidy_response_json(
    parsed,
    as_tibble = FALSE,
    table_format = "long",
    include_singleton_dims = TRUE,
    include_status = TRUE,
    character_as_factor = FALSE,
    convert_quarter_to_yearqtr = FALSE,
    convert_month_to_yearmon = FALSE
  )

  expect_true("status" %in% names(out))
  expect_equal(sort(unique(out$status)), c("A", "B"))
  expect_true("observation_id" %in% names(out))
})

test_that(".wide_to_long_grid returns empty when all dims are singleton and excluded", {
  wide_grid <- data.frame(
    Region_code = "0301",
    Region_label = "Oslo",
    value = 10,
    stringsAsFactors = FALSE
  )
  dimension_ids <- c("Region")
  completed_filters <- list(Region = "0301")

  out <- StatistikkbankR:::.wide_to_long_grid(
    wide_grid,
    dimension_ids,
    completed_filters,
    include_singleton_dims = FALSE
  )

  expect_equal(nrow(out), 0)
  expect_named(out, c("observation_id", "dimension", "code", "label", "value"))
})

test_that(".wide_to_long_grid expands dimensions when requested", {
  wide_grid <- data.frame(
    Region_code = c("0301", "1103"),
    Region_label = c("Oslo", "Stavanger"),
    Tid_code = c("2023", "2024"),
    Tid_label = c("2023", "2024"),
    value = c(10, 20),
    stringsAsFactors = FALSE
  )

  out <- StatistikkbankR:::.wide_to_long_grid(
    wide_grid,
    dimension_ids = c("Region", "Tid"),
    completed_filters = list(Region = c("0301", "1103"), Tid = c("2023", "2024")),
    include_singleton_dims = FALSE
  )

  expect_equal(nrow(out), 4)
  expect_true(all(c("Region", "Tid") %in% out$dimension))
  expect_equal(max(out$observation_id), 2)
})
