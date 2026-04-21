make_minimal_json_stat <- function() {
  list(
    id = list("Tid", "Region"),
    dimension = list(
      Tid = list(
        category = list(
          index = list(`2024K1` = 0, `2024K2` = 1),
          label = list(`2024K1` = "2024K1", `2024K2` = "2024K2")
        )
      ),
      Region = list(
        category = list(
          index = list(`0301` = 0),
          label = list(`0301` = "Oslo")
        )
      )
    ),
    value = list(10, 12)
  )
}

make_minimal_metadata <- function() {
  list(
    roles = list(time = list("Tid"))
  )
}

make_two_dim_json_stat <- function() {
  list(
    id = list("Yrke", "Tid"),
    dimension = list(
      Yrke = list(
        category = list(
          index = list(`0-9` = 0, `1` = 1),
          label = list(`0-9` = "Alle yrker", `1` = "Ledere")
        )
      ),
      Tid = list(
        category = list(
          index = list(`2024K1` = 0, `2024K2` = 1),
          label = list(`2024K1` = "2024K1", `2024K2` = "2024K2")
        )
      )
    ),
    value = list(10, 11, 20, 21)
  )
}

test_that("quarter strings are reformatted as year-quarter", {
  parsed <- make_minimal_json_stat()
  out <- .tidy_response_json(
    parsed_json = parsed,
    as_tibble = FALSE,
    table_format = "wide",
    include_singleton_dims = FALSE,
    metadata = make_minimal_metadata(),
    include_status = FALSE,
    character_as_factor = TRUE,
    quarter_as = "year_quarter"
  )

  expect_equal(as.character(out$Tid_code), c("2024-Q1", "2024-Q2"))
  expect_equal(as.character(out$Tid_label), c("2024-Q1", "2024-Q2"))
})

test_that("character columns are converted to factors except quarter columns", {
  parsed <- make_minimal_json_stat()
  out <- .tidy_response_json(
    parsed_json = parsed,
    as_tibble = FALSE,
    table_format = "wide",
    include_singleton_dims = FALSE,
    metadata = make_minimal_metadata(),
    include_status = FALSE,
    character_as_factor = TRUE,
    quarter_as = "year_quarter"
  )

  expect_true(is.factor(out$Region_code))
  expect_true(is.factor(out$Region_label))
  expect_true(is.character(out$Tid_code))
  expect_true(is.character(out$Tid_label))
})

test_that("quarter_as character leaves quarter values unchanged", {
  parsed <- make_minimal_json_stat()
  out <- .tidy_response_json(
    parsed_json = parsed,
    as_tibble = FALSE,
    table_format = "wide",
    include_singleton_dims = FALSE,
    metadata = make_minimal_metadata(),
    include_status = FALSE,
    character_as_factor = TRUE,
    quarter_as = "character"
  )

  expect_equal(as.character(out$Tid_code), c("2024K1", "2024K2"))
  expect_equal(as.character(out$Tid_label), c("2024K1", "2024K2"))
})

test_that("character_as_factor FALSE preserves character columns", {
  parsed <- make_minimal_json_stat()
  out <- .tidy_response_json(
    parsed_json = parsed,
    as_tibble = FALSE,
    table_format = "wide",
    include_singleton_dims = FALSE,
    metadata = make_minimal_metadata(),
    include_status = FALSE,
    character_as_factor = FALSE,
    quarter_as = "year_quarter"
  )

  expect_true(is.character(out$Region_code))
  expect_true(is.character(out$Region_label))
})

test_that("non-quarter character columns are not reformatted as quarters", {
  grid <- data.frame(
    foo = c("abc", "def"),
    bar = c("2024K1", "x"),
    stringsAsFactors = FALSE
  )

  converted <- .apply_output_types(
    grid,
    character_as_factor = TRUE,
    quarter_as = "year_quarter"
  )

  expect_true(is.factor(converted$foo))
  expect_true(is.factor(converted$bar))
  expect_equal(as.character(converted$bar), c("2024K1", "x"))
})

test_that("JSON-stat values align with last dimension varying fastest", {
  parsed <- make_two_dim_json_stat()
  out <- .tidy_response_json(
    parsed_json = parsed,
    as_tibble = FALSE,
    table_format = "wide",
    include_singleton_dims = FALSE,
    metadata = make_minimal_metadata(),
    include_status = FALSE,
    character_as_factor = FALSE,
    quarter_as = "character"
  )

  expect_equal(out$Yrke_code, c("0-9", "0-9", "1", "1"))
  expect_equal(out$Tid_code, c("2024K1", "2024K2", "2024K1", "2024K2"))
  expect_equal(out$value, c(10, 11, 20, 21))
})
