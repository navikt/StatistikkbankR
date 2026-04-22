should_run_live_ssb_tests <- function() {
  tolower(Sys.getenv("RUN_SSB_E2E", "false")) %in% c("1", "true", "yes")
}

skip_unless_live_ssb <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  if (!should_run_live_ssb_tests()) {
    testthat::skip("Set RUN_SSB_E2E=true to run live SSB end-to-end tests.")
  }
}

pick_one_code_per_dimension <- function(table_id, language = "no") {
  desc <- StatistikkbankR::ssb_describe(table_id, language = language)
  dimension_ids <- desc$dimensions$dimension

  filters <- lapply(dimension_ids, function(dim_id) {
    codes_df <- StatistikkbankR::ssb_codes(table_id, dim_id, language = language)
    codes_df$code[[1]]
  })

  names(filters) <- dimension_ids
  filters
}

test_that("get_ssb_data converts quarters to yearqtr by default", {
  skip_if_not_installed("zoo")
  skip_unless_live_ssb()

  filters <- pick_one_code_per_dimension("12452", language = "no")
  out <- do.call(
    StatistikkbankR::get_ssb_data,
    c(
      list(
        "12452",
        language = "no",
        as_tibble = FALSE,
        character_as_factor = FALSE,
        cache = FALSE,
        refresh_metadata = TRUE
      ),
      filters
    )
  )

  tid_col <- grep("Tid", names(out), value = TRUE)[1]
  expect_false(is.na(tid_col))
  expect_true("yearqtr" %in% class(out[[tid_col]]))
})

test_that("get_ssb_data can skip quarter conversion", {
  skip_unless_live_ssb()

  filters <- pick_one_code_per_dimension("12452", language = "no")
  out <- do.call(
    StatistikkbankR::get_ssb_data,
    c(
      list(
        "12452",
        language = "no",
        as_tibble = FALSE,
        character_as_factor = FALSE,
        convert_quarter_to_yearqtr = FALSE,
        cache = FALSE,
        refresh_metadata = TRUE
      ),
      filters
    )
  )

  tid_col <- grep("Tid", names(out), value = TRUE)[1]
  expect_false(is.na(tid_col))
  expect_true(is.character(out[[tid_col]]))
  expect_match(out[[tid_col]][1], "^[0-9]{4}[Kk][1-4]$")
})

test_that("get_ssb_data with table 13966 converts months to yearmon by default", {
  skip_if_not_installed("zoo")
  skip_unless_live_ssb()

  filters <- pick_one_code_per_dimension("13966", language = "no")
  out <- do.call(
    StatistikkbankR::get_ssb_data,
    c(
      list(
        "13966",
        language = "no",
        as_tibble = FALSE,
        character_as_factor = FALSE,
        cache = FALSE,
        refresh_metadata = TRUE
      ),
      filters
    )
  )

  tid_col <- grep("Tid", names(out), value = TRUE)[1]
  expect_false(is.na(tid_col))
  expect_true("yearmon" %in% class(out[[tid_col]]))
})

test_that("get_ssb_data with table 13966 can skip month conversion", {
  skip_unless_live_ssb()

  filters <- pick_one_code_per_dimension("13966", language = "no")
  out <- do.call(
    StatistikkbankR::get_ssb_data,
    c(
      list(
        "13966",
        language = "no",
        as_tibble = FALSE,
        character_as_factor = FALSE,
        convert_month_to_yearmon = FALSE,
        cache = FALSE,
        refresh_metadata = TRUE
      ),
      filters
    )
  )

  tid_col <- grep("Tid", names(out), value = TRUE)[1]
  expect_false(is.na(tid_col))
  expect_true(is.character(out[[tid_col]]))
  expect_match(out[[tid_col]][1], "^[0-9]{4}[Mm](0[1-9]|1[0-2])$")
})
