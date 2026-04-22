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

fetch_raw_jsonstat <- function(table_id, language, filters) {
  endpoint <- paste0("https://data.ssb.no/api/pxwebapi/v2/tables/", table_id, "/data")
  params <- list(lang = language, outputFormat = "json-stat2")

  for (nm in names(filters)) {
    params[[paste0("valueCodes[", nm, "]")]] <- filters[[nm]]
  }

  req <- httr2::request(endpoint)
  req <- httr2::req_url_query(req, !!!params)
  resp <- httr2::req_perform(req)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

parse_jsonstat_independently <- function(parsed_json) {
  dimension_ids <- unlist(parsed_json$id, use.names = FALSE)
  raw_dimensions <- parsed_json$dimension

  code_values <- setNames(vector("list", length(dimension_ids)), dimension_ids)
  label_values <- setNames(vector("list", length(dimension_ids)), dimension_ids)

  for (dimension_id in dimension_ids) {
    raw_dimension <- raw_dimensions[[dimension_id]]
    category_index <- unlist(raw_dimension$category$index, use.names = TRUE)
    codes <- names(sort(category_index))
    labels <- vapply(codes, function(code) raw_dimension$category$label[[code]], character(1))

    code_values[[dimension_id]] <- codes
    label_values[[dimension_id]] <- labels
  }

  grid_dimension_ids <- rev(dimension_ids)

  code_grid <- expand.grid(
    code_values[grid_dimension_ids],
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  code_grid <- code_grid[dimension_ids]
  names(code_grid) <- paste0(dimension_ids, "_code")

  label_grid <- expand.grid(
    label_values[grid_dimension_ids],
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  label_grid <- label_grid[dimension_ids]
  names(label_grid) <- paste0(dimension_ids, "_label")

  out <- cbind(code_grid, label_grid, stringsAsFactors = FALSE)

  values <- vapply(
    parsed_json$value,
    function(x) {
      if (is.null(x)) {
        return(NA_real_)
      }
      as.numeric(x)
    },
    numeric(1)
  )

  out$value <- values
  out
}

normalise_e2e_compare_df <- function(df) {
  out <- df
  for (nm in names(out)) {
    if (!nm %in% c("value", "status")) {
      out[[nm]] <- as.character(out[[nm]])
    }
  }
  out
}

expect_package_matches_live_jsonstat <- function(table_id, language = "no") {
  filters <- pick_one_code_per_dimension(table_id, language = language)

  pkg_call <- c(
    list(
      table_id,
      language = language,
      as_tibble = FALSE,
      table_format = "wide",
      include_singleton_dims = TRUE,
      character_as_factor = FALSE,
      convert_quarter_to_yearqtr = FALSE,
      convert_month_to_yearmon = FALSE,
      cache = FALSE,
      refresh_metadata = TRUE
    ),
    filters
  )

  package_df <- do.call(StatistikkbankR::get_ssb_data, pkg_call)
  live_json <- fetch_raw_jsonstat(table_id, language, filters)
  expected_df <- parse_jsonstat_independently(live_json)

  compare_cols <- intersect(names(expected_df), names(package_df))
  package_cmp <- normalise_e2e_compare_df(package_df[, compare_cols, drop = FALSE])
  expected_cmp <- normalise_e2e_compare_df(expected_df[, compare_cols, drop = FALSE])

  testthat::expect_equal(nrow(package_df), nrow(expected_df))
  testthat::expect_equal(package_cmp, expected_cmp)
}

test_that("Live E2E: table 07459 matches direct JSON-stat response", {
  skip_unless_live_ssb()
  expect_package_matches_live_jsonstat("07459")
})

test_that("Live E2E: table 05803 matches direct JSON-stat response", {
  skip_unless_live_ssb()
  expect_package_matches_live_jsonstat("05803")
})

test_that("Live E2E: table 12452 matches direct JSON-stat response", {
  skip_unless_live_ssb()
  expect_package_matches_live_jsonstat("12452")
})

test_that("Live E2E: table 14657 matches direct JSON-stat response", {
  skip_unless_live_ssb()
  expect_package_matches_live_jsonstat("14657")
})

test_that("Live E2E: table 13470 matches direct JSON-stat response", {
  skip_unless_live_ssb()
  expect_package_matches_live_jsonstat("13470")
})

test_that("Live E2E: table 06194 matches direct JSON-stat response", {
  skip_unless_live_ssb()
  expect_package_matches_live_jsonstat("06194")
})
