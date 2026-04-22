clear_metadata_caches <- function() {
  rm(list = ls(envir = StatistikkbankR:::.config_cache, all.names = TRUE), envir = StatistikkbankR:::.config_cache)
  rm(list = ls(envir = StatistikkbankR:::.metadata_cache, all.names = TRUE), envir = StatistikkbankR:::.metadata_cache)
}

minimal_raw_metadata <- function() {
  list(
    id = list("Region"),
    size = list(1L),
    dimension = list(
      Region = list(
        label = "Region",
        category = list(
          index = list("0301" = 0L),
          label = list("0301" = "Oslo")
        )
      )
    ),
    label = "Test",
    updated = "2024-01-01T00:00:00Z",
    role = list(time = list()),
    extension = list(px = list(tableid = "00000"))
  )
}

test_that(".get_api_config caches result when cache=TRUE", {
  clear_metadata_caches()
  calls <- 0L

  run_with_exported_helper_mocks(
    {
      a <- StatistikkbankR:::.get_api_config(cache = TRUE, refresh_metadata = FALSE)
      b <- StatistikkbankR:::.get_api_config(cache = TRUE, refresh_metadata = FALSE)
      expect_equal(calls, 1L)
      expect_equal(a$max_data_cells, b$max_data_cells)
    },
    overrides = list(
      .fetch_api_config = function() {
        calls <<- calls + 1L
        list(maxDataCells = 12345L)
      }
    )
  )
})

test_that(".get_api_config bypasses cache when refresh_metadata=TRUE", {
  clear_metadata_caches()
  calls <- 0L

  run_with_exported_helper_mocks(
    {
      StatistikkbankR:::.get_api_config(cache = TRUE, refresh_metadata = FALSE)
      StatistikkbankR:::.get_api_config(cache = TRUE, refresh_metadata = TRUE)
      expect_equal(calls, 2L)
    },
    overrides = list(
      .fetch_api_config = function() {
        calls <<- calls + 1L
        list(maxDataCells = 54321L)
      }
    )
  )
})

test_that(".get_api_config does not persist when cache=FALSE", {
  clear_metadata_caches()
  calls <- 0L

  run_with_exported_helper_mocks(
    {
      StatistikkbankR:::.get_api_config(cache = FALSE, refresh_metadata = FALSE)
      StatistikkbankR:::.get_api_config(cache = FALSE, refresh_metadata = FALSE)
      expect_equal(calls, 2L)
      expect_false(exists("config", envir = StatistikkbankR:::.config_cache, inherits = FALSE))
    },
    overrides = list(
      .fetch_api_config = function() {
        calls <<- calls + 1L
        list(maxDataCells = 99999L)
      }
    )
  )
})

test_that(".get_api_config falls back to default on fetch error", {
  clear_metadata_caches()

  run_with_exported_helper_mocks(
    {
      out <- StatistikkbankR:::.get_api_config(cache = FALSE, refresh_metadata = FALSE)
      expect_equal(out$source, "fallback")
      expect_equal(out$max_data_cells, 100000L)
    },
    overrides = list(
      .fetch_api_config = function() stop("boom")
    )
  )
})

test_that(".get_table_metadata caches result when cache=TRUE", {
  clear_metadata_caches()
  calls <- 0L

  run_with_exported_helper_mocks(
    {
      a <- StatistikkbankR:::.get_table_metadata("00000", "no", cache = TRUE, refresh_metadata = FALSE)
      b <- StatistikkbankR:::.get_table_metadata("00000", "no", cache = TRUE, refresh_metadata = FALSE)
      expect_equal(calls, 1L)
      expect_equal(a$table_id, b$table_id)
    },
    overrides = list(
      .fetch_table_metadata = function(table_id, language) {
        calls <<- calls + 1L
        minimal_raw_metadata()
      }
    )
  )
})

test_that(".get_table_metadata bypasses cache when refresh_metadata=TRUE", {
  clear_metadata_caches()
  calls <- 0L

  run_with_exported_helper_mocks(
    {
      StatistikkbankR:::.get_table_metadata("00000", "no", cache = TRUE, refresh_metadata = FALSE)
      StatistikkbankR:::.get_table_metadata("00000", "no", cache = TRUE, refresh_metadata = TRUE)
      expect_equal(calls, 2L)
    },
    overrides = list(
      .fetch_table_metadata = function(table_id, language) {
        calls <<- calls + 1L
        minimal_raw_metadata()
      }
    )
  )
})

test_that(".get_table_metadata uses language in cache key", {
  clear_metadata_caches()
  calls <- 0L

  run_with_exported_helper_mocks(
    {
      StatistikkbankR:::.get_table_metadata("00000", "no", cache = TRUE, refresh_metadata = FALSE)
      StatistikkbankR:::.get_table_metadata("00000", "en", cache = TRUE, refresh_metadata = FALSE)
      expect_equal(calls, 2L)
    },
    overrides = list(
      .fetch_table_metadata = function(table_id, language) {
        calls <<- calls + 1L
        minimal_raw_metadata()
      }
    )
  )
})
