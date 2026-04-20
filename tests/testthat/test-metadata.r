make_raw_config <- function(
  max_data_cells = 100000,
  default_language = "no",
  default_data_format = "json-stat2",
  data_formats = list("json-stat2", "csv", "px")
) {
  list(
    apiVersion = "2.0",
    appVersion = "1.0",
    defaultLanguage = default_language,
    defaultDataFormat = default_data_format,
    maxDataCells = max_data_cells,
    dataFormats = data_formats
  )
}

make_raw_metadata <- function(
  table_id = "07459",
  dim_ids = c("Region", "Tid"),
  dim_sizes = c(2L, 3L)
) {
  dims <- lapply(seq_along(dim_ids), function(i) {
    id <- dim_ids[[i]]
    n <- dim_sizes[[i]]
    codes <- paste0(id, "_code_", seq_len(n))
    labels_list <- setNames(as.list(paste0(id, "_label_", seq_len(n))), codes)
    index_list <- setNames(as.list(seq(0, n - 1)), codes)
    list(
      label = paste0(id, "_label"),
      category = list(
        index = index_list,
        label = labels_list
      )
    )
  })
  names(dims) <- dim_ids

  list(
    id = as.list(dim_ids),
    size = as.list(dim_sizes),
    dimension = dims,
    label = "Test table",
    updated = "2024-01-01T00:00:00Z",
    role = list(time = list("Tid")),
    extension = list(px = list(tableid = table_id))
  )
}

test_that(".parse_api_config extracts known fields", {
  raw <- make_raw_config()
  result <- .parse_api_config(raw)
  expect_equal(result$default_language, "no")
  expect_equal(result$default_data_format, "json-stat2")
  expect_equal(result$max_data_cells, 100000L)
  expect_equal(result$source, "api")
  expect_true("csv" %in% result$data_formats)
})

test_that(".parse_api_config uses defaults for missing/invalid max_data_cells", {
  raw <- make_raw_config(max_data_cells = -1)
  result <- .parse_api_config(raw)
  expect_equal(result$max_data_cells, .default_api_config()$max_data_cells)
})

test_that(".default_api_config returns fallback config", {
  result <- .default_api_config()
  expect_equal(result$source, "fallback")
  expect_equal(result$default_data_format, "json-stat2")
  expect_equal(result$max_data_cells, 100000L)
})

test_that(".parse_table_metadata extracts dimensions and roles", {
  raw <- make_raw_metadata()
  result <- .parse_table_metadata(raw)
  expect_equal(result$table_id, "07459")
  expect_equal(names(result$dimensions), c("Region", "Tid"))
  expect_equal(result$dimensions$Region$code_count, 2L)
  expect_equal(result$dimensions$Tid$code_count, 3L)
  expect_equal(result$full_table_estimated_rows, 6L)
  expect_equal(result$roles$time[[1]], "Tid")
})

test_that(".parse_table_metadata correctly maps codes and labels", {
  raw <- make_raw_metadata(dim_ids = "Region", dim_sizes = 2L)
  result <- .parse_table_metadata(raw)
  expect_equal(result$dimensions$Region$codes, c("Region_code_1", "Region_code_2"))
  expect_equal(unname(result$dimensions$Region$labels), c("Region_label_1", "Region_label_2"))
})

test_that(".auto_complete_filters fills missing dimensions from metadata", {
  raw <- make_raw_metadata()
  metadata <- .parse_table_metadata(raw)
  result <- .auto_complete_filters(metadata, list(), list())
  expect_equal(result$Region, c("Region_code_1", "Region_code_2"))
  expect_equal(result$Tid, c("Tid_code_1", "Tid_code_2", "Tid_code_3"))
})

test_that(".auto_complete_filters respects provided filters", {
  raw <- make_raw_metadata()
  metadata <- .parse_table_metadata(raw)
  result <- .auto_complete_filters(metadata, list(Region = "Region_code_1"), list())
  expect_equal(result$Region, "Region_code_1")
  expect_equal(result$Tid, c("Tid_code_1", "Tid_code_2", "Tid_code_3"))
})

test_that(".auto_complete_filters assigns wildcard for codelist dimensions", {
  raw <- make_raw_metadata()
  metadata <- .parse_table_metadata(raw)
  result <- .auto_complete_filters(metadata, list(), list(Region = "agg_foo"))
  expect_equal(result$Region, "*")
})

test_that(".estimate_row_count returns product of filter sizes", {
  raw <- make_raw_metadata()
  metadata <- .parse_table_metadata(raw)
  completed <- list(Region = c("a", "b"), Tid = c("x", "y", "z"))
  expect_equal(.estimate_row_count(metadata, completed), 6L)
})

test_that(".estimate_row_count uses code_count for API expressions", {
  raw <- make_raw_metadata()
  metadata <- .parse_table_metadata(raw)
  completed <- list(Region = "*", Tid = c("x"))
  expect_equal(.estimate_row_count(metadata, completed), 2L)
})

test_that(".is_api_expression identifies wildcard and range expressions", {
  expect_true(.is_api_expression("*"))
  expect_true(.is_api_expression("top(3)"))
  expect_true(.is_api_expression("from(2020)"))
  expect_true(.is_api_expression("bottom(5)"))
  expect_true(.is_api_expression("range(2010,2020)"))
  expect_false(.is_api_expression("2020"))
  expect_false(.is_api_expression("01"))
})

test_that(".build_metadata_cache_key produces consistent keys", {
  expect_equal(.build_metadata_cache_key("07459", "no"), "07459::no")
  expect_equal(.build_metadata_cache_key("07459", "en"), "07459::en")
})
