make_search_page_result <- function(page_number, total_pages, ids) {
  data <- data.frame(
    id = ids,
    label = paste("Label", ids),
    description = NA_character_,
    updated = NA_character_,
    first_period = NA_character_,
    last_period = NA_character_,
    category = NA_character_,
    source = NA_character_,
    variables = "Region, Tid",
    stringsAsFactors = FALSE
  )

  list(
    data = data,
    page_info = list(
      page_number = as.integer(page_number),
      page_size = 2L,
      total_elements = 4L,
      total_pages = as.integer(total_pages)
    )
  )
}

test_that("ssb_search keeps pagination attributes for single page", {
  parse_call <- 0L

  with_mocked_bindings(
    {
      out <- ssbapi::ssb_search(query = "befolkning", page = 2L, page_size = 2L, fetch_all = FALSE, as_tibble = FALSE)
      expect_equal(nrow(out), 2L)
      expect_equal(attr(out, "page_number"), 2L)
      expect_equal(attr(out, "page_size"), 2L)
      expect_equal(attr(out, "total_elements"), 4L)
      expect_equal(attr(out, "total_pages"), 3L)
    },
    .build_tables_request_spec = function(query, language, page, page_size) {
      list(query = query, language = language, page = page, page_size = page_size)
    },
    .execute_request_spec = function(request_spec) request_spec,
    .parse_response_json = function(resp) {
      parse_call <<- parse_call + 1L
      list(page = resp$page)
    },
    .parse_tables_response = function(parsed) {
      if (parse_call == 1L) {
        make_search_page_result(page_number = 2L, total_pages = 3L, ids = c("a", "b"))
      } else {
        stop("Unexpected extra page parse")
      }
    },
    .package = "ssbapi"
  )
})

test_that("ssb_search fetch_all combines pages and updates final attributes", {
  parse_call <- 0L

  with_mocked_bindings(
    {
      out <- ssbapi::ssb_search(query = "befolkning", page = 1L, page_size = 2L, fetch_all = TRUE, as_tibble = FALSE)
      expect_equal(nrow(out), 4L)
      expect_equal(out$id, c("a", "b", "c", "d"))
      expect_equal(attr(out, "page_number"), 2L)
      expect_equal(attr(out, "page_size"), 2L)
      expect_equal(attr(out, "total_elements"), 4L)
      expect_equal(attr(out, "total_pages"), 2L)
    },
    .build_tables_request_spec = function(query, language, page, page_size) {
      list(query = query, language = language, page = page, page_size = page_size)
    },
    .execute_request_spec = function(request_spec) request_spec,
    .parse_response_json = function(resp) {
      parse_call <<- parse_call + 1L
      list(page = resp$page)
    },
    .parse_tables_response = function(parsed) {
      if (parse_call == 1L) {
        return(make_search_page_result(page_number = 1L, total_pages = 2L, ids = c("a", "b")))
      }

      make_search_page_result(page_number = 2L, total_pages = 2L, ids = c("c", "d"))
    },
    .package = "ssbapi"
  )
})

test_that("ssb_search returns tibble when as_tibble is TRUE", {
  with_mocked_bindings(
    {
      out <- ssbapi::ssb_search(query = "befolkning", fetch_all = FALSE, as_tibble = TRUE)
      expect_s3_class(out, "tbl_df")
    },
    .build_tables_request_spec = function(query, language, page, page_size) {
      list(query = query, language = language, page = page, page_size = page_size)
    },
    .execute_request_spec = function(request_spec) request_spec,
    .parse_response_json = function(resp) list(page = resp$page),
    .parse_tables_response = function(parsed) make_search_page_result(page_number = 1L, total_pages = 1L, ids = c("a", "b")),
    .package = "ssbapi"
  )
})

test_that("ssb_search rejects invalid query input", {
  expect_error(ssbapi::ssb_search(query = 123), "query")
})
