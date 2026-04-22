test_that(".safe_response_body returns body string for valid response", {
  resp <- httr2::response(status_code = 200, body = charToRaw("ok"))
  out <- StatistikkbankR:::.safe_response_body(resp)
  expect_equal(out, "ok")
})

test_that(".safe_response_body returns fallback string on invalid response object", {
  out <- StatistikkbankR:::.safe_response_body(list())
  expect_equal(out, "(unable to read response body)")
})

test_that(".is_transient_ssb_response is TRUE for 429 and 503", {
  resp_429 <- httr2::response(status_code = 429)
  resp_503 <- httr2::response(status_code = 503)

  expect_true(StatistikkbankR:::.is_transient_ssb_response(resp_429))
  expect_true(StatistikkbankR:::.is_transient_ssb_response(resp_503))
})

test_that(".is_transient_ssb_response is FALSE for non-transient status", {
  resp_200 <- httr2::response(status_code = 200)
  expect_false(StatistikkbankR:::.is_transient_ssb_response(resp_200))
})

test_that(".is_transient_ssb_response is FALSE for invalid response object", {
  expect_false(StatistikkbankR:::.is_transient_ssb_response(list()))
})

test_that(".retry_after_seconds parses Retry-After header", {
  resp <- httr2::response(status_code = 429, headers = list(`Retry-After` = "60"))
  expect_equal(StatistikkbankR:::.retry_after_seconds(resp), 60)
})

test_that(".retry_after_seconds returns NULL when header missing", {
  resp <- httr2::response(status_code = 429)
  expect_null(StatistikkbankR:::.retry_after_seconds(resp))
})

test_that(".retry_after_seconds returns NULL for non-numeric header", {
  resp <- httr2::response(status_code = 429, headers = list(`Retry-After` = "soon"))
  expect_null(StatistikkbankR:::.retry_after_seconds(resp))
})

test_that(".retry_after_seconds returns NULL for negative header", {
  resp <- httr2::response(status_code = 429, headers = list(`Retry-After` = "-5"))
  expect_null(StatistikkbankR:::.retry_after_seconds(resp))
})
