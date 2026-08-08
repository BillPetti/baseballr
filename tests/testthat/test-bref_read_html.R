# Tests for the bref_read_html retry helper (#344)
#
# Baseball Reference rate-limits (HTTP 429) scrapers that make rapid
# sequential requests. bref_read_html wraps the request in httr2::req_retry
# so 429/503 responses are retried with exponential backoff instead of
# crashing into a generic "object 'x' not found" error.

test_that("bref_read_html returns an XML document on success", {
  skip_if_offline("www.baseball-reference.com")
  skip_on_cran()

  url <- "https://www.baseball-reference.com/boxes?year=2026&month=08&day=01"
  doc <- bref_read_html(url)
  expect_s3_class(doc, "xml_document")
  expect_true(length(rvest::html_elements(doc, "table")) > 0)
})

test_that("bref_read_html raises an error on non-existent page", {
  skip_if_offline("www.baseball-reference.com")
  skip_on_cran()

  url <- "https://www.baseball-reference.com/this-page-does-not-exist-12345"
  expect_error(bref_read_html(url))
})
