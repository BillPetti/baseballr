# Offline replay of recorded HTTP responses for the test-*_offline.R files.
#
# Fixtures are recorded by data-raw/http_fixtures/record_fixtures.R and listed in
# fixtures/http/index.json by URL. Inside a test, every httr2 request is answered
# from that index; a URL that was never recorded gets a 404, so the test fails
# instead of reaching the network.

http_fixture_index <- local({
  index <- NULL
  function() {
    if (is.null(index)) {
      index <<- jsonlite::read_json(testthat::test_path("fixtures", "http", "index.json"))
    }
    index
  }
})

local_http_fixtures <- function(env = parent.frame()) {
  testthat::skip_if_not(
    exists("local_mocked_responses", envir = asNamespace("httr2")),
    "httr2 >= 1.1.0 is needed to replay recorded responses"
  )
  index <- http_fixture_index()
  dir <- testthat::test_path("fixtures", "http")
  httr2::local_mocked_responses(function(req) {
    hit <- index[[req$url]]
    if (is.null(hit)) {
      return(httr2::response(status_code = 404L, url = req$url))
    }
    path <- file.path(dir, hit$file)
    httr2::response(
      status_code = hit$status, url = req$url,
      headers = list(`Content-Type` = hit$type),
      body = memDecompress(readBin(path, "raw", file.size(path)), "gzip")
    )
  }, env = env)
}

expect_recorded_data <- function(x) {
  testthat::expect_false(is.null(x))
  if (is.data.frame(x)) {
    testthat::expect_gt(nrow(x), 0)
  } else {
    testthat::expect_gt(length(x), 0)
  }
}
