# The shared request helpers turn off httr2's status errors, so a non-2xx
# response used to be parsed as data. They now raise a baseballr_http_error,
# which each wrapper's error handler reports with its cause.

.mock_response <- function(status, body, url = "https://statsapi.mlb.com/api/v1/windDirection?x=1") {
  force(status); force(body)
  function(req, ...) {
    httr2::response(
      status_code = status, url = url,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(body)
    )
  }
}

test_that(".baseballr_stop_for_http_status() passes 2xx and raises a classed error otherwise", {
  skip_on_cran()
  ok <- httr2::response(status_code = 200L, url = "https://x.test/a", body = charToRaw("[]"))
  expect_identical(.baseballr_stop_for_http_status(ok), ok)
  bad <- httr2::response(
    status_code = 401L, url = "https://x.test/a?key=secret",
    headers = list("Content-Type" = "application/json"),
    body = charToRaw('{"message":"Unauthorized key"}')
  )
  err <- expect_error(.baseballr_stop_for_http_status(bad), class = "baseballr_http_error")
  expect_s3_class(err, "baseballr_error")
  expect_identical(err$status, 401L)
  expect_match(conditionMessage(err), "HTTP 401 Unauthorized from https://x.test/a: Unauthorized key")
  expect_no_match(conditionMessage(err), "secret", fixed = TRUE)
})

test_that("mlb_api_call() and fg_api_call() raise on 401 / 500 instead of parsing the error body", {
  skip_on_cran()
  for (status in c(401L, 500L)) {
    local_mocked_bindings(
      req_perform = .mock_response(status, '{"messageNumber":1,"message":"Server error"}'),
      .package = "httr2"
    )
    expect_error(mlb_api_call("https://statsapi.mlb.com/api/v1/windDirection"),
                 class = "baseballr_http_error")
    expect_error(suppressMessages(fg_api_call("https://www.fangraphs.com/api/x")),
                 class = "baseballr_http_error")
  }
})

test_that("an MLB wrapper reports a 500 as its cause and a 200 as data", {
  skip_on_cran()
  local_mocked_bindings(
    req_perform = .mock_response(500L, '{"messageNumber":11,"message":"Internal error"}'),
    .package = "httr2"
  )
  msgs <- testthat::capture_messages(out <- mlb_wind_direction_codes())
  expect_null(out)
  expect_true(any(grepl("HTTP 500 Internal Server Error from https://statsapi.mlb.com/api/v1/windDirection: Internal error",
                        msgs, fixed = TRUE)))

  local_mocked_bindings(
    req_perform = .mock_response(200L, '[{"code":"In","description":"In from CF"}]'),
    .package = "httr2"
  )
  out <- mlb_wind_direction_codes()
  expect_s3_class(out, "baseballr_data")
  expect_identical(out$wind_direction_code, "In")
})

test_that("check_status() raises the classed error, so ESPN wrappers report the status", {
  skip_on_cran()
  local_mocked_bindings(.retry_request = function(...) {
    httr2::response(status_code = 404L, url = "https://sports.core.api.espn.com/v2/x?lang=en",
                    body = charToRaw("not found"))
  })
  msgs <- testthat::capture_messages(espn_mlb_player_seasons(athlete_id = 1L))
  expect_true(any(grepl("HTTP 404 Not Found from https://sports.core.api.espn.com/v2/x", msgs, fixed = TRUE)))
})
