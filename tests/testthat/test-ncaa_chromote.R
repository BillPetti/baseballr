test_that(".ncaa_is_blocked() flags Akamai block/challenge bodies", {
  # hard 403 page
  expect_true(baseballr:::.ncaa_is_blocked("<html><body>Access Denied</body></html>"))
  # soft HTTP-200 interstitial (bm-verify / akamai_validation / interstitial JS)
  expect_true(baseballr:::.ncaa_is_blocked(
    "<meta http-equiv=\"refresh\" content=\"5; URL='/x?bm-verify=ABC'\" />"
  ))
  expect_true(baseballr:::.ncaa_is_blocked(
    "<iframe src=\"https://stats.ncaa.org/akamai_validation.html\"></iframe>"
  ))
  expect_true(baseballr:::.ncaa_is_blocked("function triggerInterstitialChallenge(){}"))
  # a genuinely-served schedule page must NOT be flagged
  expect_false(baseballr:::.ncaa_is_blocked(
    "<html><body><table><tr><td>Date</td><td>Opponent</td></tr></table></body></html>"
  ))
  expect_false(baseballr:::.ncaa_is_blocked(""))
  expect_false(baseballr:::.ncaa_is_blocked(NULL))
})

test_that("ncaa_schedule_info() returns a schedule via the chromote fallback", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("chromote")
  # Live: launches headless Chrome to clear stats.ncaa.org's Akamai challenge.
  # Opt-in only (slow; needs Google Chrome installed).
  testthat::skip_if(
    !nzchar(Sys.getenv("NCAA_CHROMOTE_TESTS")),
    "Set NCAA_CHROMOTE_TESTS=1 to run the live NCAA chromote test"
  )
  on.exit(try(baseballr:::.ncaa_browser_close(), silent = TRUE), add = TRUE)

  x <- ncaa_schedule_info(team_id = 235, year = 2024, pbp_links = FALSE)
  expect_s3_class(x, "data.frame")
  expect_gt(nrow(x), 0)
  expect_true(all(c("date", "game_info_url") %in% colnames(x)))
})
