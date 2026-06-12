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
  expect_contains(colnames(x), c("date", "game_info_url"))
})

# The remaining live tests exercise the redesigned stats.ncaa.org parsers behind
# the same Akamai bypass; same opt-in gate.
skip_ncaa_live <- function() {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("chromote")
  testthat::skip_if(
    !nzchar(Sys.getenv("NCAA_CHROMOTE_TESTS")),
    "Set NCAA_CHROMOTE_TESTS=1 to run the live NCAA chromote tests"
  )
}

test_that("ncaa_team_player_stats() returns each stat category", {
  skip_ncaa_live()
  on.exit(try(baseballr:::.ncaa_browser_close(), silent = TRUE), add = TRUE)

  for (ty in c("batting", "pitching", "fielding")) {
    x <- ncaa_team_player_stats(team_id = 235, year = 2024, type = ty)
    expect_s3_class(x, "data.frame")
    expect_gt(nrow(x), 0)
    expect_contains(colnames(x), c("player_id", "player_name", "Jersey"))
  }
})

test_that("ncaa_pbp() parses the redesigned play-by-play page", {
  skip_ncaa_live()
  on.exit(try(baseballr:::.ncaa_browser_close(), silent = TRUE), add = TRUE)

  x <- ncaa_pbp(game_info_url = "https://stats.ncaa.org/contests/4525469/box_score")
  expect_s3_class(x, "data.frame")
  expect_gt(nrow(x), 0)
  expect_contains(colnames(x), c("inning", "batting", "score", "description"))
})

test_that("ncaa_game_logs() returns game and career spans", {
  skip_ncaa_live()
  on.exit(try(baseballr:::.ncaa_browser_close(), silent = TRUE), add = TRUE)

  g <- ncaa_game_logs(player_id = 8274878, year = 2024, type = "batting", span = "game")
  expect_s3_class(g, "data.frame")
  expect_gt(nrow(g), 0)
  expect_contains(colnames(g), c("player_id", "Date", "Opponent"))
})

test_that("ncaa_lineups() derives the batting order per team", {
  skip_ncaa_live()
  on.exit(try(baseballr:::.ncaa_browser_close(), silent = TRUE), add = TRUE)

  x <- ncaa_lineups(game_info_url = "https://stats.ncaa.org/contests/4525469/box_score")
  expect_s3_class(x, "data.frame")
  expect_gt(nrow(x), 0)
  expect_contains(colnames(x), c("player_name", "position", "batting_order", "team_name"))
})
