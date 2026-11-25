# espn_mlb_team_injuries() reads ESPN's core API: a list of $ref links, one
# request per injury and one per athlete. The fixtures under
# fixtures/espn_mlb_team_injuries/ are responses recorded 2026-11-25 for the
# Texas Rangers (team 13), trimmed to two injuries and the athlete fields the
# function reads. team_99_empty.json is that list response with `items`
# emptied, standing in for a team with no injuries. The "no" short and long
# comments are what ESPN sends for these two manually entered injuries.

core <- "sports.core.api.espn.com/v2/sports/baseball/leagues/mlb"
injury_fixture_urls <- c(
  "https://%s/teams/13/injuries?limit=200&lang=en&region=us" = "team_13.json",
  "https://%s/teams/99/injuries?limit=200&lang=en&region=us" = "team_99_empty.json",
  "http://%s/seasons/2026/athletes/5202598/injuries/-228767?lang=en&region=us" = "injury_-228767.json",
  "http://%s/seasons/2026/athletes/5124105/injuries/-228766?lang=en&region=us" = "injury_-228766.json",
  "http://%s/seasons/2026/athletes/5202598?lang=en&region=us" = "athlete_5202598.json",
  "http://%s/seasons/2026/athletes/5124105?lang=en&region=us" = "athlete_5124105.json"
)
names(injury_fixture_urls) <- sprintf(names(injury_fixture_urls), core)

local_injury_fixtures <- function(drop = character(), env = parent.frame()) {
  testthat::skip_if_not(
    exists("local_mocked_responses", envir = asNamespace("httr2")),
    "httr2 >= 1.1.0 is needed to replay recorded responses"
  )
  dir <- testthat::test_path("fixtures", "espn_mlb_team_injuries")
  httr2::local_mocked_responses(function(req) {
    file <- injury_fixture_urls[req$url]
    if (is.na(file) || file %in% drop) {
      return(httr2::response(status_code = 404L, url = req$url))
    }
    path <- file.path(dir, file)
    httr2::response(
      status_code = 200L, url = req$url,
      headers = list(`Content-Type` = "application/json;charset=UTF-8"),
      body = readBin(path, "raw", file.size(path))
    )
  }, env = env)
}

injury_cols <- c(
  "team_id", "athlete_id", "athlete_name", "position", "status", "date",
  "type", "side", "returns_at", "short_comment", "long_comment",
  "injury_id", "detail"
)

test_that("espn_mlb_team_injuries() resolves each injury and athlete", {
  skip_on_cran()
  local_injury_fixtures()
  x <- espn_mlb_team_injuries(team_id = "13")

  expect_s3_class(x, "baseballr_data")
  expect_identical(colnames(x), injury_cols)
  expect_equal(nrow(x), 2L)
  expect_true(all(vapply(x, is.character, logical(1))))
  expect_identical(x$team_id, c("13", "13"))
  expect_identical(x$athlete_id, c("5202598", "5124105"))
  expect_identical(x$athlete_name, c("Paul Bonzagni", "Anthony Gutierrez"))
  expect_identical(x$position, c("RP", "CF"))
  expect_identical(x$injury_id, c("-228767", "-228766"))
  expect_identical(x$status[[1]], "Out")
  expect_identical(x$type[[1]], "Elbow")
  expect_identical(x$side[[1]], "Right")
  expect_identical(x$detail[[1]], "Inflammation")
  expect_identical(x$returns_at[[1]], "2027-02-01")
  expect_identical(x$date[[1]], "2026-11-03T23:00Z")
})

test_that("espn_mlb_team_injuries() returns the documented columns with no rows for a team with no injuries", {
  skip_on_cran()
  local_injury_fixtures()
  x <- espn_mlb_team_injuries(team_id = "99")

  expect_s3_class(x, "baseballr_data")
  expect_identical(colnames(x), injury_cols)
  expect_equal(nrow(x), 0L)
})

test_that("espn_mlb_team_injuries() keeps the row and names the cause when an athlete lookup fails", {
  skip_on_cran()
  local_injury_fixtures(drop = "athlete_5124105.json")
  expect_message(
    x <- espn_mlb_team_injuries(team_id = "13"),
    "Athlete lookup .*5124105.* failed.*404"
  )
  expect_equal(nrow(x), 2L)
  expect_identical(x$athlete_id[[2]], "5124105")
  expect_true(is.na(x$athlete_name[[2]]))
  expect_identical(x$type[[2]], "Knee")
  expect_identical(x$athlete_name[[1]], "Paul Bonzagni")
})

test_that("espn_mlb_team_injuries() falls back to the $ref ids when an injury lookup fails", {
  skip_on_cran()
  local_injury_fixtures(drop = "injury_-228766.json")
  expect_message(
    x <- espn_mlb_team_injuries(team_id = "13"),
    "Injury lookup .*-228766.* failed.*404"
  )
  expect_equal(nrow(x), 2L)
  expect_identical(x$team_id[[2]], "13")
  expect_identical(x$athlete_id[[2]], "5124105")
  expect_identical(x$injury_id[[2]], "-228766")
  expect_identical(x$athlete_name[[2]], "Anthony Gutierrez")
  expect_true(is.na(x$status[[2]]))
})
