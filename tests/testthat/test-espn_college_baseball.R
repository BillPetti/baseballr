# Live ESPN College Baseball tests. Gated behind ESPN_CBASE_TESTS=1 (see
# helper-skip.R) and skip_on_cran() / skip_on_ci() so routine checks never hit
# ESPN. Column assertions use the subset direction (expected names must be a
# subset of the actual names) so new upstream columns do not break the tests.
# IDs are from the 2025 season (College World Series era).

cbase_game <- "401778093"   # 2025 CWS: Coastal Carolina @ Oregon State
cbase_team <- "59"          # Arizona State Sun Devils

test_that("espn_college_baseball_scoreboard returns games", {
  skip_on_cran(); skip_on_ci(); skip_espn_college_baseball_test()
  x <- espn_college_baseball_scoreboard(season = "20250615")
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No scoreboard rows at test time")
  expect_in("game_id", colnames(x))
  expect_s3_class(x, "data.frame")
})

test_that("espn_college_baseball_teams returns the D1 teams", {
  skip_on_cran(); skip_on_ci(); skip_espn_college_baseball_test()
  x <- espn_college_baseball_teams()
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No team rows at test time")
  expect_in(sort(c("team_id", "abbreviation", "display_name")), sort(colnames(x)))
  expect_gte(nrow(x), 100)
})

test_that("espn_college_baseball_standings returns standings entries", {
  skip_on_cran(); skip_on_ci(); skip_espn_college_baseball_test()
  x <- espn_college_baseball_standings(year = 2025)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No standings rows at test time")
  expect_s3_class(x, "data.frame")
})

test_that("espn_college_baseball_pbp returns play-by-play rows", {
  skip_on_cran(); skip_on_ci(); skip_espn_college_baseball_test()
  x <- espn_college_baseball_pbp(game_id = cbase_game)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No pbp rows at test time")
  expect_s3_class(x, "data.frame")
})

test_that("espn_college_baseball_player_box returns one wide row per player", {
  skip_on_cran(); skip_on_ci(); skip_espn_college_baseball_test()
  x <- espn_college_baseball_player_box(game_id = cbase_game)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No player box rows at test time")
  expect_s3_class(x, "data.frame")
})

test_that("espn_college_baseball_team returns the team detail list", {
  skip_on_cran(); skip_on_ci(); skip_espn_college_baseball_test()
  x <- espn_college_baseball_team(team_id = cbase_team, season = 2025)
  if (length(x) == 0 || is.null(x[["Info"]]) || nrow(x[["Info"]]) == 0) {
    skip("No team detail at test time")
  }
  expect_type(x, "list")
  expect_in("Info", names(x))
})

test_that("espn_college_baseball_team_roster returns athletes", {
  skip_on_cran(); skip_on_ci(); skip_espn_college_baseball_test()
  x <- espn_college_baseball_team_roster(team_id = cbase_team, season = 2025)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No roster rows at test time")
  expect_in(sort(c("athlete_id", "full_name", "team_id")), sort(colnames(x)))
})

test_that("espn_college_baseball_team_schedule returns events", {
  skip_on_cran(); skip_on_ci(); skip_espn_college_baseball_test()
  x <- espn_college_baseball_team_schedule(team_id = cbase_team, season = 2025)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No schedule rows at test time")
  expect_in("event_id", colnames(x))
})

test_that("espn_college_baseball_venues returns venue rows", {
  skip_on_cran(); skip_on_ci(); skip_espn_college_baseball_test()
  x <- espn_college_baseball_venues()
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No venue rows at test time")
  expect_s3_class(x, "data.frame")
})

test_that("espn_college_baseball_news returns articles", {
  skip_on_cran(); skip_on_ci(); skip_espn_college_baseball_test()
  x <- espn_college_baseball_news()
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No news rows at test time")
  expect_s3_class(x, "data.frame")
})

test_that("invalid league is rejected by the shared validator", {
  expect_error(
    baseballr:::.espn_baseball_validate_league("nfl"),
    "league must be one of"
  )
  expect_silent(baseballr:::.espn_baseball_validate_league("college-baseball"))
})
