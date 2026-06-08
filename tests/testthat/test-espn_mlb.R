# Live ESPN MLB tests. Gated behind ESPN_MLB_TESTS=1 (see helper-skip.R) and
# skip_on_cran() / skip_on_ci() so routine checks never hit ESPN. Column
# assertions use the subset direction (expected names must be a subset of the
# actual names) so new upstream columns do not break the tests.

espn_mlb_test_game <- "401569780"   # 2024-04-01 NYM @ WSH (final)
espn_mlb_test_team <- "10"          # New York Yankees

test_that("espn_mlb_scoreboard returns scheduled/played games", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_scoreboard(season = "20240704")
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No scoreboard rows at test time")
  cols <- c("game_id", "season", "home_team_abb", "away_team_abb", "home_score", "away_score")
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})

test_that("espn_mlb_teams returns the 30 MLB teams", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_teams()
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No team rows at test time")
  cols <- c("team_id", "abbreviation", "display_name")
  expect_in(sort(cols), sort(colnames(x)))
  expect_gte(nrow(x), 25)
})

test_that("espn_mlb_standings returns standings entries", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_standings(year = 2024)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No standings rows at test time")
  expect_s3_class(x, "data.frame")
})

test_that("espn_mlb_pbp returns play-by-play rows", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_pbp(game_id = espn_mlb_test_game)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No pbp rows at test time")
  cols <- c("id", "text", "at_bat_id", "period_number", "game_id")
  expect_in(sort(cols), sort(colnames(x)))
})

test_that("espn_mlb_team_box returns one wide row per team", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_team_box(game_id = espn_mlb_test_game)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No team box rows at test time")
  cols <- c("game_id", "team_id", "team_abbreviation", "batting_runs", "pitching_strikeouts")
  expect_in(sort(cols), sort(colnames(x)))
  expect_lte(nrow(x), 2)
})

test_that("espn_mlb_player_box returns batting/pitching rows", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_player_box(game_id = espn_mlb_test_game)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No player box rows at test time")
  cols <- c("game_id", "stat_group", "athlete_id", "athlete_display_name", "team_abbreviation")
  expect_in(sort(cols), sort(colnames(x)))
  expect_true(all(x$stat_group %in% c("batting", "pitching")))
})

test_that("espn_mlb_game_all bundles plays + team + player box", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_game_all(game_id = espn_mlb_test_game)
  if (is.null(x) || length(x) == 0) skip("No game data at test time")
  expect_in(c("Plays", "Team", "Player"), names(x))
})

test_that("espn_mlb_team_roster returns a team's roster", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_team_roster(team_id = espn_mlb_test_team)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No roster rows at test time")
  expect_s3_class(x, "data.frame")
})

test_that("espn_mlb_leaders returns league leaders", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_leaders(season = 2024)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No leader rows at test time")
  expect_s3_class(x, "data.frame")
})

test_that("espn_mlb_game_probables returns probable starters", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_game_probables(game_id = espn_mlb_test_game)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No probables at test time")
  cols <- c("game_id", "team_id", "home_away", "athlete_id", "athlete_display_name", "throws")
  expect_in(sort(cols), sort(colnames(x)))
  expect_lte(nrow(x), 2)
})

test_that("espn_mlb_game_info returns venue + umpire crew", {
  skip_on_cran(); skip_on_ci(); skip_espn_test()
  x <- espn_mlb_game_info(game_id = espn_mlb_test_game)
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No game info at test time")
  cols <- c("game_id", "venue_name", "attendance", "home_plate_umpire")
  expect_in(sort(cols), sort(colnames(x)))
  expect_equal(nrow(x), 1)
})
