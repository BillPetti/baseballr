
cols <- c(
  "year",
  "season_id",
  "date",
  "home_team",
  "home_team_id",
  "home_team_score",
  "home_team_conference",
  "home_team_conference_id",
  "home_team_slug",
  "home_team_division",
  "away_team",
  "away_team_id",
  "away_team_score",
  "away_team_conference",
  "away_team_conference_id",
  "away_team_slug",
  "away_team_division",
  "neutral_site",
  "innings",
  "slug",
  "game_info_url",
  "game_pbp_url",
  "contest_id",
  "game_pbp_id"
)

test_that("NCAA Get Schedule Info", {
  skip_on_cran()
  
  x <- ncaa_schedule_info(736, 2019)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
