
cols <- c(
  "year",
  "player_name",
  "position",
  "slug",
  "batting_order",
  "team_name",
  "sub",
  "attendance",
  "game_date",
  "location",
  "player_id",
  "team_id",
  "team_url",
  "conference_id",
  "conference",
  "division",
  "season_id"
)

test_that("NCAA Batting Lineups", {
  skip_on_cran()
  x <- ncaa_lineups(game_info_url="https://stats.ncaa.org/game/index/4587474?org_id=528")
  
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
