
cols <- c(
  "home_team_win_probability", "away_team_win_probability",
  "home_team_win_probability_added", "at_bat_index", "leverage_index"
)

test_that("MLB Game Win Probability", {
  skip_on_cran()
  
  x <- mlb_game_wp(game_pk = 531060)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
