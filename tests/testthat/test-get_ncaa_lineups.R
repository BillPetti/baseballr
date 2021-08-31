context("NCAA Get Batting Lineups")

cols <- c("year", "playerName", "position", "batting_order", "school")

test_that("NCAA Get Batting Lineups", {
  skip_on_cran()
  x <- get_ncaa_lineups(game_info_url="https://stats.ncaa.org/game/index/4587474?org_id=528",year=2018)
  
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
