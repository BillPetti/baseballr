context("Fangraphs Batter Game Logs")

cols <- c(
  "Date", "Team", "Opp", "BO", "Pos", "PA", 
  "H", "X2B", "X3B", "HR", "R", "RBI", "SB", "CS", "BB_perc", "K_perc", 
  "ISO", "BABIP", "AVG", "OBP", "SLG", "wOBA", "wRC_plus"
)

test_that("Fangraphs Batter Game Logs", {
  skip_on_cran()
  x <- batter_game_logs_fg(playerid = 6184, year = 2017)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
