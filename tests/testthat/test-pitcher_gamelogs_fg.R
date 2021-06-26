context("FanGraphs Pitcher Game Logs")

cols <- c(
  "Date", "Team", "Opp", "GS", "W", "L", "SV",
  "HLD", "IP", "TBF", "H", "R", "ER", "HR", "BB",
  "SO", "K_9", "BB_9", "HR_9", "BABIP", 
  "LOB_perc", "GB_perc", "HR_FB", "ERA", "FIP", "xFIP", "GSv2"
)

test_that("FanGraphs Pitcher Game Logs", {
  skip_on_cran()
  
  x <- pitcher_game_logs_fg(playerid = 104, year = 2006)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
