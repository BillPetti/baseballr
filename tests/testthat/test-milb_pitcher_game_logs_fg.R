context("FanGraphs MiLB Pitcher Game Logs")

cols <- c(
  "name", "minor_playerid", "Date", "Team", "Level", 
  "Opp", "GS", "W", "L", "CG", "ShO", "SV", "IP", 
  "TBF", "H", "R", "ER", "HR", "BB", "IBB", "HBP",
  "WP", "BK", "SO", "K.9", "BB.9", "K.BB", "HR.9", "K_perc", 
  "BB_perc", "K_minus_BB", "AVG", "WHIP", "BABIP",
  "LOB_perc", "ERA", "FIP"
)

test_that("FanGraphs MiLB Pitcher Game Logs", {
  skip_on_cran()
  
  x <- milb_pitcher_game_logs_fg(playerid = "sa829043", year=2021)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
