context("FanGraphs MiLB Batter Game Logs")

cols <- c(
  "name", "minor_playerid", "Season", "Team", 
  "Level", "G", "AB", "PA", "H", "X1B", "X2B", 
  "X3B", "HR", "R", "RBI", "BB", "IBB", "SO",
  "HBP", "SF", "SH", "GDP", "SB", "CS", "AVG", 
  "BB_perc", "K_perc", "BB_minus_K", "OBP", 
  "SLG", "OPS", "ISO", "Spd", 
  "BABIP", "UBR", "wGDP", "wSB", "wRC", "wRAA", "wOBA", "wRC_plus"
)

test_that("FanGraphs MiLB Batter Game Logs", {
  skip_on_cran()
  
  x <- milb_batter_game_logs_fg(playerid = "sa917940", year=2018)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
