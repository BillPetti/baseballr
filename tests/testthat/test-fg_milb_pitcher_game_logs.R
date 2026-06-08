
cols <- c(
  "Date",
  "Team",
  "Level",
  "Opp",
  "W",
  "L",
  "ERA",
  "G",
  "GS",
  "CG",
  "ShO",
  "SV",
  "IP",
  "TBF",
  "H",
  "R",
  "ER",
  "HR",
  "BB",
  "IBB",
  "HBP",
  "WP",
  "BK",
  "SO",
  "K/9",
  "BB/9",
  "K/BB",
  "HR/9",
  "K%",
  "K-BB%",
  "BB%",
  "AVG",
  "WHIP",
  "BABIP",
  "LOB%",
  "FIP",
  "gamedate",
  "dh"
)

test_that("FanGraphs MiLB Pitcher Game Logs", {
  skip_fangraphs_test()
  skip_on_cran()
  
  x <- fg_milb_pitcher_game_logs(playerid = "sa829043", year = 2021)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No data returned from FanGraphs at test time")
  }

  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
