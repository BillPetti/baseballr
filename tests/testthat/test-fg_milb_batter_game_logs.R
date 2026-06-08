
cols <- c(
  "Date",
  "Team",
  "Level",
  "Opp",
  "G",
  "AB",
  "PA",
  "H",
  "1B",
  "2B",
  "3B",
  "HR",
  "R",
  "RBI",
  "BB",
  "IBB",
  "SO",
  "HBP",
  "SF",
  "SH",
  "GDP",
  "SB",
  "CS",
  "AVG",
  "BB%",
  "K%",
  "BB/K",
  "OBP",
  "SLG",
  "OPS",
  "ISO",
  "Spd",
  "BABIP",
  "wRC",
  "wRAA",
  "wOBA",
  "wRC+",
  "wBsR",
  "gamedate",
  "dh"
)

test_that("FanGraphs MiLB Batter Game Logs", {
  skip_fangraphs_test()
  skip_on_cran()
  
  x <- fg_milb_batter_game_logs(playerid = "sa3010868", year=2021)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No data returned from FanGraphs at test time")
  }

  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
