
cols <- c(
  "bbref_id", "season", "Name", "Age", "Level",
  "Team", "G", "GS", "W", "L", "SV", "IP", "H",
  "R", "ER", "uBB", "BB", "SO", "HR", "HBP",
  "ERA", "AB", "X1B", "X2B", "X3B", "IBB", "GDP", 
  "SF", "SB", "CS", "PO", "BF", "Pit", "Str", "StL", 
  "StS", "GB.FB", "LD", "PU", "WHIP", "BAbip", "SO9", 
  "SO.W", "SO_perc", "uBB_perc", "SO_uBB"
)

test_that("Daily Pitcher logs Baseball-Reference", {
  skip_on_cran()
  
  x <- bref_daily_pitcher("2021-05-10", "2021-06-20")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
