context("Daily Batter logs Baseball-Reference")

cols <- c(
  "bbref_id", "season", "Name", "Age", "Level", "Team", "G", 
  "PA", "AB", "R", "H", "X1B", "X2B", "X3B", "HR",
  "RBI", "BB", "IBB", "uBB", "SO", 
  "HBP", "SH", "SF", "GDP", "SB", "CS", "BA", "OBP", "SLG", "OPS"
)

test_that("Daily Batter logs Baseball-Reference", {
  skip_on_cran()
  
  x <- daily_batter_bref("2015-05-10", "2015-06-20")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
