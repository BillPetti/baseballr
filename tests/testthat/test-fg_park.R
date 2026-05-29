
cols <- c(
  "season", "home_team", "basic_5yr", "3yr",
  "1yr", "single", "double", "triple",
  "hr", "so", "UIBB", "GB", "FB", "LD", "IFFB", "FIP"
)

test_that("FanGraphs Park Factors", {
  skip_fangraphs_test()
  skip_on_cran()
  
  x <- fg_park(2013)
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
