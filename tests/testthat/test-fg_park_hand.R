
cols <- c(
  "season", "home_team", "single_as_LHH", "single_as_RHH", 
  "double_as_LHH", "double_as_RHH",
  "triple_as_LHH", "triple_as_RHH", "hr_as_LHH", "hr_as_RHH"
)

test_that("FanGraphs Park Factors", {
  skip_on_cran()
  
  x <- fg_park_hand(2013)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
