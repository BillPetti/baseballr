
cols <- c(
  "season", "home_team", "single_as_LHH", "single_as_RHH", 
  "double_as_LHH", "double_as_RHH",
  "triple_as_LHH", "triple_as_RHH", "hr_as_LHH", "hr_as_RHH"
)

test_that("FanGraphs Park Factors", {
  skip_fangraphs_test()
  skip_on_cran()
  
  x <- fg_park_hand(2013)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No data returned from FanGraphs at test time")
  }

  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
