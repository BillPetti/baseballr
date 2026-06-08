
cols <- c(
  "stat_name"
)

test_that("MLB Runner Detail Types", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_runner_detail_types()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
