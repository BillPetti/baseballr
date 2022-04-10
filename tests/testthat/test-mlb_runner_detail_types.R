
cols <- c(
  "stat_name"
)

test_that("MLB Runner Detail Types", {
  skip_on_cran()
  
  x <- mlb_runner_detail_types()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
