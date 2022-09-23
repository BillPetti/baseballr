
cols <- c(
  "stat_name", "code", "names", "error", "chance"
)

test_that("MLB Fielder Detail Types", {
  skip_on_cran()
  
  x <- mlb_fielder_detail_types()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
