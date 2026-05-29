
cols <- c(
  "stat_type_name"
)

test_that("MLB Stat Types", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_stat_types()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
