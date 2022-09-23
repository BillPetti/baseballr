
cols <- c(
  "stat_type_name"
)

test_that("MLB Stat Types", {
  skip_on_cran()
  
  x <- mlb_stat_types()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
