
cols <- c(
  "job_code", "job", "sort_order"
)

test_that("MLB Job Types", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_job_types()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
