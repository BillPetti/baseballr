
cols <- c(
  "job_code", "job", "sort_order"
)

test_that("MLB Job Types", {
  skip_on_cran()
  
  x <- mlb_job_types()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
