
cols <- c(
  "review_reason_code", "review_reason_description"
)

test_that("MLB Review Reasons", {
  skip_on_cran()
  
  x <- mlb_review_reasons()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
