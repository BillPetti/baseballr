
cols <- c(
  "language_id", "language_code", "language_name", "locale"
)

test_that("MLB Languages", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_languages()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
