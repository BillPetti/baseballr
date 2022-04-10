
cols <- c(
  "language_code", "language_name", "locale"
)

test_that("MLB Languages", {
  skip_on_cran()
  
  x <- mlb_languages()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
