
cols <- c(
  "pitch_code", "pitch_description"
)

test_that("MLB Pitch Codes", {
  skip_on_cran()
  
  x <- mlb_pitch_codes()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
