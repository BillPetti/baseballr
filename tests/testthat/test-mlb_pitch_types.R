
cols <- c(
  "pitch_type_code", "pitch_type_description"
)

test_that("MLB Pitch Types", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_pitch_types()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
