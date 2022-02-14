
cols <- c(
  "wind_direction_code", "wind_direction_description"
)

test_that("MLB Wind Direction Codes", {
  skip_on_cran()
  
  x <- mlb_wind_direction_codes()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
