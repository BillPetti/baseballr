
cols <- c(
  "sky_code", "sky_description"
)

test_that("MLB Sky Types", {
  skip_on_cran()
  
  x <-  mlb_sky()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
