
cols <- c(
  "sky_code", "sky_description"
)

test_that("MLB Sky Types", {
  skip_mlb_test()
  skip_on_cran()
  
  x <-  mlb_sky()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
