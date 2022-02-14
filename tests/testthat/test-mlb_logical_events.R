
cols <- c(
  "event_code"
)

test_that("MLB Logical Events", {
  skip_on_cran()
  
  x <- mlb_logical_events()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
