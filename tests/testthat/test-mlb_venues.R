
cols <- c(
  "venue_id", "venue_name", "venue_link", "active"
)

test_that("MLB Venues", {
  skip_on_cran()
  
  x <- mlb_venues()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
