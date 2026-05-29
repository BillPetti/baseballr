
cols <- c(
  "venue_id", "venue_name", "venue_link", "active", "season"
)

test_that("MLB Venues", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_venues()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
