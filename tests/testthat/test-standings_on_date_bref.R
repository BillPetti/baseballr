context("BRef Standings on Date")

cols <- c(
  "Tm", "W", "L", "W-L%", "GB", "RS", "RA", "pythW-L%"
)

test_that("BRef Standings on Date", {
  skip_on_cran()
  
  x <- standings_on_date_bref(date = "2015-08-04", division = "AL East")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
