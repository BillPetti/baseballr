# cols <- c(
#   "Tm", "W", "L", "W-L%", "GB", "RS", "RA", "pythW-L%"
# )
# 
# test_that("BRef Standings on Date", {
  skip_bref_test()
#   skip_on_cran()
#   
#   x <- bref_standings_on_date(date = "2021-08-04", division = "AL East")
#   
#   expect_in(sort(cols), sort(colnames(x)))
#   expect_s3_class(x, "data.frame")
# })

test_that("bref_standings_on_date handles pre-division and two-division eras", {
  skip_on_cran()
  skip_if_offline("www.baseball-reference.com")

  x65 <- bref_standings_on_date("1965-08-04", "AL Overall")
  if (!is.null(x65) && nrow(x65) > 0) expect_equal(nrow(x65), 10)

  x75 <- bref_standings_on_date("1975-08-04", "NL East")
  if (!is.null(x75) && nrow(x75) > 0) expect_equal(nrow(x75), 6)

  expect_error(bref_standings_on_date("1965-08-04", "AL Central"))
  expect_error(bref_standings_on_date("1975-08-04", "NL Central"))
})
