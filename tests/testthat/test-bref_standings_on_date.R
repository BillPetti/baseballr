# cols <- c(
#   "Tm", "W", "L", "W-L%", "GB", "RS", "RA", "pythW-L%"
# )
# 
# test_that("BRef Standings on Date", {
#   skip_on_cran()
#   skip_on_ci()
#   
#   x <- bref_standings_on_date(date = "2021-08-04", division = "AL East")
#   
#   expect_equal(colnames(x), cols)
#   expect_s3_class(x, "data.frame")
# })
