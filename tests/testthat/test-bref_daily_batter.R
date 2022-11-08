# cols <- c(
#   "bbref_id", "season", "Name", "Age", "Level", "Team", "G", 
#   "PA", "AB", "R", "H", "X1B", "X2B", "X3B", "HR",
#   "RBI", "BB", "IBB", "uBB", "SO", 
#   "HBP", "SH", "SF", "GDP", "SB", "CS", "BA", "OBP", "SLG", "OPS"
# )
# 
# test_that("Daily Batter logs Baseball-Reference", {
#   skip_on_cran()
#   skip_on_ci()
#   x <- bref_daily_batter("2021-05-10", "2021-06-20")
#   
#   expect_equal(colnames(x), cols)
#   expect_s3_class(x, "data.frame")
# })
