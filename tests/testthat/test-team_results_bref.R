context("BRef Team Results")

cols <- c(
  "Gm", "Date", "Tm", "H_A", "Opp", "Result",
  "R", "RA", "Inn", "Record", "Rank", "GB",
  "Win", "Loss", "Save", 
  "Time", "D/N", "Attendance", "cLI", "Streak", "Orig_Scheduled", "Year"
)

test_that("BRef Team Results", {
  skip_on_cran()
  
  x <- team_results_bref(Tm="TBR", year=2008)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
