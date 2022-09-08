
cols <- c(
  "year",
  "team",
  "team_abbr",
  "rank",
  "win_percent",
  "roster",
  "active_man_payroll",
  "injured_reserve",
  "retained",
  "buried",
  "suspended",
  "yearly_total_payroll"
)

test_that("Spotrac League Payrolls Breakdown", {
  skip_on_cran()
  
  x <- sptrc_league_payrolls(year = most_recent_mlb_season())
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
