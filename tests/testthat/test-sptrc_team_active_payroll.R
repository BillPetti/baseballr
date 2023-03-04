
cols <- c(
  "year",
  "team",
  "player_name",
  "roster_status",
  "age",
  "pos",
  "status",
  "waiver_options",
  "base_salary",
  "signing_bonus",
  "payroll_salary",
  "adj_salary",
  "payroll_percent",
  "lux_tax_salary",
  "total_salary"
)

test_that("Spotrac League Payrolls Breakdown", {
  skip_on_cran()
  
  x <- sptrc_team_active_payroll(team_abbr = "BAL", year = most_recent_mlb_season())
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
