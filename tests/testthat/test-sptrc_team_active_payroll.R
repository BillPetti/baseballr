
cols <- c(
  "year",
  "team",
  "player_name",
  "roster_status",
  "age",
  "pos",
  "status",
  "base_salary",
  "signing_bonus",
  "incentives",
  "payroll_salary",
  "adj_salary",
  "payroll_percent",
  "lux_tax_salary",
  "total_salary",
  "waiver_options"
)

test_that("Spotrac League Payrolls Breakdown", {
  skip_on_cran()
  
  x <- sptrc_team_active_payroll(team_abbr = "BAL", year = most_recent_mlb_season())
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
