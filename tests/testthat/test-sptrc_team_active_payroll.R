
# Columns guaranteed by the wrapper (year/team/player_name/roster_status) plus
# a few stable Spotrac salary columns. Asserted subset-direction so Spotrac
# schema additions do not break the test. Regression guard for #392 (Spotrac
# moved its payroll URLs behind /_/year/ and changed the table schema).
cols <- c(
  "year",
  "team",
  "player_name",
  "roster_status",
  "payroll_salary",
  "base_salary",
  "signing_bonus"
)

test_that("Spotrac Team Active Payroll Breakdown", {
  skip_sptrc_test()
  skip_on_cran()
  skip_on_ci()

  x <- sptrc_team_active_payroll(team_abbr = "BAL", year = most_recent_mlb_season())

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from Spotrac at test time")
  }

  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
