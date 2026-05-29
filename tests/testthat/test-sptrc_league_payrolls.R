
# Subset-direction assertion (expected columns must be a subset of the actual
# columns) so Spotrac schema additions do not break the test. Regression guard
# for #392 (new /_/year/ URL and changed league-payroll table schema).
cols <- c(
  "year",
  "team",
  "team_abbr",
  "rank",
  "total_payroll_allocations"
)

test_that("Spotrac League Payrolls Breakdown", {
  skip_on_cran()
  skip_on_ci()

  x <- sptrc_league_payrolls(year = most_recent_mlb_season())

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from Spotrac at test time")
  }

  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
