context("Team Consistency")

cols <- c(
  "Team", "Con_R", "Con_RA", "Con_R_Ptile", "Con_RA_Ptile"
)

test_that("Team Consistency", {
  skip_on_cran()
  
  x <- team_consistency(year=2015)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
