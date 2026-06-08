
cols <- c(
  "team_id",
  "team_name",
  "team_url",
  "conference_id",
  "conference",
  "division",
  "year",
  "season_id"
)

test_that("NCAA Teams", {
  skip_ncaa_test()
  skip_on_cran()
  
  x <- ncaa_teams()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
