
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
  skip_on_cran()
  skip_on_ci()
  
  x <- ncaa_teams()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
