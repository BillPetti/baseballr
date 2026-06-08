
cols <- c(
  "player_name",
  "class",
  "player_id",
  "season",
  "number",
  "position",
  "player_url",
  "team_id",
  "team_name",
  "team_url",
  "conference_id",
  "conference",
  "division",
  "year",
  "season_id"
)

test_that("NCAA Roster", {
  skip_ncaa_test()
  skip_on_cran()
  x <- ncaa_roster(team_id = 104, year = 2019)
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
