context("NCAA Get Roster")

cols <- c(
  "name", "class", "player_id", "season", "number",
  "position", "player_url", "school",
  "conference", "school_id", "division", "conference_id"
)

test_that("NCAA Get Roster", {
  skip_on_cran()
  x <- get_ncaa_baseball_roster(teamid = 104, team_year = 2019)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
