
cols <- c(
  "jersey_number", "person_id", "person_full_name",
  "person_link", "position_code", "position_name",
  "position_type", "position_abbreviation", "status_code", 
  "status_description", "link", "team_id", "roster_type", "season", "date"
)

test_that("MLB Rosters", {
  skip_on_cran()
  
  x <- mlb_rosters(team_id = 109, season = 2018, roster_type = 'active')
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
