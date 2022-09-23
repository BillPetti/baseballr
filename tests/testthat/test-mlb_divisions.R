
cols <- c(
  "division_id", "division_name", "season", 
  "division_name_short", "division_link", 
  "division_abbreviation", "has_wildcard",
  "sort_order", "num_playoff_teams", "active",
  "league_id", "league_link", "sport_id", "sport_link"
)

test_that("MLB Divisions", {
  skip_on_cran()
  
  x <- mlb_divisions(sport_id = 1)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
