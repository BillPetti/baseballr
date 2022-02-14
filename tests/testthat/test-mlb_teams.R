
cols <- c(
  "team_id", "team_full_name", "link",
  "season", "team_code", "file_code",
  "team_name", "location_name",
  "first_year_of_play", "short_name",
  "all_star_status", "active", 
  "team_abbreviation", "parent_org_name", 
  "parent_org_id", "franchise_name", 
  "club_name", "venue_id", "venue_name",
  "venue_link", "league_id", "league_name",
  "league_link", "sport_id", "sport_link",
  "sport_name", "division_id", "division_name",
  "division_link", "spring_venue_id", "spring_venue_link",
  "spring_league_id", "spring_league_name", 
  "spring_league_link", "spring_league_abbreviation"
)

test_that("MLB Teams", {
  skip_on_cran()
  
  x <- mlb_teams()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
