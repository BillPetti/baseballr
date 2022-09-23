
cols <- c(
  "award_id", "award_name", "date", "season", "votes", 
  "notes", "player_id", "player_link", "player_name_first_last",
  "player_primary_position_code", "player_primary_position_name",
  "player_primary_position_type", 
  "player_primary_position_abbreviation", "team_id", "team_link"
)

test_that("MLB Awards recipients", {
  skip_on_cran()
  
  x <- mlb_awards_recipient(award_id = 'MLBHOF', season = 2020)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
