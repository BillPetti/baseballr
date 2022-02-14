
cols <- c(
  "total_splits", "season", "date", "is_home",
  "rank", "game_innings", "stat_at_bats", "team_id", 
  "team_name", "team_link", "opponent_id", "opponent_name",
  "opponent_link", "game_pk", "game_link", "game_number",
  "game_content_link", "home_team_id", "home_team_name",
  "home_team_link", "away_team_id", "away_team_name",
  "away_team_link", "combined_stats", "group_display_name", 
  "game_type_id", "game_type_description", "sort_stat_name", 
  "sort_stat_lookup_param", "sort_stat_is_counting", "sort_stat_label"
)

test_that("MLB Stat Highs/Lows", {
  skip_on_cran()
  
  x <- mlb_high_low_stats(org_type = 'Team', season = 2020, sort_stat = 'atBats')
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
