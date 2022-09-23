
cols <- c(
  "league_id", "league_name", "league_link", 
  "league_abbreviation", "league_name_short",
  "league_season_state", "league_has_wild_card",
  "league_has_split_season", "league_num_games",
  "league_has_playoff_points", "league_num_teams",
  "league_num_wildcard_teams", "league_season", "league_org_code",
  "league_conferences_in_use", "league_divisions_in_use",
  "league_sort_order", "league_active", "season_date_info_season_id",
  "season_date_info_pre_season_start_date",
  "season_date_info_pre_season_end_date",
  "season_date_info_season_start_date",
  "season_date_info_spring_start_date", 
  "season_date_info_spring_end_date", 
  "season_date_info_regular_season_start_date",
  "season_date_info_last_date1st_half",
  "season_date_info_all_star_date", 
  "season_date_info_first_date2nd_half", 
  "season_date_info_regular_season_end_date",
  "season_date_info_post_season_start_date",
  "season_date_info_post_season_end_date",
  "season_date_info_season_end_date", 
  "season_date_info_offseason_start_date", 
  "season_date_info_off_season_end_date",
  "season_date_info_season_level_gameday_type",
  "season_date_info_game_level_gameday_type", 
  "season_date_info_qualifier_plate_appearances",
  "season_date_info_qualifier_outs_pitched", 
  "sport_id", "sport_link"
)

test_that("MLB League", {
  skip_on_cran()
  
  x <-  mlb_league(seasons = 2021, sport_id = 1)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
