
cols <- c(
  "game_pk", "event_name", "event_date", 
  "event_type_code", "event_type_name", 
  "venue_id", "venue_name", "player_id",
  "player_full_name", "player_link",
  "player_first_name", "player_last_name", 
  "player_primary_number", "player_birth_date",
  "player_current_age", "player_birth_city", 
  "player_birth_state_province", "player_birth_country",
  "player_height", "player_weight", "player_active", 
  "player_use_name", "player_middle_name", "player_boxscore_name",
  "player_nick_name", "player_gender", "player_is_player", 
  "player_is_verified", "player_draft_year", "player_pronunciation",
  "player_mlb_debut_date", "player_name_first_last", 
  "player_name_slug", "player_first_last_name",
  "player_last_first_name", "player_last_init_name", 
  "player_init_last_name", "player_full_fml_name", 
  "player_full_lfm_name", "player_strike_zone_top", 
  "player_strike_zone_bottom", "player_name_matrilineal",
  "player_current_team_id", "player_current_team_name", 
  "player_current_team_link", "player_current_team_season",
  "player_current_team_team_code", "player_current_team_file_code", 
  "player_current_team_abbreviation", "player_current_team_team_name",
  "player_current_team_location_name",
  "player_current_team_first_year_of_play", 
  "player_current_team_short_name", 
  "player_current_team_franchise_name",
  "player_current_team_club_name", 
  "player_current_team_all_star_status",
  "player_current_team_active", 
  "player_current_team_parent_org_name", 
  "player_current_team_parent_org_id",
  "player_current_team_venue_id", 
  "player_current_team_venue_name", 
  "player_current_team_venue_link", 
  "player_current_team_spring_venue_id",
  "player_current_team_spring_venue_link",
  "player_current_team_league_id", 
  "player_current_team_league_name", 
  "player_current_team_league_link", 
  "player_current_team_division_id",
  "player_current_team_division_name",
  "player_current_team_division_link",
  "player_current_team_sport_id",
  "player_current_team_sport_link", 
  "player_current_team_sport_name",
  "player_current_team_spring_league_id",
  "player_current_team_spring_league_name", 
  "player_current_team_spring_league_link",
  "player_current_team_spring_league_abbreviation", 
  "player_primary_position_code", "player_primary_position_name", 
  "player_primary_position_type", "player_primary_position_abbreviation",
  "player_bat_side_code", "player_bat_side_description",
  "player_pitch_hand_code", "player_pitch_hand_description",
  "venue_link", "is_multi_day", 
  "is_primary_calendar", "file_code", "event_number", "public_facing"
)

test_that("MLB Homerun Derby", {
  skip_on_cran()
  
  x <-  mlb_homerun_derby_players(game_pk = 511101)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
