
cols <- c(
  "bis_player_id", "pick_round", "pick_number", 
  "round_pick_number", "rank", "pick_value", 
  "signing_bonus", "home_city", "home_state", 
  "home_country", "scouting_report", "school_name",
  "school_school_class", "school_country", "school_state",
  "blurb", "headshot_link", "person_id", "person_full_name",
  "person_link", "person_first_name", "person_last_name",
  "person_primary_number", "person_birth_date", "person_current_age",
  "person_birth_city", "person_birth_state_province",
  "person_birth_country", "person_height", "person_weight",
  "person_active", "person_primary_position_code", 
  "person_primary_position_name", "person_primary_position_type",
  "person_primary_position_abbreviation", "person_use_name", 
  "person_middle_name", "person_boxscore_name", "person_gender",
  "person_is_player", "person_is_verified", "person_draft_year",
  "person_bat_side_code", "person_bat_side_description",
  "person_pitch_hand_code", "person_pitch_hand_description", 
  "person_name_first_last", "person_name_slug",
  "person_first_last_name", "person_last_first_name",
  "person_last_init_name", "person_init_last_name",
  "person_full_fml_name", "person_full_lfm_name",
  "person_strike_zone_top", "person_strike_zone_bottom",
  "team_id", "team_name", "team_link", "team_season", 
  "team_venue_id", "team_venue_name", "team_venue_link",
  "team_spring_venue_id", "team_spring_venue_link",
  "team_team_code", "team_file_code", "team_abbreviation",
  "team_team_name", "team_location_name", 
  "team_first_year_of_play", "team_league_id", 
  "team_league_name", "team_league_link", 
  "team_division_id", "team_division_name",
  "team_division_link", "team_sport_id",
  "team_sport_link", "team_sport_name", 
  "team_short_name", "team_franchise_name", 
  "team_club_name", "team_spring_league_id", 
  "team_spring_league_name", "team_spring_league_link", 
  "team_spring_league_abbreviation", "team_all_star_status",
  "team_active", "draft_type_code", 
  "draft_type_description", "is_drafted", "is_pass", "year"
)

test_that("MLB Draft Latest", {
  skip_on_cran()
  
  x <- mlb_draft_latest(year = 2020) %>% 
    dplyr::select(tidyr::all_of(cols))
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
