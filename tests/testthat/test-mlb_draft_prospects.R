
cols <- c(
  "bis_player_id", "pick_round", "pick_number", "rank", 
  "scouting_report", "blurb", "headshot_link", "is_drafted",
  "year", "home_city", "home_state", "home_country",
  "school_name", "school_school_class", "school_country",
  "school_state", "person_id", "person_full_name", "person_link",
  "person_first_name", "person_last_name", "person_birth_date",
  "person_current_age", "person_birth_city", "person_birth_state_province",
  "person_birth_country", "person_height", "person_weight", 
  "person_active", "person_use_name", "person_middle_name", 
  "person_boxscore_name", "person_gender", "person_is_player", 
  "person_is_verified", "person_draft_year",
  "person_name_first_last", "person_name_slug", 
  "person_first_last_name", "person_last_first_name", 
  "person_last_init_name", "person_init_last_name", 
  "person_full_fml_name", "person_full_lfm_name",
  "person_strike_zone_top", "person_strike_zone_bottom",
  "person_primary_number", "person_pronunciation",
  "person_name_title", "person_mlb_debut_date", 
  "person_name_matrilineal", "person_nick_name", 
  "person_death_date", "person_death_city", 
  "person_death_state_province", "person_death_country",
  "person_primary_position_code", "person_primary_position_name", 
  "person_primary_position_type", "person_primary_position_abbreviation",
  "person_bat_side_code", "person_bat_side_description",
  "person_pitch_hand_code", "person_pitch_hand_description", 
  "team_id", "team_name", "team_link", "team_season",
  "team_team_code", "team_file_code", "team_abbreviation",
  "team_team_name", "team_location_name", 
  "team_first_year_of_play", "team_short_name",
  "team_franchise_name", "team_club_name", 
  "team_all_star_status", "team_active", "team_venue_id", 
  "team_venue_name", "team_venue_link", "team_spring_venue_id", 
  "team_spring_venue_link", "team_league_id", "team_league_name",
  "team_league_link", "team_division_id", "team_division_name",
  "team_division_link", "team_sport_id", "team_sport_link", 
  "team_sport_name", "team_spring_league_id", "team_spring_league_name",
  "team_spring_league_link", "team_spring_league_abbreviation",
  "draft_type_code", "draft_type_description"
)

test_that("MLB Draft Prospects", {
  skip_on_cran()
  
  x <- mlb_draft_prospects(year = 2020)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
