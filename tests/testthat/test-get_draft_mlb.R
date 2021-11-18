# context("MLB Draft")
# 
# cols <- c(
#   "bis_player_id", "pick_round", "pick_number", 
#   "headshot_link", "is_drafted", "is_pass", "school_name",
#   "person_id", "person_full_name", "person_link", 
#   "person_first_name", "person_last_name", 
#   "person_primary_number", "person_birth_date",
#   "person_current_age", "person_birth_city",
#   "person_birth_state_province", "person_birth_country", 
#   "person_height", "person_weight", "person_active", 
#   "person_use_name", "person_middle_name", "person_boxscore_name", 
#   "person_draft_year", "person_last_played_date",
#   "person_mlb_debut_date", "person_name_first_last",
#   "person_name_slug", "person_first_last_name", 
#   "person_last_first_name", "person_last_init_name",
#   "person_init_last_name", "person_full_fml_name", 
#   "person_full_lfm_name", "person_strike_zone_top",
#   "person_strike_zone_bottom", "person_nick_name",
#   "person_death_date", "person_death_city", 
#   "person_death_state_province",
#   "person_death_country", "person_pronunciation",
#   "person_primary_position_code", "person_primary_position_name", 
#   "person_primary_position_type", "person_primary_position_abbreviation", 
#   "person_bat_side_code", "person_bat_side_description", 
#   "person_pitch_hand_code", "person_pitch_hand_description",
#   "team_id", "team_name", "team_link", "team_all_star_status", 
#   "team_spring_league_id", "team_spring_league_name",
#   "team_spring_league_link", "team_spring_league_abbreviation", 
#   "person_name_title", "school_school_class", "rank",
#   "pick_value", "signing_bonus", "scouting_report",
#   "blurb", "home_city", "home_state", "home_country",
#   "school_city", "school_country", "school_state", 
#   "round_pick_number", "person_gender", "person_is_player",
#   "person_is_verified", "person_name_matrilineal"
# )
# 
# test_that("MLB Draft", {
#   skip_on_ci()
#   skip_on_cran()
#   
#   x <- get_draft_mlb(2009)
#   
#   expect_equal(colnames(x), cols)
#   expect_s3_class(x, "data.frame")
# })
