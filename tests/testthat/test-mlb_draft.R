context("MLB Draft")

cols <- c(
  "bis_player_id", "pick_round", "pick_number", 
  "round_pick_number", "rank", "pick_value", 
  "signing_bonus", "scouting_report", "blurb",
  "headshot_link", "is_drafted", "is_pass", "year",
  "home_city", "home_state", "home_country", "school_name",
  "school_country", "school_state", "school_city", "person_id", 
  "person_full_name", "person_link", "person_first_name", 
  "person_last_name", "person_primary_number", "person_birth_date",
  "person_current_age", "person_birth_city", 
  "person_birth_state_province",
  "person_birth_country", "person_height", "person_weight",
  "person_active", "person_use_name", "person_middle_name",
  "person_boxscore_name", "person_gender", "person_is_player",
  "person_is_verified", "person_draft_year", "person_name_first_last",
  "person_name_slug", "person_first_last_name",
  "person_last_first_name", "person_last_init_name",
  "person_init_last_name", "person_full_fml_name",
  "person_full_lfm_name", "person_strike_zone_top", 
  "person_strike_zone_bottom", "person_name_title",
  "person_mlb_debut_date", "person_pronunciation", 
  "person_name_matrilineal", "person_nick_name",
  "person_primary_position_code", "person_primary_position_name",
  "person_primary_position_type", "person_primary_position_abbreviation",
  "person_bat_side_code", "person_bat_side_description", 
  "person_pitch_hand_code", "person_pitch_hand_description",
  "team_id", "team_name", "team_link", "team_all_star_status",
  "team_spring_league_id", "team_spring_league_name", 
  "team_spring_league_link",
  "team_spring_league_abbreviation",
  "draft_type_code", "draft_type_description"
)

test_that("MLB Draft", {
  skip_on_ci()
  skip_on_cran()

  x <- mlb_draft(2019)

  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
