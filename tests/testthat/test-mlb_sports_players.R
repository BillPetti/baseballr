
cols <- c(
  "player_id",
  "full_name",
  "link",
  "first_name",
  "last_name",
  "primary_number",
  "birth_date",
  "current_age",
  "birth_city",
  "birth_country",
  "height",
  "weight",
  "active",
  "use_name",
  "middle_name",
  "boxscore_name",
  "nick_name",
  "gender",
  "is_player",
  "is_verified",
  "pronunciation",
  "mlb_debut_date",
  "name_first_last",
  "name_slug",
  "first_last_name",
  "last_first_name",
  "last_init_name",
  "init_last_name",
  "full_fml_name",
  "full_lfm_name",
  "strike_zone_top",
  "strike_zone_bottom",
  "birth_state_province",
  "draft_year",
  "name_matrilineal",
  "name_title",
  "last_played_date",
  "current_team_id",
  "current_team_name",
  "current_team_link",
  "primary_position_code",
  "primary_position_name",
  "primary_position_type",
  "primary_position_abbreviation",
  "bat_side_code",
  "bat_side_description",
  "pitch_hand_code",
  "pitch_hand_description"
)

test_that("MLB Sports Players", {
  skip_on_cran()
  
  x <- mlb_sports_players(sport_id=1,season=2021)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
