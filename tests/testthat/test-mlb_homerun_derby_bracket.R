
cols <- c(
  "game_pk", "event_name", "event_type_code",
  "event_type_name", "event_date", "venue_id",
  "venue_name", "venue_link", "is_multi_day",
  "is_primary_calendar", "file_code", "event_number",
  "public_facing", "round", "num_batters",
  "top_seed_complete", "top_seed_started", 
  "top_seed_winner", "top_seed_seed", 
  "top_seed_is_winner", "top_seed_is_complete",
  "top_seed_is_started", "top_seed_num_home_runs",
  "top_seed_player_id", "top_seed_player_full_name", 
  "top_seed_player_link", "top_seed_top_derby_hit_data_launch_speed", 
  "top_seed_top_derby_hit_data_total_distance", "bottom_seed_complete",
  "bottom_seed_started", "bottom_seed_winner", "bottom_seed_seed",
  "bottom_seed_is_winner", "bottom_seed_is_complete",
  "bottom_seed_is_started", "bottom_seed_num_home_runs", 
  "bottom_seed_player_id", "bottom_seed_player_full_name",
  "bottom_seed_player_link", "bottom_seed_top_derby_hit_data_launch_speed",
  "bottom_seed_top_derby_hit_data_total_distance"
)

test_that("MLB Homerun Derby", {
  skip_on_cran()
  
  x <-  mlb_homerun_derby_bracket(game_pk = 511101)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
