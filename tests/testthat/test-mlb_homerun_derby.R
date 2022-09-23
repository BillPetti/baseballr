
cols <- c(
  "game_pk", "event_name", "event_date", 
  "event_type_code", "event_type_name",
  "venue_id", "venue_name", "round",
  "num_batters", "batter", "batter_id", 
  "batter_link", "top_seed_complete", 
  "top_seed_started", "top_seed_winner",
  "bonus_time", "home_run", "tie_breaker",
  "is_home_run", "time_remaining",
  "is_bonus_time", "is_tie_breaker", 
  "hit_data_launch_speed", "hit_data_launch_angle",
  "hit_data_total_distance", "hit_data_coordinates_coord_x",
  "hit_data_coordinates_coord_y", "hit_data_coordinates_landing_pos_x",
  "hit_data_coordinates_landing_pos_y", 
  "hit_data_trajectory_data_trajectory_polynomial_x", 
  "hit_data_trajectory_data_trajectory_polynomial_y",
  "hit_data_trajectory_data_trajectory_polynomial_z", 
  "hit_data_trajectory_data_valid_time_interval",
  "top_seed_seed", "top_seed_is_winner", "top_seed_is_complete",
  "top_seed_is_started", "top_seed_num_home_runs", 
  "top_seed_player_id", "top_seed_player_full_name",
  "top_seed_player_link", "top_seed_top_derby_hit_data_launch_speed",
  "top_seed_top_derby_hit_data_total_distance", "bottom_seed_complete",
  "bottom_seed_started", "bottom_seed_winner", "bottom_seed_seed",
  "bottom_seed_is_winner", "bottom_seed_is_complete",
  "bottom_seed_is_started", "bottom_seed_num_home_runs", 
  "bottom_seed_player_id", "bottom_seed_player_full_name", 
  "bottom_seed_player_link", "bottom_seed_top_derby_hit_data_launch_speed",
  "bottom_seed_top_derby_hit_data_total_distance", 
  "venue_link", "is_multi_day", 
  "is_primary_calendar", "file_code", "event_number",
  "public_facing"
)

test_that("MLB Homerun Derby", {
  skip_on_cran()
  
  x <-  mlb_homerun_derby(game_pk = 511101)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
