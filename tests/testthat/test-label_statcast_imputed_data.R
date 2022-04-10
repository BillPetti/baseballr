
cols <- c(
  "pitch_type", "game_date", "release_speed", 
  "release_pos_x", "release_pos_z", "player_name",
  "batter", "pitcher", "events", "description", "spin_dir", 
  "spin_rate_deprecated", "break_angle_deprecated", 
  "break_length_deprecated", "zone", "des", "game_type",
  "stand", "p_throws", "home_team", "away_team", "type",
  "hit_location", "bb_type", "balls", "strikes", "game_year",
  "pfx_x", "pfx_z", "plate_x", "plate_z", "on_3b", "on_2b", "on_1b",
  "outs_when_up", "inning", "inning_topbot", "hc_x", "hc_y", 
  "tfs_deprecated", "tfs_zulu_deprecated", "fielder_2", "umpire",
  "sv_id", "vx0", "vy0", "vz0", "ax", "ay", "az", "sz_top", 
  "sz_bot", "hit_distance_sc", "launch_speed", "launch_angle", 
  "effective_speed", "release_spin_rate", "release_extension",
  "game_pk", "pitcher_1", "fielder_2_1", "fielder_3", "fielder_4",
  "fielder_5", "fielder_6", "fielder_7", "fielder_8", "fielder_9",
  "release_pos_y", "estimated_ba_using_speedangle", 
  "estimated_woba_using_speedangle", "woba_value", "woba_denom",
  "babip_value", "iso_value", "launch_speed_angle", "at_bat_number",
  "pitch_number", "pitch_name", "home_score", "away_score",
  "bat_score", "fld_score", "post_away_score", "post_home_score", 
  "post_bat_score", "post_fld_score", "if_fielding_alignment",
  "of_fielding_alignment", "spin_axis", 
  "delta_home_win_exp", "delta_run_exp", "ila", "ils", "imputed"
)

test_that("Statcast - Label Imputed Data", {
  skip_on_cran()
  
  statcast_df <- statcast_search("2017-05-01", "2017-05-02")
  x <- label_statcast_imputed_data(statcast_df)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
