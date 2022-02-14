
cols <- c(
  "hits_per9inn", "runs_per9inn", "pitches_per9inn", 
  "plate_appearances_per9inn", "hits_per_game",
  "runs_per_game", "innings_played_per_game",
  "pitches_per_game", "pitchers_per_game",
  "plate_appearances_per_game", "total_game_time",
  "total_innings_played", "total_hits", "total_runs",
  "total_plate_appearances", "total_pitchers", "total_pitches",
  "total_games", "total7inn_games", "total9inn_games",
  "total_extra_inn_games", "time_per_game", "time_per_pitch",
  "time_per_hit", "time_per_run", "time_per_plate_appearance",
  "time_per9inn", "time_per77plate_appearances",
  "total_extra_inn_time", "time_per7inn_game", 
  "time_per7inn_game_without_extra_inn",
  "total7inn_games_scheduled", 
  "total7inn_games_without_extra_inn",
  "total9inn_games_without_extra_inn", 
  "total9inn_games_scheduled", "hits_per_run",
  "pitches_per_pitcher", "season", 
  "total9inn_games_completed_early",
  "total7inn_games_completed_early", 
  "sport_id", "sport_code", "sport_link",
  "pr_portal_calculated_fields_total7inn_games",
  "pr_portal_calculated_fields_total9inn_games",
  "pr_portal_calculated_fields_total_extra_inn_games", 
  "pr_portal_calculated_fields_time_per7inn_game",
  "pr_portal_calculated_fields_time_per9inn_game", 
  "pr_portal_calculated_fields_time_per_extra_inn_game"
)

test_that("MLB Game Pace", {
  skip_on_cran()
  
  x <- mlb_game_pace(season = 2021, start_date = "09/14/2021", end_date = "09/16/2021")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
