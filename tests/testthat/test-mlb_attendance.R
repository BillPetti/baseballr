
cols <- c(
  "openings_total", "openings_total_away", 
  "openings_total_home", "openings_total_lost", 
  "games_total", "games_away_total", "games_home_total", 
  "year", "attendance_average_away", "attendance_average_home",
  "attendance_average_ytd", "attendance_high", "attendance_high_date",
  "attendance_low", "attendance_low_date", "attendance_opening_average",
  "attendance_total", "attendance_total_away", "attendance_total_home",
  "attendance_high_game_game_pk", "attendance_high_game_link", 
  "attendance_high_game_day_night", "attendance_high_game_content_link",
  "attendance_low_game_game_pk", "attendance_low_game_link", 
  "attendance_low_game_day_night", "attendance_low_game_content_link", 
  "game_type_id", "game_type_description", 
  "team_id", "team_name", "team_link"
)

test_that("MLB Attendance", {
  skip_on_cran()
  
  x <- mlb_attendance(team_id = 109, season = 2021)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
