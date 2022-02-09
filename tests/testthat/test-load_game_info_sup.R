
cols <- c(
  "game_date", "game_pk", "venue_name", "venue_id", 
  "temperature", "other_weather", "wind", "attendance",
  "start_time", "elapsed_time", "game_id", "game_type",
  "home_sport_code", "official_scorer", 
  "date", "status_ind", "home_league_id", "gameday_sw"
)

test_that("Load MLB Game Info Supplement", {
  skip_on_cran()
  
  x <- load_game_info_sup()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
