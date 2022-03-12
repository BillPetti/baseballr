
cols <- c(
  "season_id",
  "has_wildcard",
  "pre_season_start_date",
  "season_start_date",
  "regular_season_start_date",
  "regular_season_end_date",
  "season_end_date",
  "offseason_start_date",
  "off_season_end_date",
  "season_level_gameday_type",
  "game_level_gameday_type",
  "qualifier_plate_appearances",
  "qualifier_outs_pitched",
  "post_season_start_date",
  "post_season_end_date",
  "last_date1st_half",
  "all_star_date",
  "first_date2nd_half",
  "pre_season_end_date",
  "spring_start_date",
  "spring_end_date"
)

test_that("MLB Seasons All", {
  skip_on_cran()
  
  x <- mlb_seasons_all(sport_id = 1)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
