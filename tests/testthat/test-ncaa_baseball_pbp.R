
cols <- c(
  "date",
  "location",
  "attendance",
  "inning",
  "inning_top_bot",
  "score",
  "batting",
  "fielding",
  "description",
  "game_pbp_url",
  "game_pbp_id"
)

test_that("NCAA PBP", {
  skip_on_cran()
  
  y <- ncaa_baseball_pbp(game_info_url = "https://stats.ncaa.org/contests/2016254/box_score")
  z <- ncaa_baseball_pbp(game_pbp_url = "https://stats.ncaa.org/game/play_by_play/5005859")
  expect_equal(colnames(y), cols)
  expect_s3_class(y, "data.frame")
  expect_equal(colnames(z), cols)
  expect_s3_class(z, "data.frame")
})
