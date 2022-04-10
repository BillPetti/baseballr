
cols <- c(
  "date", "location", "attendance", "inning", 
  "inning_top_bot", "score", "batting", "fielding", "description"
)

test_that("NCAA PBP", {
  skip_on_cran()
  x <- ncaa_schedule_info(736, 2021)$game_info_url[2]
  y <- ncaa_baseball_pbp(game_info_url = x)
  
  expect_equal(colnames(y), cols)
  expect_s3_class(y, "data.frame")
})
