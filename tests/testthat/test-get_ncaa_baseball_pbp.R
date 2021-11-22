context("NCAA Get PBP")

cols <- c(
  "date", "location", "attendance", "inning", 
  "inning_top_bot", "score", "batting", "fielding", "description"
)

test_that("NCAA Get PBP", {
  skip_on_cran()
  x <- get_ncaa_schedule_info(736, 2019)$game_info_url[2]
  y <- get_ncaa_baseball_pbp(game_info_url = x)
  
  expect_equal(colnames(y), cols)
  expect_s3_class(y, "data.frame")
})
