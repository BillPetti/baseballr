
cols <- c(
  "date", "opponent", "result", 
  "score", "innings", "opponent_slug", 
  "slug", "game_info_url"
)

test_that("NCAA Get Schedule Info", {
  skip_on_cran()
  
  x <- ncaa_schedule_info(736, 2019)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
