
cols <- c(
  "timecodes"
)

test_that("MLB Game Time Codes", {
  skip_on_cran()
  
  x <- mlb_game_timecodes(game_pk = 632970)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
