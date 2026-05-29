
cols <- c(
  "timecodes"
)

test_that("MLB Game Time Codes", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_game_timecodes(game_pk = 632970)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No timecodes returned from the MLB Stats API at test time")
  }

  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
