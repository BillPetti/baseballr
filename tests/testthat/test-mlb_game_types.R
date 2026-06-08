
cols <- c(
  "game_type_id", "game_type_description"
)

test_that("MLB Game Types", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_game_types()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
