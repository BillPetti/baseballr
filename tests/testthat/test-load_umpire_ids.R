
cols <- c(
  "id", "position", "name", "game_pk", "game_date"
)

test_that("Load Umpire IDs", {
  skip_load_test()
  skip_on_cran()
  
  x <-load_umpire_ids()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
