
cols <- c(
  "player_status_code", "player_status_description"
)

test_that("MLB Player Status Codes", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_player_status_codes()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
