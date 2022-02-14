
cols <- c(
  "player_status_code", "player_status_description"
)

test_that("MLB Player Status Codes", {
  skip_on_cran()
  
  x <- mlb_player_status_codes()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
