
cols <- c(
  "abstract_game_state", "coded_game_state",
  "detailed_state", "status_code", "reason", "abstract_game_code"
)

test_that("MLB Game Status Codes", {
  skip_on_cran()
  
  x <- mlb_game_status_codes()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
