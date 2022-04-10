
cols <- c(
  "position_short_name", "position_full_name",
  "position_abbreviation", "position_code", 
  "position_type", "position_formal_name",
  "position_display_name", "outfield", "game_position",
   "pitcher", "fielder"
)

test_that("MLB Positions", {
  skip_on_cran()
  
  x <- mlb_positions()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
