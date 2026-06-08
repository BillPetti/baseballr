cols <- c(
  "id", "fullName", "abbreviation", 
  "batting_order", "batting_position_num", 
  "team", "teamName", "teamID"
)

test_that("MLB Batting Orders", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_batting_orders(game_pk=566001)
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
