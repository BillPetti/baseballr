context("MLB Get Batting Orders")

cols <- c(
  "id", "fullName", "abbreviation", 
  "batting_order", "batting_position_num", 
  "team", "teamName", "teamID"
)

test_that("MLB Get Batting Orders", {
  skip_on_cran()
  
  x <- get_batting_orders(game_pk=566001)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
