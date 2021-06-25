context("MLB Probables")

cols <- c(
  "game_pk", "game_date", "fullName", "id", 
  "team", "team_id", "home_plate_full_name", "home_plate_id"
)

test_that("MLB Probables", {
  skip_on_cran()
  
  x <- get_probables_mlb(566001)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
