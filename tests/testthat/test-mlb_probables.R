
cols <- c(
  "game_pk", "game_date", "fullName",
  "id", "team", "team_id", "home_plate_full_name", "home_plate_id"
)

test_that("MLB Probables", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_probables(game_pk = 566001)
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
