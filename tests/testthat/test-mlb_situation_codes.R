
cols <- c(
  "situation_code", "sort_order", "navigation_menu",
  "situation_code_description",
  "team", "batting", "fielding", "pitching"
)

test_that("MLB Situation Codes", {
  skip_on_cran()
  
  x <- mlb_situation_codes()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
