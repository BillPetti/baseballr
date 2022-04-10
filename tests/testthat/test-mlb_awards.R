
cols <- c(
  "award_id", "award_name", "award_description", "sort_order",
  "notes", "sport_id", "sport_link", "league_id", "league_link"
)

test_that("MLB Awards", {
  skip_on_cran()
  
  x <- mlb_awards()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
