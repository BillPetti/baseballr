
cols <- c(
  "conference_id", "conference_name", "link", 
  "conference_abbreviation", "has_wildcard", "conference_name_short", 
  "league_id", "league_link", "sport_id", "sport_link"
)

test_that("MLB Conferences", {
  skip_on_cran()
  
  x <- mlb_conferences()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
