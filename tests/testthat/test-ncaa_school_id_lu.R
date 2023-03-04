
cols <- c(
  "team_id",
  "team_name",
  "team_url",
  "conference_id",
  "conference",
  "division",
  "year",
  "season_id"
)

test_that("NCAA School ID Lookup", {
  skip_on_cran()
  
  x <- ncaa_school_id_lu("Van")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
