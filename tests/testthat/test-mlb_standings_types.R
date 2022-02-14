
cols <- c(
  "standings_type_name", "standings_type_description"
)

test_that("MLB Standing Types", {
  skip_on_cran()
  
  x <- mlb_standings_types()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
