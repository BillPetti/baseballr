
cols <- c(
  "roster_type_description", "roster_type_lookup_name", 
  "roster_type_parameter"
)

test_that("MLB Roster Types", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_roster_types()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
