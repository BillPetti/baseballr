
cols <- c(
  "school", "conference", "school_id",
  "year", "division", "conference_id"
)

test_that("NCAA School ID Lookup", {
  skip_on_cran()
  
  x <- school_id_lu("Van")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
