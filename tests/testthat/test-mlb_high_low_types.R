
cols <- c(
  "stat_name", "stat_lookup_param", "is_counting",
  "stat_label", "stat_groups", "org_types", "high_low_types"
)

test_that("MLB Stat High/Low Types", {
  skip_on_cran()
  
  x <- mlb_high_low_types()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
