
cols <- c(
  "stat_name", "stat_lookup_param", "is_counting", "stat_label", "stat_group"
)

test_that("MLB Baseball Stats", {
  skip_on_cran()
  
  x <-  mlb_baseball_stats()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
