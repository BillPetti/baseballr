
cols <- c(
  "stat_group_name"
)

test_that("MLB Stat Groups", {
  skip_on_cran()
  
  x <- mlb_stat_groups()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
