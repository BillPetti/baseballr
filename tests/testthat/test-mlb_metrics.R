
cols <- c(
  "metric_name", "metric_id", "stat_group", "metric_unit"
)

test_that("MLB Metrics", {
  skip_on_cran()
  
  x <- mlb_metrics()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
