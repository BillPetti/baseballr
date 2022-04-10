
cols <- c(
  "hit_trajectory_code", "hit_trajectory_description"
)

test_that("MLB Hit Trajectories", {
  skip_on_cran()
  
  x <- mlb_hit_trajectories()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
