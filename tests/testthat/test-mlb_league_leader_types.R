
cols <- c(
  "leader_type"
)

test_that("MLB League Leader Types", {
  skip_on_cran()
  
  x <- mlb_league_leader_types()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
