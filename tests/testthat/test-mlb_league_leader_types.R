
cols <- c(
  "leader_type"
)

test_that("MLB League Leader Types", {
  skip_mlb_test()
  skip_on_cran()
  
  x <- mlb_league_leader_types()
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
