
cols <- c(
  "season", "lg_woba", "woba_scale", "wBB",
  "wHBP", "w1B", "w2B", "w3B", "wHR",
  "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"
)

test_that("FanGraphs GUTS Factors", {
  skip_on_cran()
  
  x <- fg_guts()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
