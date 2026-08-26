test_that("fg_projections returns projections for bat and pit", {
  skip_on_cran()
  skip_if_offline("www.fangraphs.com")

  b <- fg_projections(type = "steamer", stats = "bat")
  if (!is.null(b) && nrow(b) > 0) {
    expect_s3_class(b, "baseballr_data")
    expect_in(c("team", "pa"), colnames(b))
  }
  expect_error(fg_projections(type = "bogus"))
  expect_error(fg_projections(stats = "fielding"))
})
