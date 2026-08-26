test_that("ggpitchzone builds a plot from minimal pitch data", {
  skip_on_cran()
  df <- data.frame(
    plate_x = c(-0.3, 0.2, 0.8), plate_z = c(2.1, 3.0, 1.4),
    pitch_type = c("FF", "SL", "CH"),
    sz_top = c(3.4, 3.4, 3.4), sz_bot = c(1.6, 1.6, 1.6)
  )
  g <- ggpitchzone(df)
  expect_s3_class(g, "ggplot")
  g2 <- ggpitchzone(df, color_value = NULL)
  expect_s3_class(g2, "ggplot")
})
