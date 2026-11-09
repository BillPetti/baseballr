# Offline tests for the Statcast-based helpers. fixtures/statcast_one_game.rds holds
# every pitch of one 2024 game (built by data-raw/statcast_fixture.R), so these run
# without Baseball Savant.

statcast_game <- function() {
  readRDS(test_path("fixtures", "statcast_one_game.rds"))
}

test_that("run_expectancy_code() adds run-expectancy columns", {
  skip_on_cran()
  df <- statcast_game()
  pa <- run_expectancy_code(df, level = "plate appearance")
  expect_s3_class(pa, "data.frame")
  expect_gt(nrow(pa), 0)
  expect_in(c("re24"), colnames(pa))
  pitch <- run_expectancy_code(df, level = "pitch")
  expect_s3_class(pitch, "data.frame")
  expect_gt(nrow(pitch), 0)
})

test_that("run_expectancy_table() returns a base-out state table", {
  skip_on_cran()
  df <- run_expectancy_code(statcast_game(), level = "plate appearance")
  re <- run_expectancy_table(df)
  expect_s3_class(re, "data.frame")
  expect_gt(nrow(re), 0)
})

test_that("linear_weights_savant() returns weights per event", {
  skip_on_cran()
  df <- run_expectancy_code(statcast_game(), level = "plate appearance")
  lw <- linear_weights_savant(df, level = "plate appearance")
  expect_s3_class(lw, "data.frame")
  expect_gt(nrow(lw), 0)
})

test_that("statline_from_statcast() summarises the game", {
  skip_on_cran()
  local_http_fixtures()   # it fetches FanGraphs wOBA weights via fg_guts()
  st <- statline_from_statcast(statcast_game(), base = "pa")
  expect_s3_class(st, "data.frame")
  expect_equal(nrow(st), 1)
})

test_that("edge_code() labels pitch locations", {
  skip_on_cran()
  df <- statcast_game()
  df$b_height <- "6-2"   # Statcast has no batter height; edge_code() wants feet-inches
  ec <- edge_code(df, height_var_name = "b_height")   # plate_x / plate_z / description mapped in
  expect_s3_class(ec, "data.frame")
  expect_equal(nrow(ec), nrow(df))
  expect_in(c("location", "Heart", "OutOfZone"), colnames(ec))
  expect_true(any(!is.na(ec$location)))
  expect_equal(sum(ec$called_strike), sum(df$description == "called_strike"))
  expect_equal(sum(ec$swing > 0), sum(df$description %in% c(
    "swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "foul_bunt", "hit_into_play"
  )))
})

test_that("ggspraychart() builds a ggplot from batted balls", {
  skip_on_cran()
  df <- statcast_game()
  df <- df[!is.na(df$hc_x) & !is.na(df$hc_y), , drop = FALSE]
  plain <- ggspraychart(df)
  expect_s3_class(ggplot2::ggplot_build(plain), "ggplot_built")
  events <- sort(unique(df$events))
  pal <- stats::setNames(grDevices::hcl.colors(length(events)), events)
  filled <- ggspraychart(df, fill_value = "events", fill_palette = pal, fill_legend_title = "Event")
  expect_s3_class(ggplot2::ggplot_build(filled), "ggplot_built")
  dens <- suppressWarnings(ggspraychart(df, density = TRUE))
  expect_s3_class(dens, "ggplot")
})
