
# Core Statcast columns plus the three columns label_statcast_imputed_data()
# adds (ila, ils, imputed). Asserted subset-direction so Savant schema changes
# do not break the test; the point is that the label columns are appended.
cols <- c(
  "pitch_type", "game_date", "release_speed", "player_name",
  "batter", "pitcher", "events", "description", "game_pk",
  "ila", "ils", "imputed"
)

test_that("Statcast - Label Imputed Data", {
  skip_statcast_test()
  skip_on_cran()

  statcast_df <- statcast_search("2017-05-01", "2017-05-02")
  if (is.null(statcast_df) || !is.data.frame(statcast_df) || nrow(statcast_df) == 0) {
    skip("No rows returned from Baseball Savant at test time")
  }
  x <- label_statcast_imputed_data(statcast_df)

  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
