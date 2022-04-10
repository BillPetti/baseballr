
cols <- c(
  "year", "last_name", "first_name", "player_id", "attempts",
  "avg_hit_angle", "anglesweetspotpercent", "max_hit_speed", 
  "avg_hit_speed", "fbld", "gb", "max_distance", "avg_distance",
  "avg_hr_distance", "ev95plus", 
  "ev95per-swing", "ev95percent", "barrels", "brl_percent", "brl_pa"
)

test_that("Statcast Leaderboards", {
  skip_on_cran()
  
  x <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = 2018)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
