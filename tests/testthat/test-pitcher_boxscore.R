context("MLB Pitcher Boxscore")

cols <- c(
  "id", "name", "name_display_first_last", "pos", "out",
  "bf", "er", "r", "h", "so", "hr", "bb", "np", "s",
  "w", "l", "sv", "bs", "hld", "s_ip", 
  "s_h", "s_r", "s_er", "s_bb", "s_so", "era", "note", "team"
)

test_that("MLB Pitcher Boxscore", {
  skip_on_cran()
  # pitchers
  url_base <- "http://gd2.mlb.com/components/game/mlb/"
  url <- paste0(url_base,
   "year_2016/month_05/day_21/gid_2016_05_21_milmlb_nynmlb_1/boxscore.xml")
  x <- pitcher_boxscore(url)
  
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
