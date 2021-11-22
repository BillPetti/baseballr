context("Batter Boxscores")

cols <- c(
  "id", "name", "name_display_first_last", "pos", "bo",
  "ab", "po", "r", "a", "bb", "sac", "t", "sf", "h", "e",
  "d", "hbp", "so", "hr", "rbi", "lob", "fldg", "sb", 
  "cs", "s_hr", "s_rbi", "s_h", "s_bb", 
  "s_r", "s_so", "avg", "obp", "slg", "ops", "go", 
  "ao", "gidp", "note", "team"
)
url_base <- "http://gd2.mlb.com/components/game/mlb/"
url <- paste0(url_base, "year_2016/month_05/day_21/gid_2016_05_21_milmlb_nynmlb_1/boxscore.xml")
batter_boxscore(url)
test_that("Batter Boxscores", {
  skip_on_cran()
  
  url_base <- "http://gd2.mlb.com/components/game/mlb/"
  
  url <- paste0(url_base, "year_2016/month_05/day_21/gid_2016_05_21_milmlb_nynmlb_1/boxscore.xml")
  
  x <- batter_boxscore(url)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
