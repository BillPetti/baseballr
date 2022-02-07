
cols <- c(
  "key_person", "key_uuid", "key_mlbam",
  "key_retro", "key_bbref", "key_bbref_minors", 
  "key_fangraphs", "key_npb", "key_sr_nfl",
  "key_sr_nba", "key_sr_nhl", "key_findagrave",
  "name_last", "name_first", "name_given", "name_suffix",
  "name_matrilineal", "name_nick", "birth_year", "birth_month",
  "birth_day", "death_year", "death_month", "death_day", 
  "pro_played_first", "pro_played_last", "mlb_played_first",
  "mlb_played_last", "col_played_first", "col_played_last",
  "pro_managed_first", "pro_managed_last", "mlb_managed_first",
  "mlb_managed_last", "col_managed_first", "col_managed_last",
  "pro_umpired_first", 
  "pro_umpired_last", "mlb_umpired_first", "mlb_umpired_last"
)

test_that("MLB Chadwick Lookup", {
  skip_on_cran()
  
  x <- chadwick_player_lu()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
