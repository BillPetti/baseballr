context("Player Name Lookup")

cols <- c(
  "name_first", "name_last", "name_given", "name_suffix", 
  "name_nick", "birth_year", "mlb_played_first", 
  "key_mlbam", "key_retro", "key_bbref", "key_fangraphs"
)

test_that("Player Name Lookup", {
  skip_on_cran()
  
  x <- playername_lookup("kaaihki01")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
