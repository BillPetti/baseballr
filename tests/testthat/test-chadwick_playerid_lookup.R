
cols <- c(
  "first_name", "last_name", "given_name",
  "name_suffix", "nick_name", "birth_year",
  "mlb_played_first", "mlbam_id", "retrosheet_id",
  "bbref_id", "fangraphs_id"
)

test_that("Chadwick PlayerID Lookup", {
  skip_on_cran()
  
  x <- playerid_lookup("Ohtani", "Shohei")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
