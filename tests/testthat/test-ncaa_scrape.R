
cols <- c(
  "year", "team_name", "conference", "division",
  "Jersey", "Player", "Yr", "Pos", "GP", "GS",
  "BA", "OBPct", "SlgPct", "R", "AB", "H", "2B", 
  "3B", "TB", "HR", "RBI", "BB", "HBP", "SF",
  "SH", "K", "DP", "CS", "Picked",
  "SB", "RBI2out", "team_id", "conference_id", 
  "player_id", "player_url"
)

test_that("NCAA Scrape", {
  skip_on_cran()
  
  x <- ncaa_scrape(team_id = 255, year = 2021, type = "batting")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
