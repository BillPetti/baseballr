context("MLB Get Retrosheet Data")

cols <- c(
  "game_pk", "game_date", "fullName",
  "id", "team", "team_id", "home_plate_full_name", "home_plate_id"
)

test_that("MLB Get Retrosheet Data", {
  skip_on_cran()
  
  x <-get_retrosheet_data(path_to_directory = "data-raw", 
                          years_to_acquire = c(1957,1959), 
                          sequence_years = F)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
