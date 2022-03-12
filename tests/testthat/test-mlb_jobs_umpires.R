
cols <- c(
  "jersey_number",
  "job",
  "job_code",
  "title",
  "person_id",
  "person_full_name",
  "person_link"
)

test_that("MLB Jobs - Umpires", {
  skip_on_cran()
  
  x <- mlb_jobs_umpires(sport_id = 1)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
