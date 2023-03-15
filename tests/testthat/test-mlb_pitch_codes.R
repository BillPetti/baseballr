
cols <- c(
  "pitch_code",
  "pitch_description",
  "swing_status",
  "swing_miss_status",
  "swing_contact_status",
  "sort_order",
  "strike_status",
  "ball_status",
  "pitch_status",
  "pitch_result_text"
)

test_that("MLB Pitch Codes", {
  skip_on_cran()
  
  x <- mlb_pitch_codes()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
