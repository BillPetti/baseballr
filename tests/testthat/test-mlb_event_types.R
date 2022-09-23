
cols <- c(
  "plate_appearance", "hit", "event_code",
  "base_running_event", "event_description"
)

test_that("MLB Event Types", {
  skip_on_cran()
  
  x <- mlb_event_types()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
