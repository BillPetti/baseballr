
cols <- c(
  "schedule_event_type_code", "schedule_event_type_name"
)

test_that("MLB Schedule Event Types", {
  skip_on_cran()
  
  x <- mlb_schedule_event_types()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
