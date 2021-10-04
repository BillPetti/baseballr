context("Get Umpire IDs Petti")

cols <- c(
  "id", "position", "name", "game_pk", "game_date"
)

test_that("Get Umpire IDs Petti", {
  skip_on_cran()
  
  x <-get_umpire_ids_petti()
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
