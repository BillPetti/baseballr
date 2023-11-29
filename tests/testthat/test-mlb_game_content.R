
cols <- c(
  "title", "epg_id", "content_id", "media_id", "media_state", 
  "media_feed_type", "media_feed_sub_type", "call_letters", 
  "fox_auth_required", "tbs_auth_required", "espn_auth_required",
  "fs1auth_required", "mlbn_auth_required", "free_game", "type",
  "description", "rendition_name", "language"
)

test_that("MLB Game Content", {
  skip_on_cran()
  
  x <- mlb_game_content(game_pk = 566001)
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
