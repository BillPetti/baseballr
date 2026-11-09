# ESPN can answer the scoreboard-conferences route with an empty array (it does
# for MLB); the parsers must return the documented empty frame, not error.

test_that("conference parsers return an empty frame for an empty payload", {
  skip_on_cran()
  skip_if_not(exists("local_mocked_responses", envir = asNamespace("httr2")))
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200L,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw('{"conferences":[]}')
    )
  })
  for (x in list(espn_mlb_conferences(), espn_college_baseball_conferences())) {
    expect_s3_class(x, "baseballr_data")
    expect_equal(nrow(x), 0)
    expect_in(c("group_id", "conference_name", "conference_id"), colnames(x))
  }
})
