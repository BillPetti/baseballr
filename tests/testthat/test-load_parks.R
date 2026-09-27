# Offline tests stub csv_from_url() with rows copied verbatim from the published
# mlb_parks release csv; the last test is the gated live smoke call.

test_that("load_mlb_park_dimensions reads the season-less file with contract dtypes", {
  skip_on_cran()
  seen <- character()
  local_mocked_bindings(csv_from_url = function(input, ...) {
    seen <<- c(seen, input)
    data.table::fread(text = paste(
      "league,season,venue_id,venue_name,retro_park_id,left_line_ft,left_ft,left_center_ft,center_ft,right_center_ft,right_ft,right_line_ft,capacity,turf_type,roof_type,azimuth_deg,elevation_ft,latitude,longitude,notes",
      "mlb,2025,3,Fenway Park,BOS07,310,379,390,420,380,,302,37755,Grass,Open,45.0,21,42.346456,-71.097441,",
      "mlb,2013,2680,Petco Park,SAN02,336,,386,396,391,,322,42524,Grass,Open,0.0,23,32.707861,-117.157278,\"curated left_line_ft 334 -> 336, left_ft 367 -> null\"",
      sep = "\n"
    ), ...)
  })
  x <- load_mlb_park_dimensions()

  expect_equal(seen, paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/releases/download/",
    "mlb_parks/mlb_park_dimensions.csv"
  ))
  expect_s3_class(x, "baseballr_data")
  expect_equal(colnames(x), names(.park_dimensions_col_classes))
  expect_type(x$venue_id, "character")
  expect_equal(x$venue_id, c("3", "2680"))
  expect_type(x$center_ft, "integer")
  expect_true(is.na(x$right_ft[1]))
  expect_type(x$azimuth_deg, "double")
  expect_true(is.na(x$notes[1]))
  expect_match(x$notes[2], "curated left_line_ft", fixed = TRUE)
})

test_that("load_mlb_park_dimensions warns and returns the contract columns on failure", {
  skip_on_cran()
  local_mocked_bindings(csv_from_url = function(...) stop("HTTP status was '404 Not Found'"))
  expect_warning(x <- load_mlb_park_dimensions(), "404")
  expect_equal(nrow(x), 0)
  expect_equal(colnames(x), names(.park_dimensions_col_classes))
})

test_that("load_mlb_park_dimensions live: Fenway Park 2025 fences", {
  skip_on_cran()
  skip_load_test()
  x <- load_mlb_park_dimensions()
  expect_gt(nrow(x), 0)
  fenway <- x[x$venue_id == "3" & x$season == 2025L, ]
  expect_equal(nrow(fenway), 1)
  expect_equal(fenway$venue_name, "Fenway Park")
  expect_equal(
    c(fenway$left_line_ft, fenway$center_ft, fenway$right_line_ft),
    c(310L, 420L, 302L)
  )
})
