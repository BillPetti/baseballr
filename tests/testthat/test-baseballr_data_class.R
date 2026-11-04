# A "data.table" class after "tbl_df" sends tail() to data.table's method,
# whose x[i] then hits tibble's `[`, a column selector: "Can't subset columns
# past the end" (hoopR #88). baseballr_data objects must not carry it.

test_that("make_baseballr_data() output supports head()/tail()/print()", {
  skip_on_cran()
  out <- make_baseballr_data(data.frame(a = 1:10, b = letters[1:10]), "unit test data", Sys.time())

  expect_s3_class(out, "baseballr_data")
  expect_false(inherits(out, "data.table"))
  expect_equal(tail(out)$a, 5:10)
  expect_equal(head(out, 3)$a, 1:3)
  expect_no_error(capture.output(print(out)))
})

test_that("every exported load_*() and chadwick_player_lu() result supports head()/tail() (offline)", {
  skip_on_cran()
  fake <- function(...) data.table::data.table(a = 1:10, b = letters[1:10])
  local_mocked_bindings(rds_from_url = fake, csv_from_url = fake)

  fns <- c(grep("^load_", getNamespaceExports("baseballr"), value = TRUE),
           "chadwick_player_lu")
  expect_gt(length(fns), 20)
  for (f in fns) {
    x <- suppressMessages(suppressWarnings(getExportedValue("baseballr", f)()))
    expect_s3_class(x, "baseballr_data")
    expect_false(inherits(x, "data.table"), label = f)
    expect_equal(nrow(tail(x)), 6L, label = f)
    expect_equal(nrow(head(x, 3)), 3L, label = f)
  }
})
