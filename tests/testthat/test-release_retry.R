# A slow release download used to hit R's default 60 s timeout, and the
# loader returned the other seasons with only a vague warning.

# A reader that fails `fails` times, then returns `value`; records each call.
flaky <- function(fails, value = data.frame(x = 1)) {
  calls <- 0L
  timeouts <- numeric()
  f <- function(url) {
    calls <<- calls + 1L
    timeouts <<- c(timeouts, getOption("timeout"))
    if (calls <= fails) stop("Timeout of 60 seconds was reached")
    value
  }
  list(read = f, calls = function() calls, timeouts = function() timeouts)
}

test_that("a transient failure is retried with a raised timeout, then restored", {
  old <- options(timeout = 60)
  on.exit(options(old), add = TRUE)
  r <- flaky(2)
  expect_no_warning(out <- .read_release_asset("https://x/a_2024.rds", r$read))
  expect_equal(out, data.frame(x = 1))
  expect_equal(r$calls(), 3L)
  expect_equal(r$timeouts(), rep(150, 3))
  expect_equal(getOption("timeout"), 60)
})

test_that("a larger user timeout is kept", {
  old <- options(timeout = 3600)
  on.exit(options(old), add = TRUE)
  r <- flaky(0)
  .read_release_asset("https://x/a_2024.rds", r$read)
  expect_equal(r$timeouts(), 3600)
  expect_equal(getOption("timeout"), 3600)
})

test_that("a persistent failure warns with the url and returns a try-error", {
  old <- options(timeout = 60)
  on.exit(options(old), add = TRUE)
  r <- flaky(Inf)
  expect_warning(
    out <- .read_release_asset("https://x/a_2024.rds", r$read),
    "a_2024.rds.*incomplete"
  )
  expect_s3_class(out, "try-error")
  expect_equal(r$calls(), 3L)
  expect_equal(getOption("timeout"), 60)
})

test_that("a 404 is not retried", {
  calls <- 0L
  read <- function(url) {
    calls <<- calls + 1L
    warning("cannot open URL 'u': HTTP status was '404 Not Found'")
    stop("cannot open the connection")
  }
  expect_warning(.read_release_asset("https://x/a_1800.rds", read), "a_1800.rds")
  expect_equal(calls, 1L)
})

test_that("warnings from a read that succeeds are passed on", {
  read <- function(url) {
    warning("a note from the reader")
    1
  }
  expect_warning(out <- .read_release_asset("https://x/a_2024.rds", read), "a note from the reader")
  expect_equal(out, 1)
})

test_that("a loader keeps the seasons that load and names the one that failed", {
  old <- options(timeout = 60)
  on.exit(options(old), add = TRUE)
  calls <- 0L
  local_mocked_bindings(.read_rds_url = function(url) {
    calls <<- calls + 1L
    if (endsWith(url, "_2024.rds")) stop("Timeout of 60 seconds was reached")
    data.frame(season = 2023L, game_id = 1L)
  })
  expect_warning(
    out <- load_ncaa_baseball_pbp(2023:2024),
    "ncaa_baseball_pbp_2024.rds", fixed = TRUE
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$season, 2023L)
  expect_equal(calls, 4L)
  expect_s3_class(out, "baseballr_data")
  expect_false("data.table" %in% class(out))
  expect_equal(getOption("timeout"), 60)
})

test_that("rds_from_url reads a local file", {
  src <- tempfile(fileext = ".rds")
  on.exit(unlink(src), add = TRUE)
  saveRDS(data.frame(x = 1:2), src, compress = FALSE)
  path <- normalizePath(src, winslash = "/")
  url <- paste0("file://", if (!startsWith(path, "/")) "/", path)
  expect_equal(.read_rds_url(url), data.frame(x = 1:2))
  expect_equal(rds_from_url(url)$x, 1:2)
})
