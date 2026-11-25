test_that(".baseballr_with_warnings() names the wrapper and keeps the result", {
  f <- function() {
    df <- data.frame()
    tryCatch(
      expr = .baseballr_with_warnings("my_wrapper", {
        x <- as.integer("not a number") # warns: NAs introduced by coercion
        df <- data.frame(x = x, y = 1L)
      }),
      error = function(e) NULL
    )
    df
  }
  expect_warning(out <- f(), class = "baseballr_warning", regexp = "my_wrapper\\(\\): .*coercion")
  expect_equal(nrow(out), 1L)
  expect_identical(out$y, 1L)
})

test_that("nested .baseballr_with_warnings() raises one warning, named by the inner wrapper", {
  w <- testthat::capture_warnings(
    .baseballr_with_warnings("outer", .baseballr_with_warnings("inner", warning("boom")))
  )
  expect_length(w, 1L)
  expect_match(w, "inner\\(\\): boom")
  expect_no_match(w, "outer", fixed = TRUE) # not re-named by the outer wrapper
})

test_that(".baseballr_with_warnings() keeps return(), error handling, finally and the original class", {
  early <- function() {
    tryCatch(expr = .baseballr_with_warnings("early", {
      return("returned")
      "not reached"
    }), error = function(e) "error")
  }
  expect_identical(early(), "returned")

  ran <- FALSE
  expect_warning(caught <- tryCatch(
    expr = .baseballr_with_warnings("caught", {
      warning("first")
      stop("then failed")
    }),
    error = function(e) conditionMessage(e),
    finally = ran <- TRUE
  ), class = "baseballr_warning")
  expect_identical(caught, "then failed")
  expect_true(ran)

  custom <- function() warning(warningCondition("deprecated thing", class = "my_deprecation"))
  expect_no_warning(suppressWarnings(.baseballr_with_warnings("cls", custom()), classes = "my_deprecation"))
})

test_that("an ESPN wrapper returns its rows and names the warning when parsing warns", {
  # Until 2.1.0 these wrappers had tryCatch(warning =), which stopped the parse
  # at the first warning and returned an empty frame; then .try_catch() with an
  # empty warning handler, which kept the rows but dropped the warning. The
  # mocked check_status() warns before the first assignment, so the old
  # tryCatch() returns 0 rows and the old .try_catch() raises no warning.
  skip_on_cran()
  local_http_fixtures()
  local_mocked_bindings(check_status = function(res) warning("synthetic parse warning"))
  expect_warning(
    out <- suppressMessages(espn_college_baseball_scoreboard(season = "20250615")),
    class = "baseballr_warning",
    regexp = "espn_college_baseball_scoreboard\\(\\): synthetic parse warning"
  )
  expect_gt(nrow(out), 0L)
})

test_that("statcast_search.default() falls back to today for an unparseable end_date", {
  # as.Date() errors rather than warns on a bad string, so the old
  # tryCatch(warning =) fallback never ran and the error escaped; had it run,
  # its end_Date <- Sys.Date() bound only inside the handler.
  seen <- NULL
  local_mocked_bindings(statcast_search = function(start_date, end_date, ...) {
    seen <<- list(start = start_date, end = end_date)
    "searched"
  })
  out <- suppressMessages(statcast_search.default("2024-04-01", "not a date"))
  expect_identical(out, "searched")
  expect_identical(seen$start, as.Date("2024-04-01"))
  expect_identical(seen$end, Sys.Date())

  suppressMessages(statcast_search.default("2024-04-01", ""))
  expect_identical(seen$end, Sys.Date())

  suppressMessages(statcast_search.default("2024-04-01", "2024-04-03"))
  expect_identical(seen$end, as.Date("2024-04-03"))
})

test_that("chadwick_path() returns NULL with a message when cwevent is not on the PATH", {
  # It caught system2()'s "status 1" warning with tryCatch(warning =); it now
  # asks Sys.which(), which returns "" instead of warning.
  skip_if(nzchar(Sys.which("cwevent")), "cwevent is installed here")
  expect_message(out <- chadwick_path(), "cwevent is not installed")
  expect_null(out)
  expect_false(suppressMessages(chadwick_is_installed()))
})

# espn_mlb_player_seasons() and espn_college_baseball_player_seasons() both run
# .espn_baseball_athlete_seasons(). Its warnings named that helper and its
# errors named no function; both now name the export the user called.
.seasons_resp <- function() {
  httr2::response(
    status_code = 200L,
    url = "https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/athletes/1/seasons",
    headers = list("Content-Type" = "application/json"),
    body = charToRaw('{"items":[{"$ref":"http://x/athletes/1/seasons/2024?lang=en"}]}')
  )
}

test_that("a warning in a shared ESPN helper names the export that was called", {
  skip_on_cran()
  local_mocked_bindings(
    .retry_request = function(...) .seasons_resp(),
    check_status = function(res) warning("synthetic parse warning")
  )
  for (fn in c("espn_mlb_player_seasons", "espn_college_baseball_player_seasons")) {
    w <- testthat::capture_warnings(out <- get(fn)(athlete_id = 1L))
    expect_length(w, 1L)
    expect_match(w, paste0(fn, "\\(\\): synthetic parse warning"))
    expect_no_match(w, ".espn_baseball", fixed = TRUE)
    expect_identical(out$season, 2024L)
  }
  # Reached through lapply(), the export is called as FUN and found by identity.
  w <- testthat::capture_warnings(lapply(1L, espn_mlb_player_seasons))
  expect_match(w, "espn_mlb_player_seasons\\(\\): synthetic parse warning")
})

test_that("an error in a shared ESPN helper is reported under the export that was called", {
  skip_on_cran()
  local_mocked_bindings(.retry_request = function(...) stop("mock outage"))
  for (fn in c("espn_mlb_player_seasons", "espn_college_baseball_player_seasons")) {
    msgs <- testthat::capture_messages(out <- get(fn)(athlete_id = 1L))
    expect_match(msgs[[1]], paste0(fn, "\\(\\): Failed to retrieve ESPN"))
    expect_true(any(grepl("mock outage", msgs, fixed = TRUE)))
    expect_equal(nrow(out), 0L)
  }
})

test_that("an inner request fallback in a shared ESPN helper names the export", {
  skip_on_cran()
  # .espn_baseball_athlete_career_stats() catches each request failure itself.
  local_mocked_bindings(.retry_request = function(...) stop("mock outage"))
  msgs <- testthat::capture_messages(espn_college_baseball_player_career_stats(athlete_id = 1L))
  expect_true(any(grepl(
    "espn_college_baseball_player_career_stats(): Request to", msgs, fixed = TRUE
  )))
})
