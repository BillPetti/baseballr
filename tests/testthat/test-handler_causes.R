# A wrapper's tryCatch() error handler must name the cause of a failure, not
# only "no data available": a 429, a 403 and a parse bug otherwise read like an
# empty result. Every request is made to fail offline, and every function with
# a tryCatch() error handler that is a block or a .report_api_error() call must
# pass that cause on, without the error escaping.

# Functions this setup cannot exercise, with why.
.handler_cause_skip <- c(
  statcast_search = "re-raises the failure on purpose (stop('No payload acquired: <cause>'))",
  statcast_search.default = "calls statcast_search(), which re-raises on purpose",
  scrape_statcast_savant = "re-raises the failure on purpose, as statcast_search() does",
  scrape_statcast_savant.Date = "re-raises the failure on purpose, as statcast_search() does",
  scrape_statcast_savant.default = "re-raises the failure on purpose, as statcast_search() does",
  .ncaa_chromote_fetch = "drives a headless Chrome, with no HTTP request to fail"
)

# Arguments a few functions need beyond the shared list below.
.handler_cause_overrides <- list(
  espn_mlb_player_stats = list(season_type = "regular"),
  espn_mlb_team_stats = list(season_type = "regular"),
  fg_projections = list(type = "steamer"),
  bref_standings_on_date = list(division = "AL East"),
  standings_on_date_bref = list(division = "AL East"),
  mlb_high_low_stats = list(org_type = "Team")
)

.handler_cause_args <- list(
  league = "mlb", year = 2024L, season = 2024L, game_pk = 745444L, game_id = 401569123L,
  event_id = 401569123L, team_id = 10L, athlete_id = 33192L, player_id = 545361L,
  person_id = 545361L, playerid = 545361L, coach_id = 1L, franchise_id = 10L,
  position_id = 1L, award_id = 1L, group_id = 1L, tournament_id = 1L, competitor_id = 10L,
  official_id = 1L, play_id = 1L, propbet_id = 1L, pick = 1L, round = 1L, draft_id = 1L,
  season_type = 2L, week = 1L, date = "2024-06-01", start_date = "2024-06-01",
  end_date = "2024-06-02", t1 = "2024-06-01", t2 = "2024-06-02", division = 1L,
  team_abbr = "NYY", team = "NYY", game_info_url = "https://stats.ncaa.org/contests/1/box_score",
  game_pbp_url = "https://stats.ncaa.org/contests/1/play_by_play", url = "https://example.com/x",
  resource = "standings", sport_id = 1L, league_id = 103L, stat_type = "season",
  stat_group = "hitting", leader_categories = "homeRuns", type = "batting",
  order = 1L, provider_id = 1L, ranking_id = 1L, record_id = 1L, Tm = "NYY",
  roster_type = "active", updated_since = "2024-06-01", timecode = "20240601_000000",
  start_timecode = "20240601_000000", end_timecode = "20240601_010000", sort_stat = "atBats"
)

# Fail a request made by reading a URL directly; parse anything else as usual.
.fail_urls <- function(f) {
  force(f)
  function(x, ...) {
    if (is.character(x) && length(x) == 1L && grepl("^https?://", x)) stop("mock outage")
    f(x, ...)
  }
}

test_that("error handlers report the cause of a failed request", {
  skip_on_cran()
  ns <- asNamespace("baseballr")
  # Selected by handler shape, not by whether it already reports the cause,
  # so a handler that loses conditionMessage() fails here instead of
  # silently dropping out of the list.
  has_handler <- function(nm) {
    f <- get(nm, envir = ns)
    if (!is.function(f) || is.primitive(f)) return(FALSE)
    b <- paste(deparse(body(f)), collapse = "\n")
    grepl("tryCatch", b, fixed = TRUE) &&
      grepl("error = function\\(\\w+\\) (\\{|\\.report_api_error)", b)
  }
  fns <- Filter(has_handler, ls(ns, all.names = TRUE))
  expect_true(all(names(.handler_cause_skip) %in% fns))
  fns <- setdiff(fns, names(.handler_cause_skip))
  expect_gt(length(fns), 200L)

  local_mocked_bindings(
    req_perform = function(...) stop("mock outage"),
    .package = "httr2"
  )
  local_mocked_bindings(fromJSON = .fail_urls(jsonlite::fromJSON), .package = "jsonlite")
  # rvest::read_html() keeps its own copy of the generic, so fail the URL inside xml2.
  local_mocked_bindings(path_to_connection = .fail_urls(xml2:::path_to_connection), .package = "xml2")
  local_mocked_bindings(csv_from_url = function(...) stop("mock outage"))

  silent <- character()
  escaped <- character()
  for (nm in fns) {
    f <- get(nm, envir = ns)
    args <- .handler_cause_args[intersect(names(formals(f)), names(.handler_cause_args))]
    args[names(.handler_cause_overrides[[nm]])] <- .handler_cause_overrides[[nm]]
    msgs <- character()
    withCallingHandlers(
      tryCatch(do.call(f, args), error = function(e) escaped <<- c(escaped, nm)),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) invokeRestart("muffleWarning")
    )
    if (!any(grepl("mock outage", msgs, fixed = TRUE))) silent <- c(silent, nm)
  }
  expect_identical(escaped, character())
  expect_identical(setdiff(silent, escaped), character())
})
