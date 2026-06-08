#' @name espn_mlb
#' @title **ESPN MLB Endpoint Overview**
#' @description
#' Wrappers around ESPN's Major League Baseball endpoints (`espn_mlb_*()`).
#' They cover ESPN's three public hosts:
#'
#' - `site.api.espn.com` -- scoreboard, schedule, game summary (play-by-play,
#'   team / player box scores), teams, rosters.
#' - `sports.core.api.espn.com` -- reference data: athletes, coaches, seasons,
#'   franchises, draft, futures, leaders, standings, events, plays, and
#'   per-competitor game detail.
#' - `site.web.api.espn.com` -- athlete overviews, game logs, splits, and
#'   league-wide statistics.
#'
#' These complement the MLB Stats API wrappers (`mlb_*()`), FanGraphs (`fg_*()`),
#' Baseball Reference (`bref_*()`), and Baseball Savant / Statcast
#' (`statcast_*()`) families. The `espn_mlb_*()` naming mirrors the sister
#' SportsDataverse packages -- hoopR (`espn_nba_*` / `espn_mbb_*`), wehoop
#' (`espn_wnba_*` / `espn_wbb_*`), and cfbfastR (`espn_cfb_*`).
#'
#' @details
#'
#' ## **Play-by-play, scoreboard, schedule, box scores**
#'
#' - [espn_mlb_pbp()] -- play-by-play (pitch- and at-bat-level).
#' - [espn_mlb_scoreboard()] -- scores / schedule for a date (`YYYYMMDD`).
#' - [espn_mlb_team_box()] -- team batting / pitching / fielding box score.
#' - [espn_mlb_player_box()] -- player batting / pitching box score.
#' - [espn_mlb_game_all()] -- all three (plays, team box, player box) in one call.
#' - [espn_mlb_game_rosters()] -- the two game-day rosters.
#'
#' ## **Reference data**
#'
#' - [espn_mlb_teams()], [espn_mlb_team_roster()], [espn_mlb_standings()].
#' - [espn_mlb_team_stats()], [espn_mlb_player_stats()], [espn_mlb_leaders()].
#' - [espn_mlb_athletes_index()], [espn_mlb_player_info()], [espn_mlb_draft()].
#'
#' ## **HTTP layer**
#'
#' ESPN wrappers call `.retry_request()` directly without `...`, so per-call
#' proxy overrides are not supported. Route requests through
#' `options(baseballr.proxy = ...)` or the `http_proxy` / `https_proxy`
#' environment variables instead.
#'
#' @keywords ESPN
#' @family ESPN MLB Functions
NULL
