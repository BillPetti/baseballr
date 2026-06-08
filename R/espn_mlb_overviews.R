# Category overview (doc-only) topics for the ESPN MLB wrapper family. These
# group the espn_mlb_*() functions for the pkgdown reference index and the
# "Funcs" navbar dropdown, mirroring the mlb_*_endpoints overview topics.

#' @name espn_mlb_game_endpoints
#' @aliases espn_mlb_game_endpoints espn_mlb_game espn_mlb_pbp_overview
#' @title **ESPN MLB Game Endpoint Overview**
#' @description
#'
#' * `espn_mlb_scoreboard()`: Scores and schedule for a date (`YYYYMMDD`).
#' * `espn_mlb_pbp()`: Pitch- and at-bat-level play-by-play for a game.
#' * `espn_mlb_team_box()`: Team batting / pitching / fielding box score (one wide row per team).
#' * `espn_mlb_player_box()`: Player batting / pitching box score (one row per athlete-side).
#' * `espn_mlb_game_all()`: Plays, team box, and player box bundled in one call.
#' * `espn_mlb_game_rosters()`: The two game-day rosters.
#' * `espn_mlb_game_probables()`: Probable / announced starting pitchers.
#' * `espn_mlb_game_info()`: Venue, attendance, game duration, and the umpire crew.
#' * `espn_mlb_betting()`, `espn_mlb_game_odds()`, `espn_mlb_game_propbets()`: Odds and prop bets.
#' * `espn_mlb_wp()`, `espn_mlb_game_probabilities()`: Win-probability series.
#' * `espn_mlb_game_situation()`, `espn_mlb_game_play()`, `espn_mlb_game_play_personnel()`: Per-play detail.
#' * `espn_mlb_game_team_statistics()`, `espn_mlb_game_team_linescores()`, `espn_mlb_game_team_leaders()`,
#'   `espn_mlb_game_team_records()`, `espn_mlb_game_team_roster()`, `espn_mlb_game_team_score()`: Per-competitor game detail.
#' * `espn_mlb_game_officials()`, `espn_mlb_game_official_detail()`, `espn_mlb_game_broadcasts()`: Officials and broadcasts.
#'
#' @details
#' ## **ESPN MLB Game**
#'
#' Game-level ESPN endpoints: scoreboard / schedule, the game-summary feed
#' (play-by-play and box scores), rosters, win probability, odds, and
#' per-competitor game detail. Box scores are returned wide and tidy.
#'
#' @keywords ESPN
NULL

#' @name espn_mlb_player_endpoints
#' @aliases espn_mlb_player_endpoints espn_mlb_athlete espn_mlb_athletes_overview
#' @title **ESPN MLB Player / Athlete Endpoint Overview**
#' @description
#'
#' * `espn_mlb_athletes_index()`: Paginated index of MLB athletes.
#' * `espn_mlb_player_info()`: Athlete bio / identity.
#' * `espn_mlb_player_overview()`: Athlete overview (stats, next game, news).
#' * `espn_mlb_player_gamelog()`: Game-by-game log for a season.
#' * `espn_mlb_player_splits()`: Statistical splits.
#' * `espn_mlb_player_stats()`, `espn_mlb_player_stats_v3()`: Season statistics.
#' * `espn_mlb_player_career_stats()`, `espn_mlb_player_seasons()`: Career / season history.
#' * `espn_mlb_player_eventlog()`, `espn_mlb_player_eventlog_v2()`, `espn_mlb_player_statisticslog()`: Event / statistics logs.
#' * `espn_mlb_player_contract()`, `espn_mlb_player_contracts()`: Contract data.
#' * `espn_mlb_player_awards()`: Athlete awards.
#' * `espn_mlb_draft_athletes()`, `espn_mlb_draft_athlete_detail()`: Draft prospects.
#'
#' @details
#' ## **ESPN MLB Players**
#'
#' Athlete-level ESPN endpoints across `sports.core.api.espn.com` and
#' `site.web.api.espn.com`: identity, overviews, game logs, splits, season /
#' career statistics, contracts, and awards.
#'
#' @keywords ESPN
NULL

#' @name espn_mlb_team_endpoints
#' @aliases espn_mlb_team_endpoints espn_mlb_team_overview
#' @title **ESPN MLB Team Endpoint Overview**
#' @description
#'
#' * `espn_mlb_teams()`, `espn_mlb_team()`: Team list and single-team detail.
#' * `espn_mlb_team_roster()`, `espn_mlb_team_current_roster()`: Team rosters.
#' * `espn_mlb_team_schedule()`: Team schedule.
#' * `espn_mlb_team_stats()`, `espn_mlb_team_season_statistics()`: Team statistics.
#' * `espn_mlb_team_record()`, `espn_mlb_team_record_detail()`: Team records.
#' * `espn_mlb_team_leaders()`: Team statistical leaders.
#' * `espn_mlb_team_injuries()`, `espn_mlb_team_news()`: Team injuries and news.
#' * `espn_mlb_team_depthchart()`: Team depth chart.
#' * `espn_mlb_standings()`: League standings.
#' * `espn_mlb_leaders()`: League-wide statistical leaders.
#'
#' @details
#' ## **ESPN MLB Teams**
#'
#' Team- and league-level ESPN endpoints: team lists and detail, rosters,
#' schedules, statistics, records, leaders, injuries, news, and standings.
#'
#' @keywords ESPN
NULL
