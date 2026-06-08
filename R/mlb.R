#' @name mlb
#' @title
#' **MLB Functions Overview**
#' @description
#'
#' * `mlb_batting_orders()`: Retrieve batting orders for a given MLB game.
#' * `mlb_draft()`: Retrieve draft pick information by year.
#' * `mlb_pbp()`: Acquire pitch-by-pitch data for Major and Minor League games.
#' * `mlb_game_info()`: Retrieve additional game information for major and minor league games.
#' * `mlb_game_pks()`: Get MLB Game Info by Date and Level.
#' * `mlb_schedule()`: Find game_pk values for professional baseball games (major and minor leagues).
#' * `mlb_probables()`: Retrieve probable starters for a given MLB game.
#'
#' @details
#' ### **Retrieve batting orders for a given MLB game**
#' ```r
#'   mlb_batting_orders(game_pk=566001)
#' ```
#' ### **Retrieve draft pick information by year**
#' ```r
#'   mlb_draft(year= 2018)
#' ```
#' ### **Acquire pitch-by-pitch data for Major and Minor League games**
#' ```r
#'   mlb_pbp(game_pk = 575156)
#' ```
#' ### **Retrieve additional game information for major and minor league games**
#' ```r
#'   mlb_game_info(game_pk = 566001)
#' ```
#' ### **Get MLB Game Info by Date and Level**
#' ```r
#'   mlb_game_pks("2019-04-29")
#' ```
#' ### **Find game_pk values for professional baseball games (major and minor leagues)**
#' ```r
#'   mlb_schedule(season = "2019")
#' ```
#' ### **Retrieve probable starters for a given MLB game**
#' ```r
#'   mlb_probables(566001)
#' ```
NULL
