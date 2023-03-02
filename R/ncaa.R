#' @name ncaa
#' @title
#' **NCAA Functions Overview**
#' @description
#' \describe{
#'   \item{```ncaa_team_player_stats()```:}{ This function allows the user to obtain batting or pitching statistics for any school affiliated with the NCAA at the division I, II, or III levels. The function acquires data from the NCAA's website (stats.ncaa.org) and returns a tibble.}
#'   \item{```ncaa_pbp()```:}{ Get Play-By-Play Data for NCAA Baseball Games.}
#'   \item{```ncaa_roster()```:}{ Get NCAA Baseball Rosters.}
#'   \item{```ncaa_game_logs()```:}{ Get NCAA Baseball Game Logs.}
#'   \item{```ncaa_lineups()```:}{ Get NCAA Baseball Game Lineups.}
#'   \item{```ncaa_park_factor()```:}{ Get Park Effects for NCAA Baseball Teams.}
#'   \item{```ncaa_schedule_info()```:}{ Get Schedule and Results for NCAA Baseball Teams.}
#'   \item{```ncaa_school_id_lu()```:}{Lookup NCAA School IDs (Division I, II, and III)}
#'   \item{```ncaa_teams()```:}{Lookup NCAA Teams by Division (I, II, and III) and Season}
#' }
#' @details
#' ### **Scrape NCAA baseball data (Division I, II, and III)**
#' ```r
#'   ncaa_team_player_stats(team_id = 255, year = 2013, type = "batting")
#' ```
#' ### **Get Play-By-Play Data for NCAA Baseball Games**
#' ```r
#'   x <- ncaa_schedule_info(736, 2021)$game_info_url[2]
#'   ncaa_pbp(game_info_url = x)
#' ```
#' ### **Get NCAA Baseball Rosters**
#' ```r
#'   ncaa_roster(team_id = 104, year = 2021)
#' ```
#' ### **Get NCAA Baseball Game Logs**
#' ```r
#'   ncaa_game_logs(player_id = 2113782, year = 2021, type = "pitching", span = "game")
#' ```
#' ### **Get NCAA Baseball Game Lineups**
#' ```r
#'   ncaa_lineups(game_info_url="https://stats.ncaa.org/game/index/4587474?org_id=528",year=2018)
#' ```
#' ### **Get Park Effects for NCAA Baseball Teams**
#' ```r
#'    ncaa_park_factor(team_id = 736, years = c(2017:2019), type = "conference")
#' ```
#' ### **Get Schedule and Results for NCAA Baseball Teams**
#' ```r
#'   ncaa_schedule_info(team_id = 736, year = 2021)
#' ```
#' ### **Lookup NCAA School IDs (Division I, II, and III)**
#' ```r
#'   ncaa_school_id_lu("VAN")
#' ```
#' ### **Scrape NCAA baseball Teams (Division I, II, and III)**
#' ```r
#'   ncaa_teams(year = 2023, division = 1)
#' ```
#'
NULL
