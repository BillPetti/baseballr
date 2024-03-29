#' @name fangraphs 
#' @title
#' **FanGraphs Functions Overview**
#' @description
#' \describe{
#'   \item{```fg_pitcher_game_logs()```:}{ Scrape Pitcher Game Logs from FanGraphs.}
#'   \item{```fg_batter_game_logs()```:}{ Scrape Batter Game Logs from FanGraphs.}
#'   \item{```fg_milb_pitcher_game_logs()```:}{ Scrape MiLB game logs for pitchers from Fangraphs, combining 'standard' and 'advanced' tabs.}
#'   \item{```fg_milb_batter_game_logs()```:}{ Scrape MiLB game logs for batters from Fangraphs, combining 'standard' and 'advanced' tabs.}
#'   \item{```fg_batter_leaders()```:}{ Scrape Batter Leaderboards from FanGraphs.}
#'   \item{```fg_pitcher_leaders()```:}{ Scrape Pitcher Leaderboards from FanGraphs.}
#'   \item{```fg_fielder_leaders()```:}{ Scrape Fielder Leaderboards from FanGraphs.}
#'   \item{```fg_team_batter()```:}{ Scrape Team Batter Leaderboards from FanGraphs.}
#'   \item{```fg_team_pitcher()```:}{ Scrape Team Pitcher Leaderboards from FanGraphs.}
#'   \item{```fg_team_fielder()```:}{ Scrape Team Fielder Leaderboards from FanGraphs.}
#'   \item{```fg_guts()```:}{ Scrape FanGraphs.com Guts!.}
#'   \item{```fg_park()```:}{ Scrape Park Factors from FanGraphs.com.}
#'   \item{```fg_park_hand()```:}{ Scrape Park Factors by handedness from FanGraphs.com.}
#' }
#' @details
#' ### **Scrape Pitcher Game Logs from FanGraphs**
#' ```r
#'   fg_pitcher_game_logs(playerid = 104, year = 2006)
#' ```
#' ### **Scrape Batter Game Logs from FanGraphs**
#' ```r
#'   fg_batter_game_logs(playerid = 6184, year = 2017)
#' ```
#' ### **Scrape MiLB game logs for pitchers from Fangraphs**
#' ```r
#'   fg_milb_pitcher_game_logs(playerid = "sa3004210", year=2017)
#' ```
#' ### **Scrape MiLB game logs for batters from Fangraphs**
#' ```r
#'   fg_milb_batter_game_logs(playerid = "sa917940", year=2018)
#' ```
#' ### **Scrape Batter Leaderboards from FanGraphs**
#' ```r
#'   fg_batter_leaders(startseason = 2015, endseason = 2015, qual = 400)
#' ```
#' ### **Scrape Pitcher Leaderboards from FanGraphs**
#' ```r
#'   fg_pitcher_leaders(startseason = 2015, endseason = 2015, qual = 150)
#' ```
#' ### **Scrape Fielder Leaderboards from FanGraphs**
#' ```r
#'   fg_fielder_leaders(startseason = 2015, endseason = 2015, qual = 150)
#' ```
#' ### **Scrape Team Batter Leaderboards from FanGraphs**
#' ```r
#'   fg_team_batter(startseason = 2015, endseason = 2015, qual = 400)
#' ```
#' ### **Scrape Team Pitcher Leaderboards from FanGraphs**
#' ```r
#'   fg_team_pitcher(startseason = 2015, endseason = 2015, qual = 150)
#' ```
#' ### **Scrape Team Fielder Leaderboards from FanGraphs**
#' ```r
#'   fg_team_fielder(startseason = 2015, endseason = 2015, qual = 150)
#' ```
#' ### **Scrape FanGraphs.com Guts!**
#' ```r
#'   fg_guts()
#' ```
#' ### **Scrape Park Factors from FanGraphs.com**
#' ```r
#'   fg_park(2013)
#' ```
#' ### **Scrape Park Factors by handedness from FanGraphs.com**
#' ```r
#'   fg_park_hand(2013)
#' ```
NULL




