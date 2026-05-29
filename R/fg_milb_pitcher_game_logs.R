#' @rdname fg_milb_pitcher_game_logs
#' @title **Scrape MiLB game logs for pitchers from FanGraphs**
#'
#' @description This function allows you to scrape MiLB game logs for individual batters from FanGraphs.com.
#' @param playerid The pitcher's minor league ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' @return Returns a tibble of Minor League pitcher game logs, one row per game, with the following columns:
#'
#'    |col_name |types     |description                                              |
#'    |:--------|:---------|:--------------------------------------------------------|
#'    |Date     |character |Calendar date of the game (YYYY-MM-DD).                  |
#'    |Team     |character |Minor league team the pitcher played for.                |
#'    |Level    |character |Minor league classification level (e.g. (AA), (AAA)).    |
#'    |Opp      |character |Opponent team; leading @ indicates a road game.          |
#'    |W        |numeric   |Wins.                                                    |
#'    |L        |numeric   |Losses.                                                  |
#'    |ERA      |numeric   |Earned run average (per 9 innings).                      |
#'    |G        |numeric   |Games pitched (1 per row).                               |
#'    |GS       |numeric   |Games started.                                           |
#'    |QS       |numeric   |Quality starts (6+ IP, 3 or fewer earned runs).          |
#'    |CG       |numeric   |Complete games.                                          |
#'    |ShO      |numeric   |Shutouts.                                                |
#'    |SV       |numeric   |Saves.                                                   |
#'    |IP       |numeric   |Innings pitched.                                         |
#'    |TBF      |numeric   |Total batters faced.                                     |
#'    |H        |numeric   |Hits allowed.                                            |
#'    |R        |numeric   |Runs allowed.                                            |
#'    |ER       |numeric   |Earned runs allowed.                                     |
#'    |HR       |numeric   |Home runs allowed.                                       |
#'    |BB       |numeric   |Walks allowed.                                           |
#'    |IBB      |numeric   |Intentional walks allowed.                               |
#'    |HBP      |numeric   |Batters hit by pitch.                                    |
#'    |WP       |numeric   |Wild pitches.                                            |
#'    |BK       |numeric   |Balks.                                                   |
#'    |SO       |numeric   |Strikeouts.                                              |
#'    |K/9      |numeric   |Strikeouts per 9 innings.                                |
#'    |BB/9     |numeric   |Walks per 9 innings.                                     |
#'    |K/BB     |numeric   |Strikeout-to-walk ratio.                                 |
#'    |HR/9     |numeric   |Home runs allowed per 9 innings.                         |
#'    |K%       |numeric   |Strikeout rate (per batter faced).                       |
#'    |K-BB%    |numeric   |Strikeout rate minus walk rate.                          |
#'    |BB%      |numeric   |Walk rate (per batter faced).                            |
#'    |AVG      |numeric   |Opponent batting average allowed.                        |
#'    |WHIP     |numeric   |Walks plus hits per inning pitched.                      |
#'    |BABIP    |numeric   |Batting average on balls in play allowed.                |
#'    |LOB%     |numeric   |Left-on-base percentage (strand rate).                   |
#'    |FIP      |numeric   |Fielding independent pitching.                           |
#'    |ERA-     |numeric   |ERA scaled to league/park (100 = average, lower better). |
#'    |FIP-     |numeric   |FIP scaled to league/park (100 = average, lower better). |
#'    |xFIP-    |numeric   |Expected FIP scaled to league/park (100 = average).      |
#'    |gamedate |character |Game date as parsed from the source feed.                |
#'    |dh       |integer   |Doubleheader game indicator (0 = single game).           |
#'
#' @import rvest 
#' @importFrom tidyr separate
#' @export
#' @examples \donttest{
#'   try(fg_milb_pitcher_game_logs(playerid = "sa3020682", year=2023))
#' }

fg_milb_pitcher_game_logs <- function(playerid, year) {
  payload <- NULL
  tryCatch(
    expr = {
      # CDN API game-log
      url <- paste0("https://www.fangraphs.com/api/players/game-log?position=P&type=-1&&gds=&gde=&z=1703085978&playerid=",
                    playerid,
                    "&season=",
                    year)
      
      res <- httr2::request(url) |> httr2::req_retry(max_tries = 3) |> httr2::req_perform()
      
      resp <- res |> 
        httr2::resp_body_string()
      
      payload <- jsonlite::fromJSON(resp)[['minor']] |> 
        as.data.frame()
      # remove averages/totals column
      payload <- payload[-1,]
      # separate Team column into Team & MiLB level
      suppressWarnings(
        payload <- payload |>
          tidyr::separate("Team", into = c("Team","Level"),sep = " ")
      )
      
      # url for player info table
      url_basic <- paste0("https://www.fangraphs.com/api/players/stats?playerid=",
                          playerid,
                          "&position=P&z=1703085978")
      
      stats_res <- httr2::request(url_basic) |> httr2::req_retry(max_tries = 3) |> httr2::req_perform()
      
      stats_resp <- stats_res |> 
        httr2::resp_body_string()
      
      team_data <- stats_resp |>
        jsonlite::fromJSON(flatten = TRUE) |> 
        purrr::pluck("teamInfo")
      team_df <- t(do.call(rbind, team_data)) |> 
        as.data.frame()
      team_payload <- team_df |> 
        dplyr::pull("masterid")
      
      url_player <- paste0("https://www.fangraphs.com/api/players/stats?playerid=",
                           team_payload,
                           "&position=P&z=1703085978")
      player_res <- httr2::request(url_player) |> httr2::req_retry(max_tries = 3) |> httr2::req_perform()
      
      player_resp <- player_res |> 
        httr2::resp_body_string()
      
      player_data <- player_resp |>
        jsonlite::fromJSON(flatten = TRUE) |> 
        purrr::pluck("playerInfo")
      player_df <- t(do.call(rbind, player_data)) |> 
        as.data.frame()
      
      payload <- payload |> 
        dplyr::bind_cols(player_df)
      
      # add playerid to payload
      payload <- payload |>
        dplyr::mutate(
          player_name = .data$firstLastName,
          minor_playerid = .data$MinorMasterId) |>
        dplyr::select("player_name", "minor_playerid", tidyr::everything())
      payload <- payload |>
        make_baseballr_data("MiLB Pitcher Game Logs data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no MiLB pitcher game logs data available!")
    },
    finally = {
    }
  )
  return(payload)
}  


#' @rdname milb_pitcher_game_logs_fg
#' @title **(legacy) Scrape MiLB game logs for pitchers from FanGraphs**
#' @inheritParams fg_milb_pitcher_game_logs
#' @return Returns a tibble of Minor League pitcher game logs.
#' @keywords legacy
#' @export
milb_pitcher_game_logs_fg <-  fg_milb_pitcher_game_logs
