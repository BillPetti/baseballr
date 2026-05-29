#' @rdname fg_milb_batter_game_logs
#' @title **Scrape MiLB game logs for batters from FanGraphs**
#' @description This function allows you to scrape MiLB game logs for individual batters from FanGraphs.
#' @param playerid The batter's minor league ID from FanGraphs.
#' @param year The season for which game logs should be returned.
#' @return Returns a tibble of Minor League batter game logs, one row per game, with the following columns:
#'
#'    |col_name |types     |description                                              |
#'    |:--------|:---------|:--------------------------------------------------------|
#'    |Date     |character |Calendar date of the game (YYYY-MM-DD).                  |
#'    |Team     |character |Minor league team the batter played for.                 |
#'    |Level    |character |Minor league classification level (e.g. (AA), (AAA)).    |
#'    |Opp      |character |Opponent team; leading @ indicates a road game.          |
#'    |G        |numeric   |Games played (1 per row).                                |
#'    |AB       |numeric   |At-bats.                                                 |
#'    |PA       |numeric   |Plate appearances.                                       |
#'    |H        |numeric   |Hits.                                                    |
#'    |1B       |numeric   |Singles.                                                 |
#'    |2B       |numeric   |Doubles.                                                 |
#'    |3B       |numeric   |Triples.                                                 |
#'    |HR       |numeric   |Home runs.                                               |
#'    |R        |numeric   |Runs scored.                                             |
#'    |RBI      |numeric   |Runs batted in.                                          |
#'    |BB       |numeric   |Walks (bases on balls).                                  |
#'    |IBB      |numeric   |Intentional walks.                                       |
#'    |SO       |numeric   |Strikeouts.                                              |
#'    |HBP      |numeric   |Times hit by pitch.                                      |
#'    |SF       |numeric   |Sacrifice flies.                                         |
#'    |SH       |numeric   |Sacrifice hits (bunts).                                  |
#'    |GDP      |numeric   |Grounded into double plays.                              |
#'    |SB       |numeric   |Stolen bases.                                            |
#'    |CS       |numeric   |Times caught stealing.                                   |
#'    |AVG      |numeric   |Batting average (H/AB).                                  |
#'    |BB%      |numeric   |Walk rate (BB per plate appearance).                     |
#'    |K%       |numeric   |Strikeout rate (SO per plate appearance).                |
#'    |BB/K     |numeric   |Walk-to-strikeout ratio.                                 |
#'    |OBP      |numeric   |On-base percentage.                                      |
#'    |SLG      |numeric   |Slugging percentage.                                     |
#'    |OPS      |numeric   |On-base plus slugging.                                   |
#'    |ISO      |numeric   |Isolated power (SLG minus AVG).                          |
#'    |Spd      |numeric   |Bill James Speed Score.                                  |
#'    |BABIP    |numeric   |Batting average on balls in play.                        |
#'    |wRC      |numeric   |Weighted runs created.                                   |
#'    |wRAA     |numeric   |Weighted runs above average.                             |
#'    |wOBA     |numeric   |Weighted on-base average.                                |
#'    |wRC+     |numeric   |Weighted runs created plus (league/park adjusted, 100=avg). |
#'    |wBsR     |numeric   |Weighted base running runs.                              |
#'    |gamedate |character |Game date as parsed from the source feed.                |
#'    |dh       |integer   |Doubleheader game indicator (0 = single game).           |
#'
#' @importFrom tidyr separate everything
#' @importFrom dplyr mutate select
#' @importFrom jsonlite fromJSON
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(fg_milb_batter_game_logs(playerid = "sa3019999", year=2023))
#' }

fg_milb_batter_game_logs <- function(playerid, year) {
  payload <- NULL
  tryCatch(
    expr = {
      
      # API game-log
      url <- paste0("https://www.fangraphs.com/api/players/game-log?position=&type=-1&gds=&gde=&z=1703085978&playerid=",
                    playerid,
                    "&season=",
                    year)
      
      res <- httr::RETRY("GET", url)
      
      resp <- res |> 
        httr::content(as = "text", encoding = "UTF-8")
      
      payload <- jsonlite::fromJSON(resp)[['minor']] |> 
        as.data.frame()
      # remove averages/totals column
      payload <- payload[-1,]
      # separate Team column into Team & MiLB level
      suppressWarnings(
        payload <- payload |>
          tidyr::separate("Team", into = c("Team","Level"), sep = " ")
      )
      # url for player info table
      url_basic <- paste0("https://www.fangraphs.com/api/players/stats?playerid=",
                          playerid,
                          "&position=&z=1703085978")
      
      stats_res <- httr::RETRY("GET", url_basic)
      
      stats_resp <- stats_res |> 
        httr::content(as = "text", encoding = "UTF-8")
      
      team_data <- stats_resp |>
        jsonlite::fromJSON(flatten = TRUE) |> 
        purrr::pluck("teamInfo")
      
      team_df <- t(do.call(rbind, team_data)) |> 
        as.data.frame()
      
      team_payload <- team_df |> 
        dplyr::pull("masterid")
      
      url_player <- paste0("https://www.fangraphs.com/api/players/stats?playerid=",
                           team_payload,
                           "&position=&z=1703085978")
      player_res <- httr::RETRY("GET", url_player)
      
      player_resp <- player_res |> 
        httr::content(as = "text", encoding = "UTF-8")
      
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
        make_baseballr_data("MiLB Batter Game Logs data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no MiLB batter game logs data available!")
    },
    finally = {
    }
  )
  return(payload)
}  


#' @rdname milb_batter_game_logs_fg
#' @title **(legacy) Scrape MiLB game logs for batters from FanGraphs**
#' @inheritParams fg_milb_batter_game_logs
#' @return Returns a tibble of Minor League batter game logs.
#' @keywords legacy
#' @export

milb_batter_game_logs_fg <- fg_milb_batter_game_logs
