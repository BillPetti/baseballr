#' @rdname mlb_game_pks
#' @title **Get MLB Game Info by Date and Level**
#' @description Find game_pk values for professional baseball games (major and minor leagues)
#' via the MLB api \url{https://www.mlb.com/}
#'
#' @param date The date for which you want to find game_pk values for MLB games
#' @param level_ids A numeric vector with ids for each level where game_pks are
#' desired. See below for a reference of level ids.
#' @importFrom jsonlite fromJSON
#' @return Returns a tibble that includes game_pk values and additional
#' information for games scheduled or played with the following columns:
#' 
#'  |col_name                       |types     |
#'  |:------------------------------|:---------|
#'  |game_pk                        |integer   |
#'  |link                           |character |
#'  |gameType                       |character |
#'  |season                         |character |
#'  |gameDate                       |character |
#'  |officialDate                   |character |
#'  |isTie                          |logical   |
#'  |gameNumber                     |integer   |
#'  |publicFacing                   |logical   |
#'  |doubleHeader                   |character |
#'  |gamedayType                    |character |
#'  |tiebreaker                     |character |
#'  |calendarEventID                |character |
#'  |seasonDisplay                  |character |
#'  |dayNight                       |character |
#'  |scheduledInnings               |integer   |
#'  |reverseHomeAwayStatus          |logical   |
#'  |inningBreakLength              |integer   |
#'  |gamesInSeries                  |integer   |
#'  |seriesGameNumber               |integer   |
#'  |seriesDescription              |character |
#'  |recordSource                   |character |
#'  |ifNecessary                    |character |
#'  |ifNecessaryDescription         |character |
#'  |status.abstractGameState       |character |
#'  |status.codedGameState          |character |
#'  |status.detailedState           |character |
#'  |status.statusCode              |character |
#'  |status.startTimeTBD            |logical   |
#'  |status.abstractGameCode        |character |
#'  |teams.away.score               |integer   |
#'  |teams.away.isWinner            |logical   |
#'  |teams.away.splitSquad          |logical   |
#'  |teams.away.seriesNumber        |integer   |
#'  |teams.away.leagueRecord.wins   |integer   |
#'  |teams.away.leagueRecord.losses |integer   |
#'  |teams.away.leagueRecord.pct    |character |
#'  |teams.away.team.id             |integer   |
#'  |teams.away.team.name           |character |
#'  |teams.away.team.link           |character |
#'  |teams.home.score               |integer   |
#'  |teams.home.isWinner            |logical   |
#'  |teams.home.splitSquad          |logical   |
#'  |teams.home.seriesNumber        |integer   |
#'  |teams.home.leagueRecord.wins   |integer   |
#'  |teams.home.leagueRecord.losses |integer   |
#'  |teams.home.leagueRecord.pct    |character |
#'  |teams.home.team.id             |integer   |
#'  |teams.home.team.name           |character |
#'  |teams.home.team.link           |character |
#'  |venue.id                       |integer   |
#'  |venue.name                     |character |
#'  |venue.link                     |character |
#'  |content.link                   |character |
#'  
#' @details Level IDs:
#'
#' The following IDs can be passed to the level_ids argument:
#'
#' 1 = MLB  
#' 11 = Triple-A  
#' 12 = Doubl-A  
#' 13 = Class A Advanced  
#' 14 = Class A  
#' 15 = Class A Short Season  
#' 5442 = Rookie Advanced  
#' 16 = Rookie  
#' 17 = Winter League  
#' @export
#' @examples \donttest{
#'   try(mlb_game_pks("2019-04-29"))
#' }

mlb_game_pks <- function(date,
                         level_ids = c(1)) {
  
  api_call <- paste0("http://statsapi.mlb.com/api/v1/schedule?sportId=", paste(level_ids, collapse = ','), "&date=", date)
  
  tryCatch(
    expr={
      payload <- jsonlite::fromJSON(api_call, flatten = TRUE)
      
      payload <- payload$dates$games %>%
        as.data.frame() %>%
        rename(game_pk = .data$gamePk) %>%
        make_baseballr_data("MLB Game Pks data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(payload)
}

#' @rdname get_game_pks_mlb
#' @title **(legacy) Get MLB Game Info by Date and Level**
#' @inheritParams mlb_game_pks
#' @return Returns a tibble that includes game_pk values and additional
#' information for games scheduled or played
#' @keywords legacy
#' @export
get_game_pks_mlb <- mlb_game_pks
