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
#'  |col_name                       |types     |description                                       |
#'  |:------------------------------|:---------|:-------------------------------------------------|
#'  |game_pk                        |integer   |Unique game identifier.                           |
#'  |gameGuid                       |character |Globally unique game identifier.                  |
#'  |link                           |character |MLB Stats API relative game link.                 |
#'  |gameType                       |character |Game type code (R, P, etc.).                      |
#'  |season                         |character |Season (YYYY).                                    |
#'  |gameDate                       |character |Game date-time (ISO 8601, UTC).                   |
#'  |officialDate                   |character |Official game date (YYYY-MM-DD).                  |
#'  |isTie                          |logical   |Whether the game ended in a tie.                  |
#'  |gameNumber                     |integer   |Game number within a doubleheader.                |
#'  |publicFacing                   |logical   |Whether the game is public-facing.                |
#'  |doubleHeader                   |character |Doubleheader indicator (N/Y/S).                   |
#'  |gamedayType                    |character |Gameday data type code.                           |
#'  |tiebreaker                     |character |Tiebreaker indicator.                             |
#'  |calendarEventID                |character |Calendar event identifier.                        |
#'  |seasonDisplay                  |character |Display season (YYYY).                            |
#'  |dayNight                       |character |Day/night designation.                            |
#'  |scheduledInnings               |integer   |Number of scheduled innings.                      |
#'  |reverseHomeAwayStatus          |logical   |Whether home/away designation is reversed.        |
#'  |inningBreakLength              |integer   |Length of the inning break (seconds).             |
#'  |gamesInSeries                  |integer   |Total games in the series.                        |
#'  |seriesGameNumber               |integer   |Game number within the series.                    |
#'  |seriesDescription              |character |Series description.                               |
#'  |recordSource                   |character |Source of the record data.                        |
#'  |ifNecessary                    |character |Whether the game is played only if necessary.     |
#'  |ifNecessaryDescription         |character |If-necessary description.                         |
#'  |status.abstractGameState       |character |Abstract game state (e.g. Final).                 |
#'  |status.codedGameState          |character |Coded game state.                                 |
#'  |status.detailedState           |character |Detailed game state.                              |
#'  |status.statusCode              |character |Game status code.                                 |
#'  |status.startTimeTBD            |logical   |Whether the start time is TBD.                    |
#'  |status.abstractGameCode        |character |Abstract game code.                               |
#'  |teams.away.score               |integer   |Away team score.                                  |
#'  |teams.away.isWinner            |logical   |Whether the away team won.                        |
#'  |teams.away.splitSquad          |logical   |Whether the away team is a split squad.           |
#'  |teams.away.seriesNumber        |integer   |Away team series number.                          |
#'  |teams.away.team.id             |integer   |Away team MLB ID.                                 |
#'  |teams.away.team.name           |character |Away team name.                                   |
#'  |teams.away.team.link           |character |MLB Stats API relative away team link.            |
#'  |teams.away.leagueRecord.wins   |integer   |Away team league-record wins.                     |
#'  |teams.away.leagueRecord.losses |integer   |Away team league-record losses.                   |
#'  |teams.away.leagueRecord.ties   |integer   |Away team league-record ties.                     |
#'  |teams.away.leagueRecord.pct    |character |Away team winning percentage.                     |
#'  |teams.home.score               |integer   |Home team score.                                  |
#'  |teams.home.isWinner            |logical   |Whether the home team won.                        |
#'  |teams.home.splitSquad          |logical   |Whether the home team is a split squad.           |
#'  |teams.home.seriesNumber        |integer   |Home team series number.                          |
#'  |teams.home.team.id             |integer   |Home team MLB ID.                                 |
#'  |teams.home.team.name           |character |Home team name.                                   |
#'  |teams.home.team.link           |character |MLB Stats API relative home team link.            |
#'  |teams.home.leagueRecord.wins   |integer   |Home team league-record wins.                     |
#'  |teams.home.leagueRecord.losses |integer   |Home team league-record losses.                   |
#'  |teams.home.leagueRecord.ties   |integer   |Home team league-record ties.                     |
#'  |teams.home.leagueRecord.pct    |character |Home team winning percentage.                     |
#'  |venue.id                       |integer   |Venue ID.                                         |
#'  |venue.name                     |character |Venue name.                                       |
#'  |venue.link                     |character |MLB Stats API relative venue link.                |
#'  |content.link                   |character |MLB Stats API relative game content link.         |
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
  
  payload <- NULL
  tryCatch(
    expr = {
      payload <- jsonlite::fromJSON(api_call, flatten = TRUE)
      
      payload <- payload$dates$games |>
        as.data.frame() |>
        rename("game_pk" = "gamePk") |>
        make_baseballr_data("MLB Game Pks data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
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
