#' Find game_pk values for professional baseball games (major and minor leagues)
#' via the MLB api \url{http://statsapi.mlb.com/api/}
#'
#' @param date The date for which you want to find game_pk values for MLB games
#' @param level_ids A numeric vector with ids for each level where game_pks are
#' desired. See below for a reference of level ids.
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes game_pk values and additional
#' information for games scheduled or played
#' requested
#' @section Level IDs:
#'
#' The following IDs can be passed to the level_ids argument:
#'
#' 1 = MLB \cr
#' 11 = Triple-A \cr
#' 12 = Doubl-A \cr
#' 13 = Class A Advanced \cr
#' 14 = Class A \cr
#' 15 = Class A Short Season \cr
#' 5442 = Rookie Advanced \cr
#' 16 = Rookie \cr
#' 17 = Winter League \cr
#' @export
#'
#' @examples \dontrun{get_game_pks_mlb("2019-04-29")}

get_game_pks_mlb <- function(date,
                             level_ids = c(1)) {

  api_call <- paste0("http://statsapi.mlb.com/api/v1/schedule?sportId=", paste(level_ids, collapse = ','), "&date=", date)

  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)

  if(payload$totalGames == 0) {
    cols <- c(
      "game_pk", "link", "gameType", "season", "gameDate",
      "officialDate", "isTie", "gameNumber", "publicFacing",
      "doubleHeader", "gamedayType", "tiebreaker",
      "calendarEventID", "seasonDisplay", "dayNight",
      "scheduledInnings", "reverseHomeAwayStatus",
      "inningBreakLength", "gamesInSeries", "seriesGameNumber",
      "seriesDescription", "recordSource", "ifNecessary",
      "ifNecessaryDescription", "status.abstractGameState",
      "status.codedGameState", "status.detailedState",
      "status.statusCode", "status.startTimeTBD",
      "status.abstractGameCode", "teams.away.score",
      "teams.away.isWinner", "teams.away.splitSquad",
      "teams.away.seriesNumber", "teams.away.leagueRecord.wins",
      "teams.away.leagueRecord.losses", "teams.away.leagueRecord.pct",
      "teams.away.team.id", "teams.away.team.name", "teams.away.team.link",
      "teams.home.score", "teams.home.isWinner", "teams.home.splitSquad",
      "teams.home.seriesNumber", "teams.home.leagueRecord.wins",
      "teams.home.leagueRecord.losses", "teams.home.leagueRecord.pct",
      "teams.home.team.id", "teams.home.team.name", "teams.home.team.link",
      "venue.id", "venue.name", "venue.link", "content.link"
    )
    payload <- data.frame(matrix(ncol = length(cols), nrow = 0))
    colnames(payload) <- cols
  } else {
    payload <- payload$dates$games %>%
      as.data.frame() %>%
      rename(game_pk = .data$gamePk)
  }

  return(payload)

}
