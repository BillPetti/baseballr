#' Find game_pk values for MLB games via the MLB api \url{http://statsapi.mlb.com/api/}
#'
#' @param date The date for which you want to find game_pk values for MLB games
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes game_pk values and additional
#' information for games scheduled or played
#' requested
#' @keywords MLB, sabermetrics
#' @export
#'
#' @examples get_game_pks("2019-04-29")

get_game_pks_mlb <- function(date) {

  api_call <- paste0("http://statsapi.mlb.com/api/v1/schedule?sportId=1&date=", date)

  payload <- jsonlite::fromJSON(api_call)

  payload <- payload$dates$games %>%
    as.data.frame()

  return(payload)

}
