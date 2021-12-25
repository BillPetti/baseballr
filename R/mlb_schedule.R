#' @title **Find game_pk values for professional baseball games (major and minor leagues)**
#'
#' @param season The season for which you want to find game_pk values for MLB games
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
#' @examples \donttest{
#'   mlb_schedule(season = "2019")
#' }

mlb_schedule <- function(season = 2019, level_ids = '1'){

  api_call <- paste0("http://statsapi.mlb.com/api/v1/schedule?language=en",
                     "&sportId=", level_ids, 
                     "&season=", season)

  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)

  games <- payload$dates %>% 
    tidyr::unnest(.data$games) %>%
    as.data.frame() %>%
    janitor::clean_names()

  return(games)

}
