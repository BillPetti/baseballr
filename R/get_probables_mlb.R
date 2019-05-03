#' Retrieve probable starters for a given MLB game via the MLB api \url{http://statsapi.mlb.com/api/}
#'
#' @param game_pk The unique game_pk identifier for the game
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows bind_cols mutate select
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @return Returns a data frame that includes probable starting pitchers for
#' the game_pk provided
#' requested
#' @keywords MLB, sabermetrics
#' @export
#'
#' @examples get_probables_mlb(566001)

get_probables_mlb <- function(game_pk) {
  oldw <- getOption("warn")
  options(warn = -1)
  api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/", game_pk,"/feed/live")
  payload <- jsonlite::fromJSON(api_call)
  away_probable <- if(is.null(payload$gameData$probablePitchers$away)) {
    return_table <- tibble(id = NA,
                           fullName = NA,
                           link = NA)
  } else {
    return_table <- payload$gameData$probablePitchers$away %>%
      as.data.frame()
  }
  home_probable <- if(is.null(payload$gameData$probablePitchers$home)) {
    return_table <- tibble(id = NA,
                           fullName = NA,
                           link = NA)
  } else {
    return_table <- payload$gameData$probablePitchers$home %>%
      as.data.frame()
  }
  parse_teams <- function(payload_teams) {
    return_table <- tibble(team = payload_teams$name,
                           team_id = payload_teams$id)
    return(return_table)
  }

  away_team <- parse_teams(payload$gameData$teams$away)
  home_team <- parse_teams(payload$gameData$teams$home)

  teams <- dplyr::bind_rows(away_team,
                            home_team)

  probs <- dplyr::bind_rows(away_probable,
                            home_probable)

  table <- dplyr::bind_cols(probs,
                            teams)

  table <- table %>%
    mutate(game_pk = payload$gamePk,
           game_date = stringr::str_sub(payload$gameData$game$calendarEventID,
                                        -10,
                                        -1)) %>%
    select(game_pk, game_date, fullName, id, team, team_id)

  options(warn = oldw)

  return(table)
}
