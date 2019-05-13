#' Retrieve batting orders for a given MLB game via the MLB api \url{http://statsapi.mlb.com/api/}
#'
#' @param game_pk The unique game_pk identifier for the game
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows bind_cols mutate select left_join
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @return Returns a data frame that includes probable starting pitchers and
#' the home plate umpire for the game_pk provided
#' requested
#' @keywords MLB, sabermetrics
#' @export
#'
#' @examples get_batting_orders(566001)

get_batting_orders <- function(game_pk) {

  api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/", game_pk, "/feed/live")
  list <- jsonlite::fromJSON(api_call, flatten = TRUE)

  home_team <- tibble(homeTeam = list$gameData$teams$home$name,
                      homeTeamId = list$gameData$teams$home$id)

  away_team <- tibble(awayTeam = list$gameData$teams$away$name,
                      awayTeamId = list$gameData$teams$away$id)

  home_players <- tibble(playerid = names(list[["liveData"]][["boxscore"]][["teams"]][["home"]][["players"]]))

  away_players <- tibble(playerid = names(list[["liveData"]][["boxscore"]][["teams"]][["away"]][["players"]]))

  players <- function(list, team = "home", playerid) {

    person <- list[["liveData"]][["boxscore"]][["teams"]][[team]][["players"]][[playerid]][["person"]] %>%
      bind_rows()

    position <- list[["liveData"]][["boxscore"]][["teams"]][[team]][["players"]][[playerid]][["position"]] %>%
      bind_rows()

    final_table <- dplyr::bind_cols(person,
                                    position)
  }

  home_players <- home_players %>%
    split(.$playerid) %>%
    purrr::map_df(~players(list = list, team = "home", playerid = .$playerid)) %>%
    dplyr::mutate(team ="home",
           teamName = home_team$homeTeam,
           teamID = home_team$homeTeamId)

  away_players <- away_players %>%
    split(.$playerid) %>%
    purrr:map_df(~players(list = list, team = "away", playerid = .$playerid)) %>%
    dplyr::mutate(team = "away",
           teamName = away_team$awayTeam,
           teamID = away_team$awayTeamId)

  home_batting_order <- tibble(playerid = list[["liveData"]][["boxscore"]][["teams"]][["home"]][["battingOrder"]]) %>%
    dplyr::mutate(order = seq(1,length(.$playerid),1))

  home_batting_order <- home_batting_order %>%
    dplyr::left_join(home_players, by = c("playerid" = "id"))

  away_batting_order <- tibble(playerid = list[["liveData"]][["boxscore"]][["teams"]][["away"]][["battingOrder"]]) %>%
    dplyr::mutate(order = seq(1,length(.$playerid),1))

  away_batting_order <- away_batting_order %>%
    dplyr::left_join(away_players, by = c("playerid" = "id"))

  final_batting_order_table <- dplyr::bind_rows(away_batting_order,
                                         home_batting_order) %>%
    dplyr::select(-c(link, code, name, type))

  final_batting_order_table
}
