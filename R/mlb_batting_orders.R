#' @rdname mlb_batting_orders
#' @title **Retrieve batting orders for a given MLB game**
#' @param game_pk The unique game_pk identifier for the game
#' @param type Whether to just return the starting lineup ('starting') or all
#' batters that appeared ('all')
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @return Returns a data frame that includes probable starting pitchers and
#' the home plate umpire for the `game_pk` requested
#' @export
#' @examples \donttest{
#'   mlb_batting_orders(game_pk=566001)
#' }

mlb_batting_orders <- function (game_pk,
                                type = "starting") {

  api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/",
                     game_pk, "/feed/live")
  list <- jsonlite::fromJSON(api_call, flatten = TRUE)
  home_team <- tibble::tibble(
    homeTeam = list$gameData$teams$home$name,
    homeTeamId = list$gameData$teams$home$id)
  
  away_team <- tibble::tibble(
    awayTeam = list$gameData$teams$away$name,
    awayTeamId = list$gameData$teams$away$id)
  
  home_players <- tibble::tibble(
    playerid = names(list[["liveData"]][["boxscore"]][["teams"]][["home"]][["players"]]))

  away_players <- tibble::tibble(
    playerid = names(list[["liveData"]][["boxscore"]][["teams"]][["away"]][["players"]]))

  home_players <- unique(home_players$playerid)
  home_players <- purrr::map_df(home_players, function(x){
    helper_players(list = list, team = "home", playerid = x)
  })
  
  home_players <- home_players %>%
    dplyr::mutate(
      batting_order = as.character(.data$batting_order),
      batting_position_num = as.character(.data$batting_position_num))
  

  home_players <- dplyr::bind_rows(home_players) %>%
    dplyr::mutate(
      team = "home",
      teamName = home_team$homeTeam,
      teamID = home_team$homeTeamId) %>%
    dplyr::arrange(.data$batting_order)
  away_players <- unique(away_players$playerid)
  away_players <- purrr::map_df(away_players, function(x){
    helper_players(list = list, team = "away", playerid = x)
  })

  away_players <- away_players %>%
    dplyr::mutate(
      batting_order = as.character(.data$batting_order),
      batting_position_num = as.character(.data$batting_position_num))

  away_players <- dplyr::bind_rows(away_players) %>%
    dplyr::mutate(
      team = "away",
      teamName = away_team$awayTeam,
      teamID = away_team$awayTeamId) %>%
    dplyr::arrange(.data$batting_order)
  
  final_batting_order_table <- dplyr::bind_rows(away_players, home_players) %>%
    dplyr::select(-.data$link, -.data$code, -.data$name, -.data$type) %>%
    dplyr::arrange(.data$team, .data$batting_order, .data$batting_position_num) %>%
    dplyr::filter(!is.na(.data$batting_order))

  if (type == "starting") {
    final_batting_order_table <- final_batting_order_table %>%
      dplyr::filter(.data$batting_position_num == 0)
  }

  return(final_batting_order_table)
}

#' @rdname mlb_batting_orders
#' @export
get_batting_orders <- mlb_batting_orders



helper_players <- function(list, team = "home", playerid) {
  person <- list[["liveData"]][["boxscore"]][["teams"]][[team]][["players"]][[playerid]][["person"]] %>%
    dplyr::bind_rows()
  position <- list[["liveData"]][["boxscore"]][["teams"]][[team]][["players"]][[playerid]][["position"]] %>%
    dplyr::bind_rows()
  batting_position <- list[["liveData"]][["boxscore"]][["teams"]][[team]][["players"]][[playerid]][["battingOrder"]]
  final_table <- bind_cols(person, position) 
  final_table <- final_table %>% 
    dplyr::mutate(
      batting_order = ifelse(is.null(batting_position),
                             NA, substr(batting_position, 1, 1)), 
      batting_position_num = ifelse(is.null(batting_position),
                                    NA, as.numeric(substr(batting_position, 2, 3))))
  return(final_table)
}