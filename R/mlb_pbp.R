#' @rdname mlb_pbp
#' @title **Acquire pitch-by-pitch data for Major and Minor League games**
#'
#' @param game_pk The date for which you want to find game_pk values for MLB games
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes over 100 columns of data provided
#' by the MLB Stats API at a pitch level.
#'
#' Some data will vary depending on the
#' park and the league level, as most sensor data is not available in
#' minor league parks via this API. Note that the column names have mostly
#' been left as-is and there are likely duplicate columns in terms of the
#' information they provide. I plan to clean the output up down the road, but
#' for now I am leaving the majority as-is.
#'
#' Both major and minor league pitch-by-pitch data can be pulled with this
#'  function.
#' @export
#' @examples \donttest{
#'   mlb_pbp(game_pk = 575156)
#' }

mlb_pbp <- function(game_pk) {
  
  api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/", game_pk, "/feed/live")
  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)
  
  plays <- payload$liveData$plays$allPlays$playEvents %>% 
    dplyr::bind_rows()
  
  at_bats <- payload$liveData$plays$allPlays
  
  current <- payload$liveData$plays$currentPlay
  
  game_status <- payload$gameData$status$abstractGameState
  
  home_team <- payload$gameData$teams$home$name
  
  home_level <- payload$gameData$teams$home$sport
  
  home_league <- payload$gameData$teams$home$league
  
  away_team <- payload$gameData$teams$away$name
  
  away_level <- payload$gameData$teams$away$sport
  
  away_league <- payload$gameData$teams$away$league
  
  columns <- lapply(at_bats, function(x) class(x)) %>%
    dplyr::bind_rows(.id = "variable")
  cols <- c(colnames(columns))
  classes <- c(t(unname(columns[1,])))
  
  df <- data.frame(cols, classes)
  list_columns <- df %>%
    dplyr::filter(.data$classes == "list") %>%
    dplyr::pull(.data$cols)
  
  at_bats <- at_bats %>%
    dplyr::select(-c(tidyr::one_of(list_columns)))
  
  pbp <- plays %>%
    dplyr::left_join(at_bats, by = c("endTime" = "playEndTime"))
  
  pbp <- pbp %>%
    tidyr::fill(.data$atBatIndex:.data$matchup.splits.menOnBase, .direction = "up") %>%
    dplyr::mutate(
      game_pk = game_pk,
      game_date = substr(payload$gameData$datetime$dateTime, 1, 10)) %>%
    dplyr::select(.data$game_pk, .data$game_date, tidyr::everything())
  
  pbp <- pbp %>%
    dplyr::mutate(
      matchup.batter.fullName = factor(.data$matchup.batter.fullName),
      matchup.pitcher.fullName = factor(.data$matchup.pitcher.fullName),
      atBatIndex = factor(.data$atBatIndex)
      # batted.ball.result = case_when(!result.event %in% c(
      #   "Single", "Double", "Triple", "Home Run") ~ "Out/Other",
      #   TRUE ~ result.event),
      # batted.ball.result = factor(batted.ball.result,
      #                             levels = c("Single", "Double", "Triple", "Home Run", "Out/Other"))
    ) %>%
    dplyr::mutate(
      home_team = home_team,
      home_level_id = home_level$id,
      home_level_name = home_level$name,
      home_parentOrg_id = payload$gameData$teams$home$parentOrgId,
      home_parentOrg_name = payload$gameData$teams$home$parentOrgName,
      home_league_id = home_league$id,
      home_league_name = home_league$name,
      away_team = away_team,
      away_level_id = away_level$id,
      away_level_name = away_level$name,
      away_parentOrg_id = payload$gameData$teams$away$parentOrgId,
      away_parentOrg_name = payload$gameData$teams$away$parentOrgName,
      away_league_id = away_league$id,
      away_league_name = away_league$name,
      batting_team = factor(ifelse(.data$about.halfInning == "bottom",
                                   .data$home_team,
                                   .data$away_team)),
      fielding_team = factor(ifelse(.data$about.halfInning == "bottom",
                                    .data$away_team,
                                    .data$home_team)))
  pbp <- pbp %>%
    dplyr::arrange(desc(.data$atBatIndex), desc(.data$pitchNumber))
  
  pbp <- pbp %>%
    dplyr::group_by(.data$atBatIndex) %>%
    dplyr::mutate(
      last.pitch.of.ab =  ifelse(.data$pitchNumber == max(.data$pitchNumber), "true", "false"),
      last.pitch.of.ab = factor(.data$last.pitch.of.ab)) %>%
    dplyr::ungroup()
  
  pbp <- dplyr::bind_rows(baseballr::stats_api_live_empty_df, pbp)
  
  check_home_level <- pbp %>%
    dplyr::distinct(.data$home_level_id) %>%
    dplyr::pull()
  
  # this will need to be updated in the future to properly estimate X,Z coordinates at the minor league level
  
  # if(check_home_level != 1) {
  #
  #   pbp <- pbp %>%
  #     dplyr::mutate(pitchData.coordinates.x = -pitchData.coordinates.x,
  #                   pitchData.coordinates.y = -pitchData.coordinates.y)
  #
  #   pbp <- pbp %>%
  #     dplyr::mutate(pitchData.coordinates.pX_est = predict(x_model, pbp),
  #                   pitchData.coordinates.pZ_est = predict(y_model, pbp))
  #
  #   pbp <- pbp %>%
  #     dplyr::mutate(pitchData.coordinates.x = -pitchData.coordinates.x,
  #                   pitchData.coordinates.y = -pitchData.coordinates.y)
  # }
  
  pbp <- pbp %>%
    dplyr::rename(
      count.balls.start = .data$count.balls.x,
      count.strikes.start = .data$count.strikes.x,
      count.outs.start = .data$count.outs.x,
      count.balls.end = .data$count.balls.y,
      count.strikes.end = .data$count.strikes.y,
      count.outs.end = .data$count.outs.y)
  
  return(pbp)
}

#' @rdname mlb_pbp
#' @export
get_pbp_mlb <- mlb_pbp