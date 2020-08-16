#' Acquire pitch-by-pitch data for Major and Minor League games via the MLB api \url{http://statsapi.mlb.com/api/}
#'
#' @param game_pk The date for which you want to find game_pk values for MLB games
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes over 100 columns of data provided
#' by the MLB Stats API at a pitch level.
#'
#' Some data will vary depending on the
#' park and the league level, as most sensor data is not availble in
#' minor league parks via this API. Note that the column names have mostly
#' been left as-is and there are likely duplicate columns in terms of the
#' information they provide. I plan to clean the output up down the road, but
#' for now I am leaving the majority as-is.
#'
#' Both major and minor league pitch-by-pitch data can be pulled with this
#'  function.
#' @keywords MLB, sabermetrics
#' @export
#'
#' @examples \dontrun{get_pbp_mlb(575156)}

get_pbp_mlb <- function(game_pk) {

  api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/", game_pk, "/feed/live")
  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)

  plays <- payload$liveData$plays$allPlays$playEvents %>% bind_rows()

  at_bats <- payload$liveData$plays$allPlays

  current <- payload$liveData$plays$currentPlay

  game_status <- payload$gameData$status$abstractGameState

  home_team <- payload$gameData$teams$home$name

  home_level <- payload$gameData$teams$home$sport

  home_league <- payload$gameData$teams$home$league

  away_team <- payload$gameData$teams$away$name

  away_level <- payload$gameData$teams$away$sport

  away_league <- payload$gameData$teams$away$league

  list_columns <- lapply(at_bats, function(x) class(x)) %>%
    dplyr::bind_rows(.id = "variable") %>%
    tidyr::gather(key, value) %>%
    dplyr::filter(value == "list") %>%
    dplyr::pull(key)

  at_bats <- at_bats %>%
    dplyr::select(-c(one_of(list_columns)))

  pbp <- plays %>%
    dplyr::left_join(at_bats, by = c("endTime" = "playEndTime"))

  pbp <- pbp %>%
    tidyr::fill(atBatIndex:matchup.splits.menOnBase, .direction = "up") %>%
    dplyr::mutate(game_pk = game_pk,
                  game_date = substr(payload$gameData$datetime$dateTime, 1, 10)) %>%
    dplyr::select(game_pk, game_date, everything())

  pbp <- pbp %>%
    dplyr::mutate(matchup.batter.fullName =
                    factor(matchup.batter.fullName),
                  matchup.pitcher.fullName =
                    factor(matchup.pitcher.fullName),
                  atBatIndex = factor(atBatIndex)
                  # batted.ball.result = case_when(!result.event %in% c(
                  #   "Single", "Double", "Triple", "Home Run") ~ "Out/Other",
                  #   TRUE ~ result.event),
                  # batted.ball.result = factor(batted.ball.result,
                  #                             levels = c("Single", "Double", "Triple", "Home Run", "Out/Other"))
    ) %>%
    dplyr::mutate(home_team = home_team,
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
                  batting_team = factor(ifelse(about.halfInning == "bottom",
                                               home_team,
                                               away_team)),
                  fielding_team = factor(ifelse(about.halfInning == "bottom",
                                                away_team,
                                                home_team)))
  pbp <- pbp %>%
    dplyr::arrange(desc(atBatIndex), desc(pitchNumber))

  pbp <- pbp %>%
    dplyr::group_by(atBatIndex) %>%
    dplyr::mutate(last.pitch.of.ab =
                    ifelse(pitchNumber == max(pitchNumber), "true", "false"),
                  last.pitch.of.ab = factor(last.pitch.of.ab)) %>%
    ungroup()

  pbp <- dplyr::bind_rows(stats_api_live_empty_df, pbp)

  check_home_level <- pbp %>%
    dplyr::distinct(home_level_id) %>%
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
    dplyr::rename(count.balls.start = count.balls.x,
                  count.strikes.start = count.strikes.x,
                  count.outs.start = count.outs.x,
                  count.balls.end = count.balls.y,
                  count.strikes.end = count.strikes.y,
                  count.outs.end = count.outs.y)

  return(pbp)
}
