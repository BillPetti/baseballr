#' @rdname mlb_pbp_diff
#' @title **Acquire pitch-by-pitch data between two timecodes for Major and Minor League games**
#'
#' @param game_pk The date for which you want to find game_pk values for MLB games
#' @param start_timecode The start time code for the MLB game (format: MMDDYYYY_HHMMSS)
#' @param end_timecode The end time code for the MLB game (format: MMDDYYYY_HHMMSS)
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes over 100 columns of data provided
#' by the MLB Stats API at a pitch level between the start_timecode and end_timecode
#' 
#'  |col_name                       |types     |
#'  |:------------------------------|:---------|
#'  |game_pk                        |numeric   |
#'  |game_date                      |character |
#'  |index                          |integer   |
#'  |startTime                      |character |
#'  |endTime                        |character |
#'  |isPitch                        |logical   |
#'  |type                           |character |
#'  |playId                         |character |
#'  |pitchNumber                    |integer   |
#'  |details.description            |character |
#'  |details.event                  |character |
#'  |details.awayScore              |integer   |
#'  |details.homeScore              |integer   |
#'  |details.isScoringPlay          |logical   |
#'  |details.hasReview              |logical   |
#'  |details.code                   |character |
#'  |details.ballColor              |character |
#'  |details.isInPlay               |logical   |
#'  |details.isStrike               |logical   |
#'  |details.isBall                 |logical   |
#'  |details.call.code              |character |
#'  |details.call.description       |character |
#'  |count.balls.start              |integer   |
#'  |count.strikes.start            |integer   |
#'  |count.outs.start               |integer   |
#'  |player.id                      |integer   |
#'  |player.link                    |character |
#'  |pitchData.strikeZoneTop        |numeric   |
#'  |pitchData.strikeZoneBottom     |numeric   |
#'  |details.fromCatcher            |logical   |
#'  |pitchData.coordinates.x        |numeric   |
#'  |pitchData.coordinates.y        |numeric   |
#'  |hitData.trajectory             |character |
#'  |hitData.hardness               |character |
#'  |hitData.location               |character |
#'  |hitData.coordinates.coordX     |numeric   |
#'  |hitData.coordinates.coordY     |numeric   |
#'  |actionPlayId                   |character |
#'  |details.eventType              |character |
#'  |details.runnerGoing            |logical   |
#'  |position.code                  |character |
#'  |position.name                  |character |
#'  |position.type                  |character |
#'  |position.abbreviation          |character |
#'  |battingOrder                   |character |
#'  |atBatIndex                     |character |
#'  |result.type                    |character |
#'  |result.event                   |character |
#'  |result.eventType               |character |
#'  |result.description             |character |
#'  |result.rbi                     |integer   |
#'  |result.awayScore               |integer   |
#'  |result.homeScore               |integer   |
#'  |about.atBatIndex               |integer   |
#'  |about.halfInning               |character |
#'  |about.inning                   |integer   |
#'  |about.startTime                |character |
#'  |about.endTime                  |character |
#'  |about.isComplete               |logical   |
#'  |about.isScoringPlay            |logical   |
#'  |about.hasReview                |logical   |
#'  |about.hasOut                   |logical   |
#'  |about.captivatingIndex         |integer   |
#'  |count.balls.end                |integer   |
#'  |count.strikes.end              |integer   |
#'  |count.outs.end                 |integer   |
#'  |matchup.batter.id              |integer   |
#'  |matchup.batter.fullName        |character |
#'  |matchup.batter.link            |character |
#'  |matchup.batSide.code           |character |
#'  |matchup.batSide.description    |character |
#'  |matchup.pitcher.id             |integer   |
#'  |matchup.pitcher.fullName       |character |
#'  |matchup.pitcher.link           |character |
#'  |matchup.pitchHand.code         |character |
#'  |matchup.pitchHand.description  |character |
#'  |matchup.splits.batter          |character |
#'  |matchup.splits.pitcher         |character |
#'  |matchup.splits.menOnBase       |character |
#'  |batted.ball.result             |factor    |
#'  |home_team                      |character |
#'  |home_level_id                  |integer   |
#'  |home_level_name                |character |
#'  |home_parentOrg_id              |integer   |
#'  |home_parentOrg_name            |character |
#'  |home_league_id                 |integer   |
#'  |home_league_name               |character |
#'  |away_team                      |character |
#'  |away_level_id                  |integer   |
#'  |away_level_name                |character |
#'  |away_parentOrg_id              |integer   |
#'  |away_parentOrg_name            |character |
#'  |away_league_id                 |integer   |
#'  |away_league_name               |character |
#'  |batting_team                   |character |
#'  |fielding_team                  |character |
#'  |last.pitch.of.ab               |character |
#'  |pfxId                          |character |
#'  |details.trailColor             |character |
#'  |details.type.code              |character |
#'  |details.type.description       |character |
#'  |pitchData.startSpeed           |numeric   |
#'  |pitchData.endSpeed             |numeric   |
#'  |pitchData.zone                 |integer   |
#'  |pitchData.typeConfidence       |numeric   |
#'  |pitchData.plateTime            |numeric   |
#'  |pitchData.extension            |numeric   |
#'  |pitchData.coordinates.aY       |numeric   |
#'  |pitchData.coordinates.aZ       |numeric   |
#'  |pitchData.coordinates.pfxX     |numeric   |
#'  |pitchData.coordinates.pfxZ     |numeric   |
#'  |pitchData.coordinates.pX       |numeric   |
#'  |pitchData.coordinates.pZ       |numeric   |
#'  |pitchData.coordinates.vX0      |numeric   |
#'  |pitchData.coordinates.vY0      |numeric   |
#'  |pitchData.coordinates.vZ0      |numeric   |
#'  |pitchData.coordinates.x0       |numeric   |
#'  |pitchData.coordinates.y0       |numeric   |
#'  |pitchData.coordinates.z0       |numeric   |
#'  |pitchData.coordinates.aX       |numeric   |
#'  |pitchData.breaks.breakAngle    |numeric   |
#'  |pitchData.breaks.breakLength   |numeric   |
#'  |pitchData.breaks.breakY        |numeric   |
#'  |pitchData.breaks.spinRate      |integer   |
#'  |pitchData.breaks.spinDirection |integer   |
#'  |hitData.launchSpeed            |numeric   |
#'  |hitData.launchAngle            |numeric   |
#'  |hitData.totalDistance          |numeric   |
#'  |injuryType                     |character |
#'  |umpire.id                      |integer   |
#'  |umpire.link                    |character |
#'  |about.isTopInning              |logical   |
#'  |matchup.postOnFirst.id         |integer   |
#'  |matchup.postOnFirst.fullName   |character |
#'  |matchup.postOnFirst.link       |character |
#' @export
#' @examples \donttest{
#'   try(mlb_pbp_diff(game_pk = 632970, 
#'                    start_timecode = "20210808_231704", 
#'                    end_timecode = "20210808_233711"))
#' }

mlb_pbp_diff <- function(
  game_pk, 
  start_timecode, 
  end_timecode) {
  
  query_params <- list( 
    startTimecode = start_timecode, 
    endTimecode = end_timecode
  )
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1.1/game/{game_pk}/feed/live/diffPatch"))
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint,
                                   query = query_params)
  
  payload <- mlb_endpoint %>% 
    mlb_api_call() %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE)
  
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

#' @rdname get_pbp_mlb
#' @title **(legacy) Acquire pitch-by-pitch data for Major and Minor League games**
#' @inheritParams mlb_pbp
#' @return Returns a data frame that includes over 100 columns of data provided
#' by the MLB Stats API at a pitch level.
#' @keywords legacy
#' @export
get_pbp_mlb <- mlb_pbp