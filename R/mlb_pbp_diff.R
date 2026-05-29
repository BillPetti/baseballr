#' @rdname mlb_pbp_diff
#' @title **Acquire pitch-by-pitch data between two timecodes for Major and Minor League games**
#'
#' @param game_pk The date for which you want to find game_pk values for MLB games
#' @param start_timecode The start time code for the MLB game (format: MMDDYYYY_HHMMSS)
#' @param end_timecode The end time code for the MLB game (format: MMDDYYYY_HHMMSS)
#' @importFrom jsonlite fromJSON
#' @return Returns a tibble that includes over 100 columns of data provided
#' by the MLB Stats API at a pitch level between the start_timecode and end_timecode
#' 
#'  |col_name                       |types     |description                                                       |
#'  |:------------------------------|:---------|:-----------------------------------------------------------------|
#'  |game_pk                        |numeric   |MLB game primary key.                                             |
#'  |game_date                      |character |Game date (YYYY-MM-DD).                                          |
#'  |index                          |integer   |Index of the play event within the at-bat.                        |
#'  |startTime                      |character |Event start timestamp (ISO 8601).                                 |
#'  |endTime                        |character |Event end timestamp (ISO 8601).                                   |
#'  |isPitch                        |logical   |Whether the event is a pitch.                                     |
#'  |type                           |character |Play event type (e.g. 'pitch', 'action').                        |
#'  |playId                         |character |Unique play event identifier (UUID).                              |
#'  |pitchNumber                    |integer   |Pitch number within the at-bat.                                   |
#'  |details.description            |character |Pitch/event description (e.g. 'Foul', 'Ball').                   |
#'  |details.event                  |character |Event name for non-pitch actions.                                 |
#'  |details.awayScore              |integer   |Away score recorded at the event.                                 |
#'  |details.homeScore              |integer   |Home score recorded at the event.                                 |
#'  |details.isScoringPlay          |logical   |Whether the event is a scoring play.                              |
#'  |details.hasReview              |logical   |Whether the event was reviewed.                                   |
#'  |details.code                   |character |Pitch/result code (e.g. 'F', 'B').                              |
#'  |details.ballColor              |character |Gameday ball color rgba string.                                   |
#'  |details.isInPlay               |logical   |Whether the ball was put in play.                                 |
#'  |details.isStrike               |logical   |Whether the pitch was a strike.                                   |
#'  |details.isBall                 |logical   |Whether the pitch was a ball.                                     |
#'  |details.call.code              |character |Umpire call code.                                                 |
#'  |details.call.description       |character |Umpire call description.                                          |
#'  |count.balls.start              |integer   |Ball count before the pitch.                                      |
#'  |count.strikes.start            |integer   |Strike count before the pitch.                                    |
#'  |count.outs.start               |integer   |Out count before the pitch.                                       |
#'  |player.id                      |integer   |Player id for action-event participant.                           |
#'  |player.link                    |character |API relative link to the action player.                           |
#'  |pitchData.strikeZoneTop        |numeric   |Top of the batter strike zone (feet).                             |
#'  |pitchData.strikeZoneBottom     |numeric   |Bottom of the batter strike zone (feet).                          |
#'  |details.fromCatcher            |logical   |Whether the event originated from the catcher.                    |
#'  |pitchData.coordinates.x        |numeric   |Pitch x pixel coordinate (Gameday).                               |
#'  |pitchData.coordinates.y        |numeric   |Pitch y pixel coordinate (Gameday).                               |
#'  |hitData.trajectory             |character |Batted ball trajectory.                                           |
#'  |hitData.hardness               |character |Batted ball hardness.                                             |
#'  |hitData.location               |character |Fielding position where the ball was hit.                         |
#'  |hitData.coordinates.coordX     |numeric   |Batted ball landing x coordinate.                                 |
#'  |hitData.coordinates.coordY     |numeric   |Batted ball landing y coordinate.                                 |
#'  |actionPlayId                   |character |Identifier of an associated action play.                          |
#'  |details.eventType              |character |Event type slug for non-pitch actions.                            |
#'  |details.runnerGoing            |logical   |Whether a runner was going on the pitch.                          |
#'  |position.code                  |character |Fielding position code for the player.                            |
#'  |position.name                  |character |Fielding position name.                                           |
#'  |position.type                  |character |Fielding position type.                                           |
#'  |position.abbreviation          |character |Fielding position abbreviation.                                   |
#'  |battingOrder                   |character |Batting order slot.                                               |
#'  |atBatIndex                     |character |At-bat index within the game (factor).                            |
#'  |result.type                    |character |Result type (e.g. 'atBat').                                      |
#'  |result.event                   |character |At-bat result event.                                              |
#'  |result.eventType               |character |At-bat result event slug.                                         |
#'  |result.description             |character |Narrative description of the at-bat result.                       |
#'  |result.rbi                     |integer   |Runs batted in on the at-bat.                                     |
#'  |result.awayScore               |integer   |Away score after the at-bat.                                      |
#'  |result.homeScore               |integer   |Home score after the at-bat.                                      |
#'  |about.atBatIndex               |integer   |At-bat index (numeric).                                          |
#'  |about.halfInning               |character |Half inning ('top' or 'bottom').                                |
#'  |about.inning                   |integer   |Inning number.                                                    |
#'  |about.startTime                |character |At-bat start timestamp (ISO 8601).                                |
#'  |about.endTime                  |character |At-bat end timestamp (ISO 8601).                                  |
#'  |about.isComplete               |logical   |Whether the at-bat is complete.                                   |
#'  |about.isScoringPlay            |logical   |Whether the at-bat is a scoring play.                             |
#'  |about.hasReview                |logical   |Whether the at-bat had a review.                                  |
#'  |about.hasOut                   |logical   |Whether the at-bat produced an out.                               |
#'  |about.captivatingIndex         |integer   |MLB captivating index for the play.                               |
#'  |count.balls.end                |integer   |Ball count after the pitch.                                       |
#'  |count.strikes.end              |integer   |Strike count after the pitch.                                     |
#'  |count.outs.end                 |integer   |Out count after the pitch.                                        |
#'  |matchup.batter.id              |integer   |Batter player id.                                                 |
#'  |matchup.batter.fullName        |character |Batter full name (factor).                                       |
#'  |matchup.batter.link            |character |API relative link to the batter.                                  |
#'  |matchup.batSide.code           |character |Batter handedness code (e.g. 'L', 'R').                         |
#'  |matchup.batSide.description    |character |Batter handedness description.                                    |
#'  |matchup.pitcher.id             |integer   |Pitcher player id.                                                |
#'  |matchup.pitcher.fullName       |character |Pitcher full name (factor).                                      |
#'  |matchup.pitcher.link           |character |API relative link to the pitcher.                                 |
#'  |matchup.pitchHand.code         |character |Pitcher throwing hand code (e.g. 'R').                          |
#'  |matchup.pitchHand.description  |character |Pitcher throwing hand description.                                |
#'  |matchup.splits.batter          |character |Batter platoon split (e.g. 'vs_RHP').                           |
#'  |matchup.splits.pitcher         |character |Pitcher platoon split (e.g. 'vs_RHB').                          |
#'  |matchup.splits.menOnBase       |character |Men on base split (e.g. 'Men_On').                              |
#'  |batted.ball.result             |factor    |Categorized batted ball result.                                  |
#'  |home_team                      |character |Home team name.                                                   |
#'  |home_level_id                  |integer   |Home team level/sport id (1 for MLB).                            |
#'  |home_level_name                |character |Home team level/sport name.                                       |
#'  |home_parentOrg_id              |integer   |Home team parent organization id (minors).                        |
#'  |home_parentOrg_name            |character |Home team parent organization name (minors).                      |
#'  |home_league_id                 |integer   |Home team league id.                                              |
#'  |home_league_name               |character |Home team league name.                                            |
#'  |away_team                      |character |Away team name.                                                   |
#'  |away_level_id                  |integer   |Away team level/sport id (1 for MLB).                            |
#'  |away_level_name                |character |Away team level/sport name.                                       |
#'  |away_parentOrg_id              |integer   |Away team parent organization id (minors).                        |
#'  |away_parentOrg_name            |character |Away team parent organization name (minors).                      |
#'  |away_league_id                 |integer   |Away team league id.                                              |
#'  |away_league_name               |character |Away team league name.                                            |
#'  |batting_team                   |character |Team batting on the play (factor).                               |
#'  |fielding_team                  |character |Team fielding on the play (factor).                              |
#'  |last.pitch.of.ab               |character |Whether the pitch was the last of the at-bat (factor).            |
#'  |pfxId                          |character |Pitch f/x tracking identifier.                                    |
#'  |details.trailColor             |character |Gameday pitch trail color rgba string.                            |
#'  |details.type.code              |character |Pitch type code (e.g. 'SI', 'CH').                              |
#'  |details.type.description       |character |Pitch type description (e.g. 'Sinker', 'Changeup').              |
#'  |pitchData.startSpeed           |numeric   |Pitch release speed (mph).                                        |
#'  |pitchData.endSpeed             |numeric   |Pitch speed crossing the plate (mph).                             |
#'  |pitchData.zone                 |integer   |Strike zone region of the pitch.                                  |
#'  |pitchData.typeConfidence       |numeric   |Pitch type classification confidence.                             |
#'  |pitchData.plateTime            |numeric   |Time from release to plate (seconds).                             |
#'  |pitchData.extension            |numeric   |Pitcher release extension (feet).                                 |
#'  |pitchData.coordinates.aY       |numeric   |Pitch acceleration in y (ft/s^2).                                |
#'  |pitchData.coordinates.aZ       |numeric   |Pitch acceleration in z (ft/s^2).                                |
#'  |pitchData.coordinates.pfxX     |numeric   |Horizontal pitch movement (inches).                               |
#'  |pitchData.coordinates.pfxZ     |numeric   |Vertical pitch movement (inches).                                 |
#'  |pitchData.coordinates.pX       |numeric   |Horizontal pitch location at plate (feet).                        |
#'  |pitchData.coordinates.pZ       |numeric   |Vertical pitch location at plate (feet).                          |
#'  |pitchData.coordinates.vX0      |numeric   |Pitch initial velocity in x (ft/s).                              |
#'  |pitchData.coordinates.vY0      |numeric   |Pitch initial velocity in y (ft/s).                              |
#'  |pitchData.coordinates.vZ0      |numeric   |Pitch initial velocity in z (ft/s).                              |
#'  |pitchData.coordinates.x0       |numeric   |Pitch initial x position (feet).                                  |
#'  |pitchData.coordinates.y0       |numeric   |Pitch initial y position (feet).                                  |
#'  |pitchData.coordinates.z0       |numeric   |Pitch initial z position (feet).                                  |
#'  |pitchData.coordinates.aX       |numeric   |Pitch acceleration in x (ft/s^2).                                |
#'  |pitchData.breaks.breakAngle    |numeric   |Pitch break angle (degrees).                                     |
#'  |pitchData.breaks.breakLength   |numeric   |Pitch break length (inches).                                     |
#'  |pitchData.breaks.breakY        |numeric   |Distance from plate where break is measured.                      |
#'  |pitchData.breaks.spinRate      |integer   |Pitch spin rate (RPM).                                           |
#'  |pitchData.breaks.spinDirection |integer   |Pitch spin direction (degrees).                                  |
#'  |hitData.launchSpeed            |numeric   |Batted ball exit velocity (mph).                                 |
#'  |hitData.launchAngle            |numeric   |Batted ball launch angle (degrees).                              |
#'  |hitData.totalDistance          |numeric   |Batted ball total distance (feet).                               |
#'  |injuryType                     |character |Injury type when the event is injury-related.                     |
#'  |umpire.id                      |integer   |Umpire person id for the event.                                   |
#'  |umpire.link                    |character |API relative link to the umpire.                                  |
#'  |details.isOut                  |logical   |Whether the pitch/event resulted in an out.                       |
#'  |pitchData.breaks.breakVertical |numeric   |Total vertical break (inches).                                    |
#'  |pitchData.breaks.breakVerticalInduced |numeric |Induced vertical break (inches).                            |
#'  |pitchData.breaks.breakHorizontal |numeric |Horizontal break (inches).                                      |
#'  |details.disengagementNum       |integer   |Pitcher disengagement number on the play.                         |
#'  |result.isOut                   |logical   |Whether the at-bat resulted in an out.                            |
#'  |about.isTopInning              |logical   |Whether the play occurred in the top of the inning.               |
#'  |matchup.postOnFirst.id         |integer   |Runner id on first base after the play.                           |
#'  |matchup.postOnFirst.fullName   |character |Runner name on first base after the play.                         |
#'  |matchup.postOnFirst.link       |character |API relative link to the first base runner.                       |
#'
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
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1.1/game/{game_pk}/feed/live/diffPatch"))
  query_params <- list( 
    startTimecode = start_timecode, 
    endTimecode = end_timecode
  )
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  pbp <- NULL
  tryCatch(
    expr = {
      payload <- mlb_endpoint |> 
        mlb_api_call() |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE)
      
      plays <- payload$liveData$plays$allPlays$playEvents |> 
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
      
      columns <- lapply(at_bats, function(x) class(x)) |>
        dplyr::bind_rows(.id = "variable")
      cols <- c(colnames(columns))
      classes <- c(t(unname(columns[1,])))
      
      df <- data.frame(cols, classes)
      list_columns <- df |>
        dplyr::filter(.data$classes == "list") |>
        dplyr::pull("cols")
      
      at_bats <- at_bats |>
        dplyr::select(-c(tidyr::one_of(list_columns)))
      
      pbp <- plays |>
        dplyr::left_join(at_bats, by = c("endTime" = "playEndTime"))
      
      pbp <- pbp |>
        tidyr::fill("atBatIndex":"matchup.splits.menOnBase", .direction = "up") |>
        dplyr::mutate(
          game_pk = game_pk,
          game_date = substr(payload$gameData$datetime$dateTime, 1, 10)) |>
        dplyr::select("game_pk", "game_date", tidyr::everything())
      
      pbp <- pbp |>
        dplyr::mutate(
          matchup.batter.fullName = factor(.data$matchup.batter.fullName),
          matchup.pitcher.fullName = factor(.data$matchup.pitcher.fullName),
          atBatIndex = factor(.data$atBatIndex)
          # batted.ball.result = case_when(!result.event %in% c(
          #   "Single", "Double", "Triple", "Home Run") ~ "Out/Other",
          #   TRUE ~ result.event),
          # batted.ball.result = factor(batted.ball.result,
          #                             levels = c("Single", "Double", "Triple", "Home Run", "Out/Other"))
        ) |>
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
      pbp <- pbp |>
        dplyr::arrange(desc(.data$atBatIndex), desc(.data$pitchNumber))
      
      pbp <- pbp |>
        dplyr::group_by(.data$atBatIndex) |>
        dplyr::mutate(
          last.pitch.of.ab =  ifelse(.data$pitchNumber == max(.data$pitchNumber), "true", "false"),
          last.pitch.of.ab = factor(.data$last.pitch.of.ab)) |>
        dplyr::ungroup()
      
      pbp <- dplyr::bind_rows(baseballr::stats_api_live_empty_df, pbp)
      
      check_home_level <- pbp |>
        dplyr::distinct(.data$home_level_id) |>
        dplyr::pull()
      
      # this will need to be updated in the future to properly estimate X,Z coordinates at the minor league level
      
      # if(check_home_level != 1) {
      #
      #   pbp <- pbp |>
      #     dplyr::mutate(pitchData.coordinates.x = -pitchData.coordinates.x,
      #                   pitchData.coordinates.y = -pitchData.coordinates.y)
      #
      #   pbp <- pbp |>
      #     dplyr::mutate(pitchData.coordinates.pX_est = predict(x_model, pbp),
      #                   pitchData.coordinates.pZ_est = predict(y_model, pbp))
      #
      #   pbp <- pbp |>
      #     dplyr::mutate(pitchData.coordinates.x = -pitchData.coordinates.x,
      #                   pitchData.coordinates.y = -pitchData.coordinates.y)
      # }
      
      pbp <- pbp |>
        dplyr::rename(
          "count.balls.start" = "count.balls.x",
          "count.strikes.start" = "count.strikes.x",
          "count.outs.start" = "count.outs.x",
          "count.balls.end" = "count.balls.y",
          "count.strikes.end" = "count.strikes.y",
          "count.outs.end" = "count.outs.y") |>
        make_baseballr_data("MLB Play-by-Play diff data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(pbp)
}

#' @rdname get_pbp_mlb
#' @title **(legacy) Acquire pitch-by-pitch data for Major and Minor League games**
#' @inheritParams mlb_pbp
#' @return Returns a tibble that includes over 100 columns of data provided
#' by the MLB Stats API at a pitch level.
#' @keywords legacy
#' @export
get_pbp_mlb <- mlb_pbp
