#' Query BaseballSavant Leaderboards from \url{http://baseballsavant.mlb.com}
#'
#' This function allows you to read leaderboard data from BaseballSavant dirctly into R as #` a \code{\link{data.frame}}.
#' @param leaderboard The type of leaderboard to retrieve, input as a string. Current
#' options include exit_velocity_barrels, expected_statistics, pitch_arsenal,
#' outs_above_average, directional_oaa, catch_probability, pop_time, sprint_speed, and
#' running_splits_90_ft.
#' @param year The season for which you want data.
#' @param abs The minimum number of batted balls. Applies only to exit_velocity_barrels
#' leaderboards.
#' @param min_pa Minimum number of plate appearances.
#' @param min_pitches Minimum number of pitches thrown.
#' @param min_field Minimum number of fieding opportunities.
#' @param min_run Minimum number of running opportunities.
#' @param player_type One of either 'batter' or pitcher. For the expected_statistics
#' leaderboard, 'batter-team' and 'pitcher-team' are also available.
#' @param fielding_type One of either 'player' or 'team'.
#' @param team An abbreviation for a team. Can be left blank.
#' @param arsenal_type One of either 'n_', 'avg_spin', or 'avg_speed'.
#' @param run_type One of either 'percent' or 'raw'.
#' @param min2b The minimum number of throwing attempts to second base.
#' @param min3b The minimum number of throwing attempts to third base.
#' @param position The numeric position of the player. For DH use 10. Can be left blank.
#' @param bats The handedness of the batter. One of 'R' or 'L'. Can be left blank.
#' @param hand The handedness of the pitcher. One of 'R' or 'L'. Can be left blank.
#' @keywords MLB, sabermetrics, Statcast
#' @importFrom readr read_csv
#' @export
#' @examples
#' \dontrun{
#' correa <- scrape_statcast_savant(start_date = "2016-04-06",
#'   end_date = "2016-04-15", playerid = 621043)
#'
#' noah <- scrape_statcast_savant(start_date = "2016-04-06",
#'   end_date = "2016-04-15", playerid = 592789, player_type = 'pitcher')
#'
#' daily <- scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-06")
#' }

scrape_savant_leaderboards <- function(leaderboard = "exit_velocity_barrels",
                                       year = 2018,
                                       abs = 50,
                                       min_pa = 250,
                                       min_pitches = 100,
                                       min_field = "q",
                                       min_run = 0,
                                       player_type = "batter",
                                       fielding_type = "player",
                                       team = "",
                                       arsenal_type = "n_",
                                       run_type = "raw",
                                       min2b = 5,
                                       min3b = 0,
                                       position = "",
                                       bats = "",
                                       hand = "") {

  if (leaderboard == "exit_velocity_barrels") {

    if (!year %in% c(seq(2015,substr(Sys.time(), 1, 4),1))) {

      message("Exit Velocity and Barrel leaderboards are only available starting in 2015. Please choose an appropriate year.")
    }

    if (!abs %in% c(0,5,10,20,25,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,250,300,400,500)) {

      message("Please choose one of the following for the number of batted balls: 0,5,10,20,25,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,250,300,400,500")

      return(NULL)
    }

    url <- paste0("https://baseballsavant.mlb.com/statcast_leaderboard?year=", year, "&abs=", abs, "&player_type=", player_type, "&csv=true")

    payload <- read_csv(url)
  }

  if (leaderboard == "expected_statistics") {

    if (!year %in% c(seq(2015,substr(Sys.time(), 1, 4),1))) {

      message("Expected Statistics leaderboards are only available starting in 2015. Please choose an appropriate year.")

      return(NULL)
    }

    if (!min_pa %in% c(1,25,50,100,200,250,350,450,500,600)) {

      message("Please choose one of the following for the minimum number of plate appearances:1,25,50,100,200,250,350,450,500,600")

      return(NULL)
    }

    if(!team %in% c("", "ATL", "ARI", "BAL", "BOS", "CHC", "CIN", "CLE", "COL", "CWS", "DET", "HOU", "KC", "LAA", "LAD", "OAK", "MIA", "MIL", "MIN", "NYM", "NYY", "PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH")) {

      message("If you are trying to query records for a specific team, please be sure to enter one of the following: ATL, ARI, BAL, BOS, CHC, CIN, CLE, COL, CWS, DET, HOU, KC, LAA, LAD, OAK, MIA, MIL, MIN, NYM, NYY, PHI, PIT, SD, SEA, SF, STL, TB, TEX, TOR, WSH")

      return(NULL)
    }

    if(!position %in% c("", 1,2,3,4,5,6,7,8,9,10)) {

      message("Please enter one of the following for position: 1,2,3,4,5,6,7,8,9,10")

      return(NULL)
    }

    url <- paste0("https://baseballsavant.mlb.com/expected_statistics?type=", player_type, "&year=", year, "&position=", position, "&team=", team, "&min=", min_pa,"&csv=true")

    payload <- read_csv(url)
  }

  if (leaderboard == "pitch_arsenal") {

    if (!year %in% c(seq(2017,substr(Sys.time(), 1, 4),1))) {

      message("Pitch Arsenal leaderboards are only available starting in 2017. Please choose an appropriate year.")
    }

    if (!min_pitches %in% c(50, 100, 250, 500, 750, 1000, 1500, 2000, 2500, 3000)) {

      message("Please choose one of the following for the minimum number of pitches: 50, 100, 250, 500, 750, 1000, 1500, 2000, 2500, 3000")

      return(NULL)
    }

    if(!hand %in% c("", "R", "L")) {

      message("Please choose either 'R' or 'L' for `hand`, or leave the parameter blank.")

      return(NULL)
    }

    if(!arsenal_type %in% c("n_", "avg_spin", "avg_speed")){

      message("Please enter of the following: 'n_' for percentages, 'avg_spin' for average spin, or 'avg_speed' for average speed.")

      return(NULL)
    }

    url <- paste0("https://baseballsavant.mlb.com/pitch-arsenals?year=", year, "&min=", min_pitches, "&type=", arsenal_type, "&hand=", hand, "&csv=true")

    payload <- read_csv(url)
  }



  if (leaderboard == "outs_above_average") {

    if (!year %in% c(seq(2016,substr(Sys.time(), 1, 4),1))) {

      message("Outs Above Average leaderboards are only available starting in 2016. Please choose an appropriate year.")

      return(NULL)
    }

    if (!min_field %in% c("q",0,5,25,50,75,100,150,200,250)) {

      message("Please choose one of the following for the minimum number of fielding opportunities:'q',0,5,25,50,75,100,150,200,250")

      return(NULL)
    }

    url <- paste0("https://baseballsavant.mlb.com/outs_above_average?type=", fielding_type, "&year=", year, "&min=", min_field, "&csv=true")

    payload <- read_csv(url)
  }



  if (leaderboard == "directional_oaa") {

    if (!year %in% c(seq(2016,substr(Sys.time(), 1, 4),1))) {

      message("Directional Outs Above Average leaderboards are only available starting in 2016. Please choose an appropriate year.")

      return(NULL)
    }

    if (!min_field %in% c("q",0,5,25,50,75,100,150,200,250)) {

      message("Please choose one of the following for the minimum number of fielding opportunities:'q',0,5,25,50,75,100,150,200,250")

      return(NULL)
    }

    if(!team %in% c("", "ATL", "ARI", "BAL", "BOS", "CHC", "CIN", "CLE", "COL", "CWS", "DET", "HOU", "KC", "LAA", "LAD", "OAK", "MIA", "MIL", "MIN", "NYM", "NYY", "PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH")) {

      message("If you are trying to query records for a specific team, please be sure to enter one of the following: ATL, ARI, BAL, BOS, CHC, CIN, CLE, COL, CWS, DET, HOU, KC, LAA, LAD, OAK, MIA, MIL, MIN, NYM, NYY, PHI, PIT, SD, SEA, SF, STL, TB, TEX, TOR, WSH")

      return(NULL)
    }

    url <- paste0("https://baseballsavant.mlb.com/directional_outs_above_average?year=", year, "&min=", min_field, "&team=", team, "&csv=true")

    payload <- read_csv(url)
  }

  if (leaderboard == "catch_probability") {

    if (!year %in% c(seq(2016,substr(Sys.time(), 1, 4),1))) {

      message("Catch Probability Expected Statistic leaderboards are only available starting in 2016. Please choose an appropriate year.")

      return(NULL)
    }

    if (!min_field %in% c("q",0,5,25,50,75,100,150,200,250)) {

      message("Please choose one of the following for the minimum number of fielding opportunities:'q',0,5,25,50,75,100,150,200,250")

      return(NULL)
    }

    url <- paste0("https://baseballsavant.mlb.com/catch_probability_leaderboard?type=", fielding_type, "&min=", min_field, "&year=", year, "&csv=true")

    payload <- read_csv(url)
  }

  if (leaderboard == "pop_time") {

    if (!year %in% c(seq(2015,substr(Sys.time(), 1, 4),1))) {

      message("Pop Time leaderboards are only available starting in 2015. Please choose an appropriate year.")

      return(NULL)
    }

    if (!min2b %in% c(1,5,10,15,20)) {

      message("Please choose one of the following for the minimum number of throws to second base: 1,5,10,15,20")

      return(NULL)
    }

    if (!min3b %in% c(0,1,5,10,15,20)) {

      message("Please choose one of the following for the minimum number of throws to third base: 0,1,5,10,15,20")

      return(NULL)
    }

    if(!team %in% c("", "ATL", "ARI", "BAL", "BOS", "CHC", "CIN", "CLE", "COL", "CWS", "DET", "HOU", "KC", "LAA", "LAD", "OAK", "MIA", "MIL", "MIN", "NYM", "NYY", "PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH")) {

      message("If you are trying to query records for a specific team, please be sure to enter one of the following: ATL, ARI, BAL, BOS, CHC, CIN, CLE, COL, CWS, DET, HOU, KC, LAA, LAD, OAK, MIA, MIL, MIN, NYM, NYY, PHI, PIT, SD, SEA, SF, STL, TB, TEX, TOR, WSH")

      return(NULL)
    }

    url <- paste0("https://baseballsavant.mlb.com/poptime?year=", year, "&team=", team, "&min2b=", min2b, "&min3b=", min3b, "&csv=true")

    payload <- read_csv(url)
  }

  if (leaderboard == "sprint_speed") {

    if (!year %in% c(seq(2015,substr(Sys.time(), 1, 4),1))) {

      message("Pop Time leaderboards are only available starting in 2015. Please choose an appropriate year.")

      return(NULL)
    }

    if(!team %in% c("", "ATL", "ARI", "BAL", "BOS", "CHC", "CIN", "CLE", "COL", "CWS", "DET", "HOU", "KC", "LAA", "LAD", "OAK", "MIA", "MIL", "MIN", "NYM", "NYY", "PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH")) {

      message("If you are trying to query records for a specific team, please be sure to enter one of the following: ATL, ARI, BAL, BOS, CHC, CIN, CLE, COL, CWS, DET, HOU, KC, LAA, LAD, OAK, MIA, MIL, MIN, NYM, NYY, PHI, PIT, SD, SEA, SF, STL, TB, TEX, TOR, WSH")

      return(NULL)
    }

    if(!position %in% c("", 1,2,3,4,5,6,7,8,9,10)) {

      message("Please enter one of the following for position, or leave the parameter blank: 1,2,3,4,5,6,7,8,9,10")

      return(NULL)
    }

    if (!min_run %in% c(0,5,25,50,75,100,150,200,250)) {

      message("Please choose one of the following for the minimum number of running opportunities: 0,5,25,50,75,100,150,200,250")

      return(NULL)
    }

    url <- paste0("https://baseballsavant.mlb.com/sprint_speed_leaderboard?year=", year, "&position=", position, "&team=", team, "&min=", min_run, "&csv=true")

    payload <- read_csv(url)
  }


  if (leaderboard == "running_splits_90_ft") {

    if (!year %in% c(seq(2017,substr(Sys.time(), 1, 4),1))) {

      message("Pop Time leaderboards are only available starting in 2017. Please choose an appropriate year.")

      return(NULL)
    }

    if(!team %in% c("", "ATL", "ARI", "BAL", "BOS", "CHC", "CIN", "CLE", "COL", "CWS", "DET", "HOU", "KC", "LAA", "LAD", "OAK", "MIA", "MIL", "MIN", "NYM", "NYY", "PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH")) {

      message("If you are trying to query records for a specific team, please be sure to enter one of the following: ATL, ARI, BAL, BOS, CHC, CIN, CLE, COL, CWS, DET, HOU, KC, LAA, LAD, OAK, MIA, MIL, MIN, NYM, NYY, PHI, PIT, SD, SEA, SF, STL, TB, TEX, TOR, WSH")

      return(NULL)
    }

    if(!position %in% c("", 1,2,3,4,5,6,7,8,9,10)) {

      message("Please enter one of the following for position, or leave the parameter blank: 1,2,3,4,5,6,7,8,9,10")

      return(NULL)
    }

    if (!min_run %in% c(0,5,25,50,75,100,150,200,250)) {

      message("Please choose one of the following for the minimum number of running opportunities: 0,5,25,50,75,100,150,200,250")

      return(NULL)
    }

    if(!bats %in% c("", "R", "L")) {

      message("Please choose either 'R' or 'L' for `bats`, or leave the parameter blank.")

      return(NULL)
    }

    if(!run_type %in% c("percent", "raw")) {

      message("Please choose either 'percent' or 'raw' for 'run_type'.")

      return(NULL)
    }

    url <- paste0("https://baseballsavant.mlb.com/running_splits?type=", run_type, "&bats=", bats, "&year=", year, "&position=", position, "&team=", team, "&min=", min_run, "&csv=true")

    payload <- read_csv(url)
  }

  if(!"year" %in% colnames(payload)) {

    payload <- payload %>%
      mutate(year = year)
  }

  payload <- payload %>%
    select(year, everything())

  return(payload)
}
