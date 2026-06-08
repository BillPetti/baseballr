#' @rdname fg_fielder_leaders
#' @title **Scrape Fielder Leaderboards from FanGraphs**
#' @description This function allows you to scrape all leaderboard statistics (basic and advanced) from FanGraphs.com.
#' @param age (integer) Age of players
#' @param pos (character) Position of players, defaults to "all". To exclude pitchers, use "np".
#' @param stats (character) Statistic to return. Defaults to "bat".
#' @param lg (character) League to return. Defaults to "all". Options are "al", "nl", or "all".
#' @param qual (character) Whether you want only batters/pitchers that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimum number of plate appearaces/innings pitched, use the number desired. Defaults to "y".
#' @param startseason (character) Season for which you want to scrape the data.
#' @param endseason (character) Last season for which you want data.
#' @param startdate (character) Start date for which you want data.
#' @param enddate (character) End date for which you want data.
#' @param month (character) Month for which you want data.
#' @param hand (character) Handedness of batter. Options are "L", "R", or "B". Empty string returns all.
#' @param team (character) Teams for which you want data, comma separated.
#' @param pageitems (character) Number of items per page.
#' @param pagenum (character) Page number.
#' @param ind (character) Whether or not to break the seasons out individual, or roll them up together. 1 = split seasons, 0 = aggregate seasons.
#' @param rost (character) Whether or not to include players on the roster. 1 = include, 0 = exclude.
#' @param players (character) Whether or not to include players on the roster. 1 = include only active roster players, 0 = exclude.
#' @param type (character) Defaults to 8, which is the standard leaderboard. The values for the leaderboards appear to go to from type = 0 to 48+, which correspond to links on the leaderboard page.
#' @param postseason (logical) Whether or not to include postseason data. TRUE = include postseason, FALSE = exclude postseason.
#' @param sortdir (character) Sort direction. Options are "asc" or "desc" or "default".
#' @param sortstat (character) Sort by stat. Default is "Defense".
#' @return A data frame of fielder data.
#'
#'  |col_name        |types     |description                                       |
#'  |:-------------- |:-------- |:------------------------------------------------ |
#'  |Season          |integer   |Season (YYYY).                                    |
#'  |team_name       |character |Team name.                                        |
#'  |xMLBAMID        |integer   |MLBAM player ID.                                  |
#'  |PlayerNameRoute |character |Player name URL slug used by FanGraphs.           |
#'  |PlayerName      |character |Player name.                                      |
#'  |playerid        |integer   |FanGraphs player ID.                              |
#'  |SeasonMin       |integer   |First season in the queried span.                 |
#'  |SeasonMax       |integer   |Last season in the queried span.                  |
#'  |Pos             |character |Primary position.                                 |
#'  |Position        |character |Position played.                                  |
#'  |G               |integer   |Games played.                                     |
#'  |GS              |integer   |Games started.                                    |
#'  |Inn             |numeric   |Innings played in the field.                      |
#'  |PO              |integer   |Putouts.                                          |
#'  |A               |integer   |Assists.                                          |
#'  |E               |integer   |Errors.                                           |
#'  |FE              |integer   |Fielding errors.                                  |
#'  |TE              |integer   |Throwing errors.                                  |
#'  |DP              |integer   |Double plays.                                     |
#'  |DPS             |integer   |Double plays started.                             |
#'  |DPT             |integer   |Double plays turned.                              |
#'  |DPF             |integer   |Double plays finished.                            |
#'  |SB              |integer   |Stolen bases.                                     |
#'  |CS              |integer   |Caught stealing.                                  |
#'  |PB              |integer   |Passed balls.                                     |
#'  |WP              |integer   |Wild pitches.                                     |
#'  |FP              |numeric   |Fielding percentage.                              |
#'  |rSB             |integer   |Stolen-base runs (catcher arm).                   |
#'  |rGFP            |integer   |Good-fielding-play runs.                          |
#'  |rSZ             |numeric   |Strike-zone (framing) runs.                       |
#'  |rCERA           |integer   |Catcher-ERA runs.                                 |
#'  |DRS             |integer   |Defensive Runs Saved.                             |
#'  |Defense         |numeric   |Total defensive value (runs above average).       |
#'  |CStrikes        |numeric   |Catcher framing called strikes above average.     |
#'  |CFraming        |numeric   |Catcher framing runs.                             |
#'  |dFRP            |integer   |Double-play component of Fielding Run Prevention. |
#'  |bFRP            |integer   |Bunt component of Fielding Run Prevention.        |
#'  |tFRP            |integer   |Throwing component of Fielding Run Prevention.    |
#'  |fFRP            |integer   |Framing component of Fielding Run Prevention.     |
#'  |FRP             |integer   |Total Fielding Run Prevention.                    |
#'  |Q               |numeric   |Quality of contact / quality score.               |
#'  |TInn            |numeric   |Total innings played in the field.                |
#'  |positionDB      |character |Position code from the database.                  |
#'  |teamid          |integer   |FanGraphs team ID.                                |
#'  |team_name_abb   |character |Team name abbreviation.                           |
#'  |playerTeamId    |integer   |FanGraphs player-team ID.                         |
#'  |rGDP            |integer   |Double-play runs.                                 |
#'  |rPM             |integer   |Plus/minus range runs.                            |
#'  |BIZ             |integer   |Balls hit in defensive zone.                      |
#'  |Plays           |integer   |Plays made in zone.                               |
#'  |RZR             |numeric   |Revised Zone Rating.                              |
#'  |OOZ             |integer   |Plays made out of zone.                           |
#'  |DPR             |numeric   |Double-play runs (UZR component).                 |
#'  |RngR            |numeric   |Range runs (UZR component).                       |
#'  |ErrR            |numeric   |Error runs (UZR component).                       |
#'  |UZR             |numeric   |Ultimate Zone Rating.                             |
#'  |UZR_150         |numeric   |Ultimate Zone Rating per 150 defensive games.     |
#'  |OAA             |integer   |Outs Above Average (Statcast).                    |
#'  |rFRP            |integer   |Range component of Fielding Run Prevention.       |
#'  |aFRP            |integer   |Arm component of Fielding Run Prevention.         |
#'  |rARM            |integer   |Outfield-arm runs.                                |
#'  |ARM             |numeric   |Outfield-arm runs (UZR component).                |
#'  |Scp             |integer   |Scoops (first-base picks).                        |
#'
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(fg_fielder_leaders(startseason = 2023, endseason = 2023))
#' }
fg_fielder_leaders <- function(
    age = "",
    pos = "all",
    stats = "fld",
    lg = "all",
    qual = "0",
    startseason = "2023",
    endseason = "2023",
    startdate = "",
    enddate = "",
    month = "0",
    hand = "",
    team = "0",
    pageitems = "10000",
    pagenum = "1",
    ind = "0",
    rost = "0",
    players = "",
    type = "1",
    postseason = "",
    sortdir = "default",
    sortstat = "Defense") {
  
  params <- list(
    age = age,
    pos = pos,
    stats = stats,
    lg = lg,
    qual = qual,
    season = startseason,
    season1 = endseason,
    startdate = startdate,
    enddate = enddate,
    month = month,
    hand = hand,
    team = team,
    pageitems = pageitems,
    pagenum = pagenum,
    ind = ind,
    rost = rost,
    players = players,
    type = type,
    postseason = postseason,
    sortdir = sortdir,
    sortstat = sortstat
  )
  
  url <- "https://www.fangraphs.com/api/leaders/major-league/data"
  
  fg_endpoint <- httr2::url_modify_query(url, !!!params)
  
  leaders <- NULL
  tryCatch(
    expr = {
      
      resp <- fg_endpoint |> 
        fg_api_call()
      
      fg_df <- resp$data |> 
        jsonlite::toJSON() |>
        jsonlite::fromJSON(flatten=TRUE)
      
      c <- colnames(fg_df)
      c <- gsub("%", "_pct", c, fixed = TRUE)
      c <- gsub("/", "_", c, fixed = TRUE)
      c <- ifelse(substr(c, nchar(c) - 1 + 1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
      c <- gsub(" ", "_", c, fixed = TRUE)
      colnames(fg_df) <- c
      leaders <- fg_df |> 
        dplyr::rename_with(~ gsub("pi", "pi_", .x), starts_with("pi")) |> 
        dplyr::rename_with(~ gsub("pfx", "pfx_", .x), starts_with("pfx")) |>
        # any_of() so FanGraphs' narrower split leaderboards (month = 13/14)
        # don't error on absent renamed/leading columns (cf. #323).
        dplyr::rename(dplyr::any_of(c(
          "team_name" = "TeamName",
          "team_name_abb" = "TeamNameAbb"))) |>
        dplyr::select(-dplyr::any_of(c(
          "Name",
          "Team"
        ))) |>
        dplyr::select(
          dplyr::any_of(c(
            "Season",
            "team_name",
            "xMLBAMID",
            "PlayerNameRoute",
            "PlayerName",
            "playerid")),
          tidyr::everything()) |>
        make_baseballr_data("MLB Player Fielding Leaders data from FanGraphs.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no player fielding leaders data available!")
    },
    finally = {
    }
  )
  return(leaders)
}
#' @rdname fg_bat_leaders
#' @title **(legacy) Scrape Batter Leaderboards from FanGraphs**
#' @inheritParams fg_batter_leaders
#' @return A data frame of batter data.
#' @keywords legacy
#' @export
fg_bat_leaders <- fg_batter_leaders
