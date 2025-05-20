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
#'   |col_name        |types     |
#'   |:---------------|:---------|
#'   |Season          |integer   |
#'   |team_name       |character |
#'   |xMLBAMID        |integer   |
#'   |PlayerNameRoute |character |
#'   |PlayerName      |character |
#'   |playerid        |integer   |
#'   |SeasonMin       |integer   |
#'   |SeasonMax       |integer   |
#'   |Pos             |character |
#'   |G               |integer   |
#'   |GS              |integer   |
#'   |Inn             |numeric   |
#'   |PO              |integer   |
#'   |A               |integer   |
#'   |E               |integer   |
#'   |FE              |integer   |
#'   |TE              |integer   |
#'   |DP              |integer   |
#'   |DPS             |integer   |
#'   |DPT             |integer   |
#'   |DPF             |integer   |
#'   |SB              |integer   |
#'   |CS              |integer   |
#'   |PB              |integer   |
#'   |WP              |integer   |
#'   |FP              |numeric   |
#'   |rSB             |integer   |
#'   |rGFP            |integer   |
#'   |rSZ             |numeric   |
#'   |rCERA           |integer   |
#'   |DRS             |integer   |
#'   |Defense         |numeric   |
#'   |CStrikes        |numeric   |
#'   |CFraming        |numeric   |
#'   |Q               |numeric   |
#'   |TInn            |numeric   |
#'   |teamid          |integer   |
#'   |team_name_abb   |character |
#'   |rARM            |integer   |
#'   |rPM             |integer   |
#'   |BIZ             |integer   |
#'   |Plays           |integer   |
#'   |RZR             |numeric   |
#'   |OOZ             |integer   |
#'   |ARM             |numeric   |
#'   |RngR            |numeric   |
#'   |ErrR            |numeric   |
#'   |UZR             |numeric   |
#'   |UZR_150         |numeric   |
#'   |OAA             |integer   |
#'   |rFRP            |integer   |
#'   |rGDP            |integer   |
#'   |DPR             |numeric   |
#'   |Scp             |integer   |
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
  
  fg_endpoint <- httr::modify_url(url, query = params)
  
  tryCatch(
    expr = {
      
      resp <- fg_endpoint %>% 
        mlb_api_call()
      
      fg_df <- resp$data %>% 
        jsonlite::toJSON() %>%
        jsonlite::fromJSON(flatten=TRUE)
      
      c <- colnames(fg_df)
      c <- gsub("%", "_pct", c, fixed = TRUE)
      c <- gsub("/", "_", c, fixed = TRUE)
      c <- ifelse(substr(c, nchar(c) - 1 + 1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
      c <- gsub(" ", "_", c, fixed = TRUE)
      colnames(fg_df) <- c
      leaders <- fg_df %>% 
        dplyr::rename_with(~ gsub("pi", "pi_", .x), starts_with("pi")) %>% 
        dplyr::rename_with(~ gsub("pfx", "pfx_", .x), starts_with("pfx")) %>%
        dplyr::rename(
          "team_name" = "TeamName",
          "team_name_abb" = "TeamNameAbb") %>%
        dplyr::select(-dplyr::any_of(c(
          "Name", 
          "Team"
        ))) %>%
        dplyr::select(
          "Season",
          "team_name",
          "xMLBAMID", 
          "PlayerNameRoute",
          "PlayerName",
          "playerid",
          tidyr::everything()) %>% 
        make_baseballr_data("MLB Player Fielding Leaders data from FanGraphs.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no player fielding leaders data available!"))
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
