#' @title Scrape NCAA baseball data (Division I, II, and III)
#' @description This function allows the user to obtain batting or pitching statistics for any school affiliated with the NCAA at the division I, II, or III levels. The function acquires data from the NCAA's website (stats.ncaa.org) and returns a data frame.
#' @param teamid The numerical ID that the NCAA website uses to identify a team
#' @param year The season for which data should be returned, in the form of "YYYY". Years currently available: 2013-2017.
#' @param type A string indicating whether to return "batting" or "pitching" statistics
#' @import dplyr
#' @import rvest
#' @importFrom stringr str_split
#' @export
#' @examples
#' \donttest{
#'   ncaa_scrape(teamid=255, year=2013, type = "batting")
#' }

ncaa_scrape <- function(teamid, year, type = 'batting') {

  if (year < 2013) {
    stop('you must provide a year that is equal to or greater than 2013')
  }

  if (type == "batting") {
    id <- baseballr::ncaa_season_id_lu %>% 
      dplyr::filter(.data$season == year) %>% 
      dplyr::select(.data$id)

    url <- paste0("http://stats.ncaa.org/team/",teamid,"/stats?game_sport_year_ctl_id=", id, "&id=", id)
    data_read <- xml2::read_html(url)
    data <- (data_read %>%
      rvest::html_elements("table"))[[3]] %>%
      rvest::html_table(fill = TRUE)
    df <- as.data.frame(data)
    df$year <- year
    df$teamid <- teamid
    df <- df %>%
      dplyr::left_join(baseballr::master_ncaa_team_lu,
                       by = c("teamid" = "school_id", "year" = "year"))
    df <- df %>% 
      dplyr::select(.data$year, .data$school, .data$conference, .data$division, tidyr::everything())
    df$Player <- gsub("x ", "", df$Player)
    if (!"RBI2out" %in% names(df)) {
      df$RBI2out <- NA
    }

    if('OPP DP' %in% colnames(df) == TRUE) {

      df <- df %>%
        dplyr::rename(DP = .data$`OPP DP`)
    }

    df <- df %>% dplyr::select(
      .data$year,.data$school,.data$conference,.data$division,.data$Jersey,.data$Player,
      .data$Yr,.data$Pos,.data$GP,.data$GS,.data$BA,.data$OBPct,.data$SlgPct,.data$R,.data$AB,
      .data$H,.data$`2B`,.data$`3B`,.data$TB,.data$HR,.data$RBI,.data$BB,.data$HBP,.data$SF,.data$SH,
      .data$K,.data$DP,.data$CS,.data$Picked,.data$SB,.data$RBI2out,.data$teamid,.data$conference_id)

    character_cols <- c("year", "school", "conference", "Jersey", "Player",
                        "Yr", "Pos")

    numeric_cols <- c("division", "GP", "GS", "BA", "OBPct", "SlgPct", "R", "AB",
                      "H", "2B", "3B", "TB", "HR", "RBI", "BB", "HBP", "SF", "SH",
                      "K", "DP", "CS", "Picked", "SB", "RBI2out", "teamid", "conference_id")

    suppressWarnings(
      df <- df %>%
        dplyr::mutate_at(vars(character_cols), function(x){as.character(x)})
    )
    suppressWarnings(
      df <- df %>%
        dplyr::mutate_at(vars(numeric_cols), function(x){as.numeric(as.character(x))})
    )

  } else {
    year_id <- baseballr::ncaa_season_id_lu %>% 
      dplyr::filter(.data$season == year) %>% 
      dplyr::select(.data$id)
    type_id <- baseballr::ncaa_season_id_lu %>% 
      dplyr::filter(.data$season == year) %>% 
      dplyr::select(.data$pitching_id)
    url <- paste0("http://stats.ncaa.org/team/", teamid, "/stats?id=", year_id, "&year_stat_category_id=", type_id)
    data_read <- xml2::read_html(url)
    data <- (data_read %>%
      rvest::html_elements("table"))[[3]] %>%
      rvest::html_table(fill = TRUE)
    df <- as.data.frame(data)
    df <- df[,-6]
    df$year <- year
    df$teamid <- teamid
    df <- df %>%
      dplyr::left_join(baseballr::master_ncaa_team_lu, by = c("teamid" = "school_id", "year" = "year"))
    df <- df %>% 
      dplyr::select(.data$year, .data$school, .data$conference, .data$division, tidyr::everything())
    df$Player <- gsub("x ", "", df$Player)

    df <- df %>% dplyr::select( 
      .data$year,.data$school,.data$conference,.data$division,.data$Jersey,.data$Player,
      .data$Yr,.data$Pos,.data$GP,.data$App,.data$GS,.data$ERA,.data$IP,.data$H,.data$R,
      .data$ER,.data$BB,.data$SO,.data$SHO,.data$BF,.data$`P-OAB`,
      .data$`2B-A`,.data$`3B-A`,.data$Bk,.data$`HR-A`,.data$WP,.data$HB,
      .data$IBB,.data$`Inh Run`,.data$`Inh Run Score`,
      .data$SHA,.data$SFA,.data$Pitches,.data$GO,.data$FO,.data$W,.data$L,
      .data$SV,.data$KL,.data$teamid,.data$conference_id)

    character_cols <- c("year", "school", "conference", "Jersey", "Player",
                        "Yr", "Pos")

    numeric_cols <- c("division",  "GP", "App", "GS", "ERA", "IP", "H", "R", "ER",
                      "BB", "SO", "SHO", "BF", "P-OAB", "2B-A", "3B-A", "Bk", "HR-A",
                      "WP", "HB", "IBB", "Inh Run", "Inh Run Score", "SHA", "SFA",
                      "Pitches", "GO", "FO", "W", "L", "SV", "KL", "teamid", "conference_id")
    suppressWarnings(
      df <- df %>%
        dplyr::mutate_at(vars(character_cols), function(x){as.character(x)})
    )
    suppressWarnings(
      df <- df %>%
        dplyr::mutate_at(vars(numeric_cols), function(x){as.numeric(as.character(x))})
    )
  }

  player_url <- data_read %>%
    html_elements('#stat_grid a') %>%
    html_attr('href') %>%
    as.data.frame() %>%
    dplyr::rename(player_url = .data$`.`) %>%
    dplyr::mutate(player_url = paste0('http://stats.ncaa.org', .data$player_url))

  player_names_join <- data_read %>%
    html_elements('#stat_grid a') %>%
    html_text() %>%
    as.data.frame() %>%
    dplyr::rename(player_names_join = .data$`.`)

  player_id <-
    stringr::str_split(
      pattern = '&stats_player_seq=',  
      string = player_url$player_url, simplify = TRUE)[,2] %>%
    as.data.frame() %>%
    dplyr::rename(player_id = .data$`.`)

  player_url_comb <- dplyr::bind_cols(player_names_join, player_id, player_url)

  df <- df %>% 
    dplyr::left_join(player_url_comb, by = c('Player' = 'player_names_join'))

  df <- df %>%
    dplyr::mutate_at(vars(player_url), as.character) %>%
    dplyr::mutate_at(c("conference_id", "player_id", "year"), as.integer)

  return(df)

}
