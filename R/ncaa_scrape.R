#' Scrape NCAA baseball data (Division I, II, and III)
#'
#' This function allows the user to obtain batting or pitching statistics for any school affiliated with the NCAA at the division I, II, or III levels. The function acquires data from the NCAA's website (stats.ncaa.org) and returns a data frame.
#'
#' @param teamid The numerical ID that the NCAA website uses to identify a team
#' @param year The season for which data should be returned, in the form of "YYYY". Years currently available: 2013-2017.
#' @param type A string indicating whether to return "batting" or "pitching" statistics
#' @keywords baseball, NCAA, college
#' @import dplyr
#' @importFrom rvest html_table html_attr html_nodes
#' @importFrom xml2 read_html
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_split
#' @export ncaa_scrape
#' @examples
#' \dontrun{
#' ncaa_scrape(255, 2013, "batting")
#' }

ncaa_scrape <- function(teamid, year, type) {

  if (year < 2013) {
    stop('you must provide a year that is equal to or greater than 2013')
  }

  else
    if (type == "batting") {
      id <- subset(ncaa_season_id_lu, season == year, select = id)
      url <- paste0("http://stats.ncaa.org/team/",teamid,"/stats?game_sport_year_ctl_id=", id, "&id=", id)
      data_read <- xml2::read_html(url)
      data <- data_read %>%
        rvest::html_nodes("table") %>%
        .[[3]] %>%
        rvest::html_table(fill = TRUE)
      df <- as.data.frame(data)
      df$year <- year
      df$teamid <- teamid
      df <- df %>%
        dplyr::left_join(master_ncaa_team_lu, by = c("teamid" = "school_id", "year" = "year"))
      df <- dplyr::select(df, year, school, conference, division, everything())
      df$Player <- gsub("x ", "", df$Player)
      if (!"RBI2out" %in% names(df)) {
        df$RBI2out <- NA
      }

      if('OPP DP' %in% colnames(df) == TRUE) {

        df <- df %>%
          dplyr::rename(DP = `OPP DP`)
      }

      df <- dplyr::select(df,year,school,conference,division,Jersey,Player,Yr,Pos,GP,GS,BA,OBPct,SlgPct,R,AB,H,`2B`,`3B`,TB,HR,RBI,BB,HBP,SF,SH,K,DP,CS,Picked,SB,RBI2out,teamid,conference_id)
    }

  else {
    year_id <- subset(ncaa_season_id_lu, season == year, select = id)
    type_id <- subset(ncaa_season_id_lu, season == year, select = pitching_id)
    url <- paste0("http://stats.ncaa.org/team/", teamid, "/stats?id=", year_id, "&year_stat_category_id=", type_id)
    data_read <- xml2::read_html(url)
    data <- data_read %>%
      rvest::html_nodes("table") %>%
      .[[3]] %>%
      rvest::html_table(fill = TRUE)
    df <- as.data.frame(data)
    df <- df[,-6]
    df$year <- year
    df$teamid <- teamid
    df <- df %>%
      left_join(master_ncaa_team_lu, by = c("teamid" = "school_id", "year" = "year"))
    df <- dplyr::select(df, year, school, conference, division, everything())
    df$Player <- gsub("x ", "", df$Player)
    df <- dplyr::select(df, year,school,conference,division,Jersey,Player,Yr,Pos,GP,App,GS,ERA,IP,H,R,ER,BB,SO,SHO,BF,`P-OAB`,`2B-A`,`3B-A`,Bk,`HR-A`,WP,HB,IBB,`Inh Run`,`Inh Run Score`,SHA,SFA,Pitches,GO,FO,W,L,SV,KL,teamid,conference_id)
  }

  player_url <- data_read %>%
    rvest::html_nodes('#stat_grid a') %>%
    rvest::html_attr('href') %>%
    as.data.frame() %>%
    dplyr::rename(player_url = '.') %>%
    dplyr::mutate(player_url = paste0('stats.ncaa.org', player_url))

  player_id <-
    stringr::str_split(pattern = '&stats_player_seq=',  string = player_url$player_url,simplify = T)[,2] %>%
    as.data.frame() %>%
    dplyr::rename(player_id = '.')

  player_url_comb <- dplyr::bind_cols(player_id, player_url)

  df <- dplyr::left_join(df %>%
                           tibble::rownames_to_column('rows'),
                         player_url_comb %>%
                           tibble::rownames_to_column('rows'), by = 'rows')

  return(df)
}
