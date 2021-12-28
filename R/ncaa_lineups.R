#' @rdname ncaa_lineups
#' @title **Retrieve lineups for a given NCAA game via its `game_info_url`**
#' @param game_info_url The unique game info url
#' @param year The year the game was played in
#' @return Returns a data frame of each school's starting lineup and starting pitcher
#' @importFrom stringr str_detect str_squish str_starts str_remove_all str_split_fixed
#' @import rvest
#' @export
#' @examples 
#' \donttest{
#'   ncaa_lineups(game_info_url="https://stats.ncaa.org/game/index/4587474?org_id=528",year=2018)
#' }
ncaa_lineups <- function(game_info_url,year) {
  url <- game_info_url
  dfa <- (url %>% 
    xml2::read_html() %>% 
    rvest::html_elements("table"))[[6]] %>% 
    rvest::html_table(trim=FALSE) %>% 
    dplyr::rename(
      playerName = .data$X1,
      position = .data$X2) %>% 
    dplyr::mutate(
      school = .data$playerName[1]) %>% 
    dplyr::select(.data$playerName,.data$position,.data$school)
  
  dfa <- dfa[c(-1:-2,-nrow(dfa)),]
  dfa = dfa %>% 
    dplyr::mutate(
      playerName = stringr::str_remove_all(.data$playerName,"\\n"),
      sub = ifelse(stringr::str_starts(.data$playerName,"[[:space:]]")==TRUE,1,0),
      playerName = stringr::str_squish(.data$playerName))
  
  dfa = dfa %>% 
    dplyr::filter(.data$sub == 0) %>% 
    dplyr::mutate(
      batting_order = dplyr::row_number(),
      batting_order = ifelse(.data$position == "P","SP",.data$batting_order))
  dfb <-  (game_info_url %>%
    xml2::read_html() %>% 
    rvest::html_elements("table"))[[7]] %>% 
    rvest::html_table(trim = FALSE) %>% 
    dplyr::rename(
      playerName = .data$X1, 
      position = .data$X2) %>% 
    dplyr::mutate(school = .data$playerName[1]) %>% 
    dplyr::select(.data$playerName, .data$position, .data$school)
  dfb <- dfb[c(-1:-2,-nrow(dfb)),]
  dfb <-  dfb %>% 
    dplyr::mutate(
      playerName = stringr::str_remove_all(.data$playerName,"\\n"),
      sub = ifelse(stringr::str_starts(.data$playerName,"[[:space:]]")==TRUE,1,0),
      playerName = stringr::str_squish(.data$playerName))
  
  dfb = dfb  %>% 
    dplyr::filter(.data$sub == 0) %>% 
    dplyr::mutate(
      batting_order = dplyr::row_number(),
      batting_order = ifelse(.data$position == "P","SP",.data$batting_order))
  lineup_table <- dplyr::bind_rows(dfa,dfb) %>% 
    dplyr::mutate(
      year = {{year}},
      school = stringr::str_squish(.data$school))
  
  lineup_table <- lineup_table %>% 
    dplyr::select(
      .data$year,
      .data$playerName,
      .data$position,
      .data$batting_order,
      .data$school)
  
  return(lineup_table)
}

#' @rdname get_ncaa_lineups
#' @title **(legacy) Retrieve lineups for a given NCAA game via its `game_info_url`**
#' @inheritParams ncaa_lineups
#' @return Returns a data frame of each school's starting lineup and starting pitcher
#' @keywords legacy
#' @export
get_ncaa_lineups <- ncaa_lineups