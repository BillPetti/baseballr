#' Retrieve lineups for a given NCAA game via its game_info_url
#'
#' @param game_info_url The unique game info url
#' @param year The year the game was played in
#'
#' @return returns a data frame of each school's starting lineup and starting pitcher
#' @export
#'
#' @examples get_ncaa_lineups("https://stats.ncaa.org/game/index/4587474?org_id=528",2018)
get_ncaa_lineups <- function(game_info_url,year) {
  dfa <- xml2::read_html({{game_info_url}}) %>% rvest::html_nodes("table") %>% .[[6]] %>% rvest::html_table(trim = F, fill = T) 
  dfa <- dfa %>% dplyr::rename(playerName = X1, position = X2) %>% dplyr::mutate(school = playerName[1]) %>% dplyr::select(playerName,position,school)
  dfa <- dfa[c(-1:-2,-nrow(dfa)),]
  dfa = dfa %>% dplyr::mutate(playerName = stringr::str_remove_all(playerName,"\\n"),
                              sub = ifelse(stringr::str_starts(playerName,"[[:space:]]")==TRUE,1,0),
                              playerName = stringr::str_squish(playerName))
  
  dfa = dfa %>% dplyr::filter(sub == 0) %>% dplyr::mutate(batting_order = dplyr::row_number(),
                                                          batting_order = ifelse(position == "P","SP",batting_order))
  dfb <-  xml2::read_html({{game_info_url}}) %>% rvest::html_nodes("table") %>% .[[7]] %>% rvest::html_table(trim = F, fill = T)
  dfb <- dfb %>% dplyr::rename(playerName = X1, position = X2) %>% dplyr::mutate(school = playerName[1]) %>% dplyr::select(playerName,position,school)
  dfb <- dfb[c(-1:-2,-nrow(dfb)),]
  dfb <-  dfb %>% dplyr::mutate(playerName = stringr::str_remove_all(playerName,"\\n"),
                                sub = ifelse(stringr::str_starts(playerName,"[[:space:]]")==TRUE,1,0),
                                playerName = stringr::str_squish(playerName))
  
  dfb = dfb  %>% dplyr::filter(sub == 0) %>% dplyr::mutate(batting_order = dplyr::row_number(),
                                                           batting_order = ifelse(position == "P","SP",batting_order))
  lineup_table <- dplyr::bind_rows(dfa,dfb) %>% dplyr::mutate(year = {{year}})
  
  lineup_table <- lineup_table %>% dplyr::select(year,playerName,position,batting_order,school)
  
  return(lineup_table)
}
