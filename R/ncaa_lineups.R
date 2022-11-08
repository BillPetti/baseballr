#' @rdname ncaa_lineups
#' @title **Retrieve lineups for a given NCAA game via its `game_info_url`**
#' @param game_info_url The unique game info url
#' @param year The year the game was played in
#' @return Returns a tibble of each school's starting lineup and starting pitcher
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |year          |numeric   |
#'  |playerName    |character |
#'  |position      |character |
#'  |batting_order |character |
#'  |school        |character |
#' @importFrom stringr str_detect str_squish str_starts str_remove_all str_split_fixed
#' @import rvest
#' @export
#' @examples 
#' \donttest{
#'   try(ncaa_lineups(game_info_url="https://stats.ncaa.org/game/index/4587474?org_id=528",year=2018))
#' }
ncaa_lineups <- function(game_info_url,year) {
  url <- game_info_url
  
  tryCatch(
    expr={
      dfa <- (url %>% 
                xml2::read_html() %>% 
                rvest::html_elements("table"))[[6]] %>% 
        rvest::html_table(trim=FALSE) %>% 
        dplyr::rename(
          "playerName" = "X1",
          "position" = "X2") %>% 
        dplyr::mutate(
          school = .data$playerName[1]) %>% 
        dplyr::select(
          "playerName",
          "position",
          "school")
      
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
          "playerName" = "X1", 
          "position" = "X2") %>% 
        dplyr::mutate(school = .data$playerName[1]) %>% 
        dplyr::select(
          "playerName", 
          "position", 
          "school")
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
          "year",
          "playerName",
          "position",
          "batting_order",
          "school") %>%
        make_baseballr_data("NCAA Baseball Lineups data from stats.ncaa.org",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  
  return(lineup_table)
}

#' @rdname get_ncaa_lineups
#' @title **(legacy) Retrieve lineups for a given NCAA game via its `game_info_url`**
#' @inheritParams ncaa_lineups
#' @return Returns a tibble of each school's starting lineup and starting pitcher
#' @keywords legacy
#' @export
get_ncaa_lineups <- ncaa_lineups
