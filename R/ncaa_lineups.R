#' @rdname ncaa_lineups
#' @title **Retrieve lineups for a given NCAA game via its `game_info_url`**
#' @param game_info_url The unique game info url
#' @param ... Additional arguments passed to an underlying function like httr.
#' @return Returns a tibble of each school's starting lineup and starting pitcher
#' 
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |year          |numeric   |
#'  |playerName    |character |
#'  |position      |character |
#'  |slug          |character |
#'  |batting_order |character |
#'  |team_name     |character |
#'  |sub           |numeric   |
#'  |attendance    |character |
#'  |game_date     |character |
#'  |location      |character |
#'  |player_id     |integer   |
#'  |team_id       |numeric   |
#'  |team_url      |character |
#'  |conference_id |numeric   |
#'  |conference    |character |
#'  |division      |numeric   |
#'  |season_id     |numeric   |
#' 
#' @importFrom stringr str_detect str_squish str_starts str_remove_all str_split_fixed
#' @import rvest
#' @export
#' @examples 
#' \donttest{
#'   try(ncaa_lineups(game_info_url="https://stats.ncaa.org/contests/2167178/box_score"))
#'   try(ncaa_lineups(game_info_url="https://stats.ncaa.org/game/index/4587474?org_id=528"))
#' }
ncaa_lineups <- function(game_info_url, ...) {
  dots <- rlang::dots_list(..., .named = TRUE)
  proxy <- dots$.proxy
  url <- game_info_url
  ncaa_teams <- load_ncaa_baseball_teams()
  tryCatch(
    expr = {
      if (stringr::str_detect(game_info_url,"contests")){
        game_info_resp <- httr::RETRY("GET", url = game_info_url, proxy, httr::add_headers(.headers = .ncaa_headers()))
        
        check_status(game_info_resp)
        
        init_payload <- game_info_resp %>% 
          httr::content(as = "text", encoding = "UTF-8") %>% 
          xml2::read_html() 
        
        url <- init_payload %>% 
          rvest::html_elements("#root li:nth-child(1) a") %>%
          rvest::html_attr("href") %>%
          as.data.frame() %>%
          dplyr::rename(pbp_url_slug = ".") %>%
          dplyr::mutate(game_pbp_url = paste0("https://stats.ncaa.org", .data$pbp_url_slug)) %>%
          dplyr::pull(.data$game_pbp_url)
      }
      lineup_resp <- httr::RETRY("GET", url = url, proxy, httr::add_headers(.headers = .ncaa_headers()))
      
      check_status(lineup_resp)
      
      payload <- lineup_resp %>% 
        httr::content(as = "text", encoding = "UTF-8") %>% 
        xml2::read_html()
      
      game_info <- payload %>%
        rvest::html_elements("table:nth-child(7)") %>%
        rvest::html_table() %>%
        as.data.frame() %>%
        tidyr::spread("X1", "X2") 
      
      game_info <- dplyr::rename_with(game_info,~gsub(":", "", .x)) %>%
        janitor::clean_names() %>%
        dplyr::mutate(game_date = substr(.data$game_date, 1, 10))
      
      athlete_extractor <- function(x){
        data.frame(slug = ifelse(
          length(
            (x %>%
               rvest::html_elements("a") %>% 
               rvest::html_attr("href"))) == 0, 
          NA_character_,
          (x %>%
             rvest::html_elements("a")) %>% 
            html_attr("href")
        ))
      }
      ### First Team -----
      first_team <- (payload %>% 
                       rvest::html_elements("table"))[[6]]
      
      first_team_table <- first_team %>% 
        rvest::html_table(trim=FALSE) %>% 
        dplyr::rename(
          "playerName" = "X1",
          "position" = "X2") %>% 
        dplyr::mutate(
          team_name = .data$playerName[1]) %>% 
        dplyr::select(
          "playerName",
          "position",
          "team_name")
      first_team_rows <- first_team %>% 
        rvest::html_elements("tr")
      
      first_team_slugs <- lapply(first_team_rows, athlete_extractor) %>% 
        dplyr::bind_rows()
      first_team_table <- first_team_table %>% 
        dplyr::bind_cols(first_team_slugs)
      
      first_team_table <- first_team_table[c(-1:-2,-nrow(first_team_table)),]
      first_team_table = first_team_table %>% 
        dplyr::mutate(
          playerName = stringr::str_remove_all(.data$playerName, "\\n"),
          sub = ifelse(stringr::str_starts(.data$playerName, "[[:space:]]") == TRUE, 1, 0),
          playerName = stringr::str_squish(.data$playerName))
      
      first_team_table = first_team_table %>% 
        dplyr::filter(.data$sub == 0) %>% 
        dplyr::mutate(
          batting_order = dplyr::row_number(),
          batting_order = ifelse(.data$position == "P","SP",.data$batting_order))
      
      ### Second Team -----
      second_team <- (payload %>% 
                        rvest::html_elements("table"))[[7]]
      
      second_team_table <- second_team %>% 
        rvest::html_table(trim = FALSE) %>% 
        dplyr::rename(
          "playerName" = "X1",
          "position" = "X2") %>% 
        dplyr::mutate(
          team_name = .data$playerName[1]) %>% 
        dplyr::select(
          "playerName",
          "position",
          "team_name")
      
      second_team_rows <- second_team %>% 
        rvest::html_elements("tr")
      
      second_team_slugs <- lapply(second_team_rows, athlete_extractor) %>% 
        dplyr::bind_rows()
      
      second_team_table <- second_team_table %>% 
        dplyr::bind_cols(second_team_slugs)
      
      second_team_table <- second_team_table[c(-1:-2,-nrow(second_team_table)),]
      second_team_table <-  second_team_table %>% 
        dplyr::mutate(
          playerName = stringr::str_remove_all(.data$playerName, "\\n"),
          sub = ifelse(stringr::str_starts(.data$playerName, "[[:space:]]") == TRUE, 1, 0),
          playerName = stringr::str_squish(.data$playerName))
      
      second_team_table = second_team_table  %>% 
        dplyr::filter(.data$sub == 0) %>% 
        dplyr::mutate(
          batting_order = dplyr::row_number(),
          batting_order = ifelse(.data$position == "P","SP",.data$batting_order))
      
      
      
      lineup_table <- first_team_table %>% 
        dplyr::bind_rows(second_team_table) %>% 
        dplyr::bind_cols(game_info) %>% 
        dplyr::mutate(
          year = as.integer(stringr::str_extract(.data$game_date, "\\d{4}")),
          team_name = stringr::str_squish(.data$team_name),
          player_id = as.integer(stringr::str_extract(.data$slug, "(?<=&stats_player_seq=)\\d+")))
      
      lineup_table <- lineup_table %>% 
        dplyr::left_join(ncaa_teams, by = c("team_name" = "team_name", "year" = "year"))
      
      lineup_table <- lineup_table %>% 
        dplyr::select(
          "year",
          "playerName",
          "position",
          "slug",
          "batting_order",
          "team_name",
          tidyr::everything()) %>%
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
