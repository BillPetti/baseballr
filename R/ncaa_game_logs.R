#' @rdname ncaa_game_logs
#' @title **Get NCAA Baseball Game Logs**
#' @param player_id A player's unique id. Can be found using the
#' get_ncaa_baseball_roster function.
#' @param year The year of interest.
#' @param type The kind of statistics you want to return. Current options
#' are 'batting' or 'pitching'.
#' @param span The span of time; can either be 'game' for game logs in a season, or 'career' which
#' returns seasonal stats for a player's career.
#' @param ... Additional arguments passed to an underlying function like httr.
#' @return A data frame containing player and school information
#' as well as game by game statistics
#' 
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |player_id     |numeric   |
#'  |player_name   |character |
#'  |Date          |character |
#'  |Opponent      |character |
#'  |Result        |character |
#'  |App           |numeric   |
#'  |G             |numeric   |
#'  |GS            |numeric   |
#'  |IP            |numeric   |
#'  |CG            |numeric   |
#'  |H             |numeric   |
#'  |R             |numeric   |
#'  |ER            |numeric   |
#'  |BB            |numeric   |
#'  |SO            |numeric   |
#'  |SHO           |numeric   |
#'  |BF            |numeric   |
#'  |P-OAB         |numeric   |
#'  |2B-A          |numeric   |
#'  |3B-A          |numeric   |
#'  |Bk            |numeric   |
#'  |HR-A          |numeric   |
#'  |WP            |numeric   |
#'  |HB            |numeric   |
#'  |IBB           |numeric   |
#'  |Inh Run       |numeric   |
#'  |Inh Run Score |numeric   |
#'  |SHA           |numeric   |
#'  |SFA           |numeric   |
#'  |Pitches       |numeric   |
#'  |GO            |numeric   |
#'  |FO            |numeric   |
#'  |W             |numeric   |
#'  |L             |numeric   |
#'  |SV            |numeric   |
#'  |OrdAppeared   |numeric   |
#'  |KL            |numeric   |
#'  |pickoffs      |character |
#'  
#' @importFrom tibble tibble
#' @importFrom tidyr extract_numeric
#' @import rvest
#' @export
#' @examples \donttest{
#'   try(ncaa_game_logs(player_id = 2649785, year = 2023, type = "pitching", span = "game"))
#'   try(ncaa_game_logs(player_id = 2477974, year = 2023, type = "pitching", span = "career"))
#'   try(ncaa_game_logs(player_id = 2680961, year = 2023, type = "batting", span = "game"))
#'   try(ncaa_game_logs(player_id = 2486588, year = 2023, type = "batting", span = "career"))
#' }

ncaa_game_logs <- function(player_id, year, type = "batting", span = 'game', ...) {
  if (is.null(player_id)) {
    cli::cli_abort("Enter valid player_id")
  }
  if (is.null(year)) {
    cli::cli_abort("Enter valid year as a number (YYYY)")
  }
  if (is.null(type) | !(type %in% c("batting","pitching", "fielding"))) {
    cli::cli_abort("Enter valid type: 'batting', 'pitching'")
  }
  if (is.null(span) | !(span %in% c("game","career"))) {
    cli::cli_abort("Enter valid span: 'game', 'career'")
  }
  
  
  season_ids <- load_ncaa_baseball_season_ids()
  year_id <- season_ids %>% 
    dplyr::filter(.data$season == year) %>% 
    dplyr::select("id")
  batting_id <- season_ids %>% 
    dplyr::filter(.data$season == year) %>% 
    dplyr::select("batting_id")
  pitching_id <- season_ids %>% 
    dplyr::filter(.data$season == year) %>% 
    dplyr::select("pitching_id")
  
  headers <- httr::add_headers(.headers = .ncaa_headers())
  tryCatch(
    expr = {
      if (type == "batting") {
        
        batting_url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", batting_id)
        batting_resp <- request_with_proxy(url = batting_url, ..., headers)
        
        check_status(batting_resp)
        
        batting_payload <- batting_resp %>% 
          httr::content(as = "text", encoding = "UTF-8") %>% 
          xml2::read_html()
        
        player_name <- ((batting_payload %>% 
                           rvest::html_elements("select"))[3] %>% 
                          rvest::html_elements(xpath = "//option[@selected]") %>% 
                          rvest::html_text())[3]
        player_name <- stringr::str_remove(stringr::str_extract(player_name,".*(?<= #)")," #")
      } else {
        
        pitching_url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", pitching_id)
        
        pitching_resp <- request_with_proxy(url = pitching_url, ..., headers)
        
        check_status(pitching_resp)
        
        pitching_payload <- pitching_resp %>% 
          httr::content(as = "text", encoding = "UTF-8") %>% 
          xml2::read_html()
        
        player_name <- ((pitching_payload %>% 
                           rvest::html_elements("select"))[3] %>% 
                          rvest::html_elements(xpath = "//option[@selected]") %>% 
                          rvest::html_text())[3]
        player_name <- stringr::str_remove(stringr::str_extract(player_name,".*(?<= #)")," #")
      }
      
      if (span == 'game') {
        
        if (type == "batting") {
          
          payload_df <- ((batting_payload %>%
                            rvest::html_elements("table"))[5] %>%
                           rvest::html_table() %>%
                           as.data.frame())
          
          colnames(payload_df) <- payload_df[2,]
          
          payload_df <- payload_df[-c(1:2),]
          payload_df <- payload_df[!is.na(names(payload_df))]
          
          if ('OPP DP' %in% colnames(payload_df) == TRUE) {
            payload_df <- payload_df %>%
              dplyr::rename("DP" = "OPP DP")
          }
          
          batting_cols <- c("Date", "Opponent", "Result",
                            "G", "R", "AB", "H", "2B", "3B", "TB", "HR", "RBI",
                            "BB", "HBP", "SF", "SH", "K", "DP", "CS", "Picked",
                            "SB", "IBB", "RBI2out")
          
          payload_df <- payload_df %>% 
            dplyr::select(batting_cols)
          
          cols_to_num <- c("G", "R", "AB", "H", "2B", "3B", "TB", "HR", "RBI",
                           "BB", "HBP", "SF", "SH", "K", "DP", "CS", "Picked",
                           "SB", "IBB", "RBI2out")
          suppressWarnings(
            payload_df <- payload_df %>%
              dplyr::mutate_at(cols_to_num, as.numeric)
          )
          payload_df <- payload_df %>%
            dplyr::mutate(
              player_id = player_id,
              player_name = player_name,
              Year = year) %>%
            dplyr::select("player_id", "player_name", tidyr::everything())
          
        } else {
          
          payload_df <- ((pitching_payload %>%
                            rvest::html_elements("table"))[5] %>%
                           rvest::html_table() %>%
                           as.data.frame())
          
          colnames(payload_df) <- payload_df[2,]
          payload_df <- payload_df[!is.na(names(payload_df))]
          
          payload_df <- payload_df[-c(1:2),]
          
          if ('OPP DP' %in% colnames(payload_df) == TRUE) {
            
            payload_df <- payload_df %>%
              dplyr::rename("DP" = "OPP DP")
          }
          
          cols_to_num <- c("G", "App", "GS", "IP", "CG", "H", "R", "ER", "BB", "SO", "SHO", "BF", "P-OAB", "2B-A", "3B-A", "Bk", "HR-A", "WP", "HB", "IBB", "Inh Run", "Inh Run Score", "SHA", "SFA", "Pitches", "GO", "FO", "W", "L", "SV", "OrdAppeared", "KL")
          suppressWarnings(
            payload_df <- payload_df %>%
              dplyr::mutate_at(cols_to_num, as.numeric)
          )
        }
        
        payload_df <- payload_df %>%
          dplyr::mutate(
            player_id = player_id,
            player_name = player_name,
            Year = year) %>%
          dplyr::select("player_id", "player_name", tidyr::everything())
        
      } else {
        
        if (type == 'batting') {
          
          payload_df <- ((batting_payload %>%
                            rvest::html_elements('table'))[3] %>%
                           rvest::html_table() %>%
                           as.data.frame())[-1,]
          
          colnames(payload_df) <- payload_df[1,]
          payload_df <- payload_df[!is.na(names(payload_df))]
          
          payload_df <- payload_df[-1,]
          
          if ('OPP DP' %in% colnames(payload_df) == TRUE) {
            
            payload_df <- payload_df %>%
              dplyr::rename("DP" = "OPP DP")
          }
          
          payload_df <- payload_df %>%
            dplyr::select(
              "Year", 
              "Team", 
              "GP",
              "BA",
              "G",
              "OBPct",
              "SlgPct",
              "R",
              "AB",
              "H",
              "2B",
              "3B",
              "TB",
              "HR",
              "RBI",
              "BB",
              "HBP",
              "SF",
              "SH",
              "K",
              "DP",
              "CS",
              "Picked",
              "SB",
              "RBI2out")
          
          payload_df <- payload_df %>%
            dplyr::mutate(
              player_id = player_id,
              player_name = player_name) %>%
            dplyr::select("Year", "player_id", "player_name", tidyr::everything())
          
        } else {
          
          payload_df <- ((pitching_payload %>%
                            rvest::html_elements('table'))[3] %>%
                           rvest::html_table() %>%
                           as.data.frame())[-1,]
          
          colnames(payload_df) <- payload_df[1,]
          payload_df <- payload_df[!is.na(names(payload_df))]
          
          payload_df <- payload_df[-1,]
          
          payload_df <- payload_df %>%
            dplyr::select(
              "Year",
              "Team",
              "GP",
              "G",
              "App",
              "GS",
              "ERA",
              "IP",
              "CG",
              "H",
              "R",
              "ER",
              "BB",
              "SO",
              "SHO",
              "BF",
              "P-OAB",
              "2B-A",
              "3B-A",
              "Bk",
              "HR-A",
              "WP",
              "HB",
              "IBB",
              "Inh Run",
              "Inh Run Score",
              "SHA",
              "SFA",
              "Pitches",
              "GO",
              "FO",
              "W",
              "L",
              "SV",
              "KL", 
              tidyr::everything())
          
          
          payload_df <- payload_df %>%
            dplyr::mutate(
              player_id = player_id,
              player_name = player_name) %>%
            dplyr::select("Year", "player_id", "player_name", tidyr::everything())
        }
        
      }
      payload_df <- payload_df %>%
        make_baseballr_data("NCAA Baseball Game Logs data from stats.ncaa.org",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(payload_df)
  
}
#' @rdname get_ncaa_game_logs
#' @title **(legacy) Get NCAA Baseball Game Logs**
#' @inheritParams ncaa_game_logs
#' @return A data frame containing player and school information
#' as well as game by game statistics
#' @keywords legacy
#' @export
get_ncaa_game_logs <- ncaa_game_logs