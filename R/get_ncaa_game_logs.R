#' Get NCAA Baseball Game Logs
#'
#' @param player_id A player's unique id. Can be found using the
#' get_ncaa_baseball_roster function.
#' @param year The year of interest.
#' @param type The kind of statistics you want to return. Current options
#' are 'batting' or 'pitching'.
#' @param span The span of time; can either be 'game' for game logs in a season, or 'career' which
#' returns seasonal stats for a player's career.
#'
#' @importFrom rvest html_nodes html_attr html_table html_text
#' @importFrom tibble tibble
#' @importFrom xml2 read_html
#' @importFrom tidyr extract_numeric
#'
#' @return A data frame containing player and school information
#' as well as game by game statistics
#' @export
#'
#' @examples \donttest{
#' get_ncaa_game_logs(player_id = 2113782, year = 2021, type = "pitching", span = "game")
#' get_ncaa_game_logs(player_id = 1879650, year = 2019, type = "batting", span="career")
#' }

get_ncaa_game_logs <- function(player_id,
                               year = 2019,
                               type = "batting",
                               span = 'game') {

  year_id <- baseballr::ncaa_season_id_lu %>% 
    dplyr::filter(.data$season == year) %>% 
    dplyr::select(.data$id)
  batting_id <- baseballr::ncaa_season_id_lu %>% 
    dplyr::filter(.data$season == year) %>% 
    dplyr::select(.data$batting_id)
  pitching_id <- baseballr::ncaa_season_id_lu %>% 
    dplyr::filter(.data$season == year) %>% 
    dplyr::select(.data$pitching_id)

  if (type == "batting") {

    batting_url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", batting_id)
    batting_payload <- xml2::read_html(batting_url)
    player_name <- ((batting_payload %>% 
                       rvest::html_elements("select"))[3] %>% 
                      rvest::html_elements(xpath="//option[@selected]") %>% 
                      rvest::html_text())[3]
  } else {

    pitching_url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", pitching_id)
    pitching_payload <- xml2::read_html(pitching_url)
    player_name <- ((pitching_payload %>% 
      rvest::html_elements("select"))[3] %>% 
      rvest::html_elements(xpath="//option[@selected]") %>% 
      rvest::html_text())[3]
  }
  
  if (span == 'game') {

    if (type == "batting") {

      payload_df <- ((batting_payload %>%
        rvest::html_elements("table"))[5] %>%
        rvest::html_table() %>%
        as.data.frame())

      colnames(payload_df) <- payload_df[2,]

      payload_df <- payload_df[-c(1:2),]
      batting_cols <- c("Date", "Opponent", "Result",
                        "G", "R", "AB", "H", "2B", "3B", "TB", "HR", "RBI",
                        "BB", "HBP", "SF", "SH", "K", "DP", "CS", "Picked",
                        "SB", "IBB", "RBI2out")
      payload_df <- payload_df %>% 
        dplyr::select(batting_cols)
      payload_df <- payload_df %>%
        dplyr::mutate_at(vars(.data$G:.data$RBI2out), extract_numeric)

      if('OPP DP' %in% colnames(payload_df) == TRUE) {

        payload_df <- payload_df %>%
          dplyr::rename(DP = -.data$`OPP DP`)
      }

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
          player_name = player_name) %>%
        dplyr::select(.data$player_id, .data$player_name, tidyr::everything())
      
    } else {

      payload_df <- ((pitching_payload %>%
        rvest::html_elements("table"))[5] %>%
        rvest::html_table() %>%
        as.data.frame())

      colnames(payload_df) <- payload_df[2,]

      payload_df <- payload_df[-c(1:2),]

      if('OPP DP' %in% colnames(payload_df) == TRUE) {

        payload_df <- payload_df %>%
          dplyr::rename(DP = .data$`OPP DP`)
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
        player_name = player_name) %>%
      dplyr::select(.data$player_id, .data$player_name, tidyr::everything())
    
  } else {

    if(type == 'batting') {

      payload_df <- ((batting_payload %>%
        rvest::html_elements('table'))[3] %>%
        rvest::html_table() %>%
        as.data.frame())[-1,]

      colnames(payload_df) <- payload_df[1,]

      payload_df <- payload_df[-1,]

      if('OPP DP' %in% colnames(payload_df) == TRUE) {

        payload_df <- payload_df %>%
          dplyr::rename(DP = .data$`OPP DP`)
      }

      payload_df <- payload_df %>%
        dplyr::select(
          .data$Year, .data$Team, .data$GP, .data$BA, .data$G, .data$OBPct,
          .data$SlgPct, .data$R, .data$AB, .data$H, .data$`2B`, .data$`3B`,
          .data$TB, .data$HR, .data$RBI, .data$BB, .data$HBP, .data$SF, .data$SH,
          .data$K, .data$DP, .data$CS, .data$Picked, .data$SB, .data$RBI2out)

      payload_df <- payload_df %>%
        dplyr::mutate(
          player_id = player_id,
          player_name = player_name) %>%
        dplyr::select(.data$Year, .data$player_id, .data$player_name, tidyr::everything())

    } 
    else {

      payload_df <- ((pitching_payload %>%
        rvest::html_elements('table'))[3] %>%
        rvest::html_table() %>%
        as.data.frame())[-1,]

      colnames(payload_df) <- payload_df[1,]

      payload_df <- payload_df[-1,]

      payload_df <- payload_df %>%
        dplyr::select(
          .data$Year,.data$Team,.data$GP,.data$G,
          .data$App,.data$GS,.data$ERA,.data$IP,.data$CG,.data$H,
          .data$R,.data$ER,.data$BB,.data$SO,.data$SHO,.data$BF,
          .data$`P-OAB`,.data$`2B-A`,.data$`3B-A`,.data$Bk,
          .data$`HR-A`,.data$WP,.data$HB,.data$IBB,
          .data$`Inh Run`,.data$`Inh Run Score`,
          .data$SHA,.data$SFA,.data$Pitches,.data$GO,.data$FO,
          .data$W,.data$L,.data$SV,.data$KL, tidyr::everything())

      
      payload_df <- payload_df %>%
        dplyr::mutate(
          player_id = player_id,
          player_name = player_name) %>%
        dplyr::select(.data$Year, .data$player_id, .data$player_name, tidyr::everything())
    }

  }

  return(payload_df)

}

