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
#' @examples \dontrun{
#' get_ncaa_game_logs(player_id = 1879650, year = 2019), type = "batting")
#' }

get_ncaa_game_logs <- function(player_id,
                               year = 2019,
                               type = "batting",
                               span = 'game') {

  year_id <- subset(ncaa_season_id_lu, season == year, select = id)
  batting_id <- subset(ncaa_season_id_lu, season == year, select = batting_id)
  pitching_id <- subset(ncaa_season_id_lu, season == year, select = pitching_id)

  if (type == "batting") {

    batting_url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", batting_id)
    batting_payload <- xml2::read_html(batting_url)
  } else {

    pitching_url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", pitching_id)
    pitching_payload <- xml2::read_html(pitching_url)
  }

  if (span == 'game') {

    if (type == "batting") {

      payload_df <- batting_payload %>%
        rvest::html_nodes("table") %>%
        .[5] %>%
        rvest::html_table(fill = TRUE) %>%
        as.data.frame() %>%
        .[,c(1:23)]

      names(payload_df) <- payload_df[2,]

      payload_df <- payload_df[-c(1:2),]

      payload_df <- payload_df %>%
        mutate_at(vars(G:RBI2out), extract_numeric)

      if('OPP DP' %in% colnames(payload_df) == TRUE) {

        payload_df <- payload_df %>%
          dplyr::rename(DP = `OPP DP`)
      }

      cols_to_num <- c("G", "R", "AB", "H", "2B", "3B", "TB", "HR", "RBI",
                       "BB", "HBP", "SF", "SH", "K", "DP", "CS", "Picked",
                       "SB", "IBB", "RBI2out")

      payload_df <- payload_df %>%
        dplyr::mutate_at(cols_to_num, as.numeric)

    } else {

      payload_df <- pitching_payload %>%
        rvest::html_nodes("table") %>%
        .[5] %>%
        rvest::html_table(fill = TRUE) %>%
        as.data.frame() %>%
        .[,c(1:35)]

      names(payload_df) <- payload_df[2,]

      payload_df <- payload_df[-c(1:2),]

      if('OPP DP' %in% colnames(payload_df) == TRUE) {

        payload_df <- payload_df %>%
          dplyr::rename(DP = `OPP DP`)
      }

      cols_to_num <- c("G", "App", "GS", "IP", "CG", "H", "R", "ER", "BB", "SO", "SHO", "BF", "P-OAB", "2B-A", "3B-A", "Bk", "HR-A", "WP", "HB", "IBB", "Inh Run", "Inh Run Score", "SHA", "SFA", "Pitches", "GO", "FO", "W", "L", "SV", "OrdAppeared", "KL")

      payload_df <- payload_df %>%
        dplyr::mutate_at(vars(-c("Date")),
                         list(~gsub("\\/", "", x = .))) %>%
        dplyr::mutate_at(cols_to_num, as.numeric)
    }

  } else {

    if(type == 'batting') {

      payload_df <- batting_payload %>%
        rvest::html_nodes('table') %>%
        .[3] %>%
        rvest::html_table(fill = T) %>%
        as.data.frame() %>%
        .[-1,]

      names(payload_df) <- payload_df[1,]

      payload_df <- payload_df[-1,]

      if('OPP DP' %in% colnames(payload_df) == TRUE) {

        payload_df <- payload_df %>%
          dplyr::rename(DP = `OPP DP`)
      }

      payload_df <- payload_df %>%
        dplyr::select(Year,Team,GP,BA,G,OBPct,SlgPct,R,AB,H,`2B`,`3B`,TB,HR,RBI,BB,HBP,SF,SH,K,DP,CS,Picked,SB,RBI2out)

      payload_df <- payload_df %>%
        dplyr::mutate(player_id = player_id) %>%
        dplyr::select(Year, player_id, everything())

    } else {

      payload_df <- pitching_payload %>%
        rvest::html_nodes('table') %>%
        .[3] %>%
        rvest::html_table(fill = T) %>%
        as.data.frame() %>%
        .[-1,]

      names(payload_df) <- payload_df[1,]

      payload_df <- payload_df[-1,]

      payload_df <- payload_df %>%
        dplyr::select(Year,Team,GP,G,App,GS,ERA,IP,CG,H,R,ER,BB,SO,SHO,BF,`P-OAB`,`2B-A`,`3B-A`,Bk,`HR-A`,WP,HB,IBB,`Inh Run`,`Inh Run Score`,SHA,SFA,Pitches,GO,FO,W,L,SV,KL)

      payload_df <- payload_df %>%
        dplyr::mutate(player_id = player_id) %>%
        dplyr::select(Year, player_id, everything())
    }

  }

  return(payload_df)

}

