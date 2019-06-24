#' Get NCAA Baseball Game Logs
#'
#' @param player_id A player's unique id. Can be found using the
#' get_ncaa_baseball_roster function.
#' @param year The year of interest.
#' @param type The kind of statistics you want to return. Current options
#' are 'batting' or 'pitching'.
#'
#' @importFrom dplyr mutate mutate_at select slice bind_cols bind_rows
#' @importFrom rvest html_nodes html_attr html_table html_text
#' @importFrom tibble tibble
#' @importFrom xml2 read_html
#'
#' @return A data frame containing player and school information
#' as well as game by game statistics
#' @export
#'
#' @examples
#' \dontrun{
#' get_ncaa_game_logs(player_id = 1879650, year = 2019), type = "batting")
#' }

get_ncaa_game_logs <- function(player_id,
                               year = 2019,
                               type = "batting") {

  year_id <- subset(ncaa_season_id_lu, season == year, select = id)
  batting_id <- subset(ncaa_season_id_lu, season == year, select = batting_id)
  pitching_id <- subset(ncaa_season_id_lu, season == year, select = pitching_id)

  if (type == "batting") {

    url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", batting_id)

    payload <- xml2::read_html(url)

    payload_df <- payload %>%
      rvest::html_nodes("table") %>%
      .[4] %>%
      rvest::html_table(fill = TRUE) %>%
      as.data.frame() %>%
      .[,c(1:23)]

    names(payload_df) <- payload_df[3,]

    payload_df <- payload_df[-c(1:3),]

    cols_to_num <- c("G", "R", "AB", "H", "2B", "3B", "TB", "HR", "RBI",
                     "BB", "HBP", "SF", "SH", "K", "DP", "CS", "Picked",
                     "SB", "IBB", "RBI2out")

    payload_df <- payload_df %>%
      dplyr::mutate_at(cols_to_num, as.numeric)

  } else {

    url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", pitching_id)

    payload <- xml2::read_html(url)

    payload_df <- payload %>%
      rvest::html_nodes("table") %>%
      .[4] %>%
      rvest::html_table(fill = TRUE) %>%
      as.data.frame() %>%
      .[,c(1:35)]

    names(payload_df) <- payload_df[3,]

    payload_df <- payload_df[-c(1:3),]

    cols_to_num <- c("G", "App", "GS", "IP", "CG", "H", "R", "ER", "BB", "SO", "SHO", "BF", "P-OAB", "2B-A", "3B-A", "Bk", "HR-A", "WP", "HB", "IBB", "Inh Run", "Inh Run Score", "SHA", "SFA", "Pitches", "GO", "FO", "W", "L", "SV", "OrdAppeared", "KL")

    payload_df <- payload_df %>%
      dplyr::mutate_at(vars(-c("Date")),
                       list(~gsub("\\/", "", x = .))) %>%
      dplyr::mutate_at(cols_to_num, as.numeric)
  }

  return(payload_df)

}
