#' Get NCAA Baseball Rosters
#'
#' @param teamid NCAA id for a school
#' @param team_year The year of interest
#'
#' @importFrom rvest html_nodes html_node html_attr html_table html_text
#' @importFrom tibble tibble
#' @importFrom xml2 read_html
#'
#' @return A data frame containing roster information, including
#' IDs and urls for each player (if available)
#' @export
#'
#' @examples
#' \donttest{
#' get_ncaa_baseball_roster(teamid = 104, team_year =2019)
#' }

get_ncaa_baseball_roster <- function(teamid = NA,
                                     team_year = 2019) {

  id <- baseballr::ncaa_season_id_lu %>% 
    dplyr::filter(.data$season == team_year) %>% 
    dplyr::select(.data$id)

  school_info <- baseballr::master_ncaa_team_lu %>% 
    dplyr::filter(.data$school_id == teamid & .data$year == team_year) %>%
    dplyr::select(-.data$year) %>%

    dplyr::distinct()

  url <- paste0("https://stats.ncaa.org/team/", teamid, "/roster/", id)

  payload <- xml2::read_html(url)

  payload1 <- (payload %>%
    rvest::html_nodes("table"))[[1]] %>%
    rvest::html_nodes("tr")
  payload_table <- (payload %>%
                 rvest::html_nodes("table"))[[1]] %>%
    rvest::html_table()
  colnames(payload_table) <- payload_table[1,]
  payload_table <- payload_table[-1,]
  url_slug <- lapply(payload1, function(x){
    (url_slug <- x %>%
       rvest::html_nodes("td"))[2] %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>% 
      as.data.frame() }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::rename(url_slug = .data$`.`)
  
  roster <- dplyr::bind_cols(payload_table, url_slug) %>% 
    dplyr::rename(
      name = .data$Player,
      class = .data$Yr,
      position = .data$Pos,
      games_played = .data$GP,
      games_started = .data$GS,
      number = .data$Jersey)
  roster <- roster %>%
    dplyr::mutate(
      season = team_year,
      player_id = gsub(".*stats_player_seq=\\s*", "", .data$url_slug),
      player_url = ifelse(is.na(.data$player_id), NA, paste0("https://stats.ncaa.org", .data$url_slug)),
    ) %>%
    dplyr::select(.data$name, .data$class, .data$player_id, .data$season, 
                  .data$number, .data$position, .data$player_url)

  school_info <- school_info %>%
    dplyr::slice(rep(1:n(), each = nrow(roster)))

  roster <- dplyr::bind_cols(roster, school_info)

  return(roster)

  Sys.sleep(sample(seq(.05, 5, .01), 1))
}
