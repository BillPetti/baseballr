#' @rdname ncaa_baseball_roster
#' @title **Get NCAA Baseball Rosters**
#' @param teamid NCAA id for a school
#' @param team_year The year of interest
#' @return A data frame containing roster information, including
#' IDs and urls for each player (if available)
#' @importFrom tibble tibble
#' @import rvest
#' @export
#' @examples
#' \donttest{
#'   ncaa_baseball_roster(teamid = 104, team_year = 2021)
#' }

ncaa_baseball_roster <- function(teamid = NA, team_year){

  id <- baseballr::ncaa_season_id_lu %>% 
    dplyr::filter(.data$season == team_year) %>% 
    dplyr::select(.data$id)

  school_info <- baseballr::ncaa_team_lu %>% 
    dplyr::filter(.data$school_id == teamid & .data$year == team_year) %>%
    dplyr::select(-.data$year) %>%
    dplyr::distinct()

  url <- paste0("https://stats.ncaa.org/team/", teamid, "/roster/", id)

  payload <- url %>% 
    xml2::read_html()

  payload1 <- (payload %>%
    rvest::html_elements("table"))[[1]] %>%
    rvest::html_elements("tr")
  
  payload_table <- (payload %>%
                 rvest::html_elements("table"))[[1]] %>%
    rvest::html_table()
  
  colnames(payload_table) <- payload_table[1,]
  
  payload_table <- payload_table[-1,]
  
  url_slug <- lapply(payload1, function(x){
    (url_slug <- x %>%
       rvest::html_elements("td"))[2] %>%
      rvest::html_elements("a") %>%
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
      player_url = ifelse(is.na(.data$player_id), NA, paste0("https://stats.ncaa.org", .data$url_slug))) %>%
    dplyr::select(.data$name, .data$class, .data$player_id, .data$season, 
                  .data$number, .data$position, .data$player_url)

  school_info <- school_info %>%
    dplyr::slice(rep(1:n(), each = nrow(roster)))

  roster <- dplyr::bind_cols(roster, school_info)

  return(roster)
}

#' @rdname get_ncaa_baseball_roster
#' @title **(legacy) Get NCAA Baseball Rosters**
#' @inheritParams ncaa_baseball_roster
#' @return A data frame containing roster information, including
#' IDs and urls for each player (if available)
#' @keywords legacy
#' @export
get_ncaa_baseball_roster <- ncaa_baseball_roster