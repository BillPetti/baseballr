#' Get NCAA Baseball Rosters
#'
#' @param teamid NCAA id for a school
#' @param year The year of interest
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
#' \dontrun{
#' get_ncaa_baseball_roster(104, 2019)
#' }

get_ncaa_baseball_roster <- function(teamid = NA,
                                     year = 2019) {

  id <- baseballr::ncaa_season_id_lu %>% 
    dplyr::filter(.data$season == year) %>% 
    dplyr::select(.data$id)

  school_info <- baseballr::master_ncaa_team_lu %>% 
    dplyr::filter(.data$school_id == teamid & .data$year == year) %>%
    dplyr::select(-.data$year) %>%
    dplyr::distinct()

  url <- paste0("https://stats.ncaa.org/team/", teamid, "/roster/", id)

  payload <- xml2::read_html(url)

  payload <- (payload %>%
    rvest::html_nodes("table"))[1] %>%
    rvest::html_nodes("tr")

  parse_roster_table <- function(trs) {

    (number <- trs %>%
      rvest::html_nodes("td"))[1] %>%
      rvest::html_text()

    (name <- trs %>%
      rvest::html_nodes("td"))[2] %>%
      rvest::html_text()

    (position <- trs %>%
      rvest::html_nodes("td"))[3] %>%
      rvest::html_text()

    (class <- trs %>%
      rvest::html_nodes("td"))[4] %>%
      rvest::html_text()

    (url_slug <- trs %>%
        rvest::html_nodes("td"))[2] %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")

    url_slug <- ifelse(url_slug %>%
                         as.data.frame() %>%
                         dplyr::rename(var = .data$`.`) %>%
                         nrow() == 0, NA, url_slug)

    player_id <- gsub(".*stats_player_seq=\\s*", "", url_slug)

    payload <- tibble::tibble(name = name,
                              class = class,
                              player_id = player_id,
                              number = number,
                              position = position,
                              url_slug = url_slug)

    payload$url_slug <- as.character(payload$url_slug)

    return(payload)
  }

  roster <- lapply(payload, function(x) parse_roster_table(x)) %>%
    dplyr::bind_rows()

  roster <- roster %>%
    dplyr::mutate(season = -.data$year,
                  player_url = ifelse(is.na(-.data$player_id), NA, paste0("https://stats.ncaa.org", -.data$url_slug))) %>%
    dplyr::select(-.data$name, -.data$class, -.data$player_id, -.data$season, 
                  -.data$number, -.data$position, -.data$player_url)

  school_info <- school_info %>%
    dplyr::slice(rep(1:n(), each = nrow(roster)))

  roster <- dplyr::bind_cols(roster, school_info)

  return(roster)

  Sys.sleep(sample(seq(.05, 5, .01), 1))
}
