#' @rdname ncaa_baseball_roster
#' @title **Get NCAA Baseball Rosters**
#' @param teamid NCAA id for a school
#' @param team_year The year of interest
#' @return A data frame containing roster information, including
#' IDs and urls for each player (if available)
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |name          |character |
#'  |class         |character |
#'  |player_id     |character |
#'  |season        |numeric   |
#'  |number        |character |
#'  |position      |character |
#'  |player_url    |character |
#'  |school        |character |
#'  |conference    |character |
#'  |school_id     |numeric   |
#'  |division      |numeric   |
#'  |conference_id |numeric   |
#' @importFrom tibble tibble
#' @import rvest
#' @export
#' @examples
#' \donttest{
#'   try(ncaa_baseball_roster(teamid = 104, team_year = 2021))
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
  
  tryCatch(
    expr={
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
      payload1 <- payload1[c(3:length(payload1))]
      extractor <- function(x){
        data.frame(url_slug = ifelse(
          is.null(
            (x %>%
               rvest::html_elements("td"))[2] %>% 
              rvest::html_element("a")), 
          NA_character_,
          (x %>%
             rvest::html_elements("td"))[2] %>% 
            rvest::html_element("a")  %>% 
            html_attr("href")
        ))
      }
      url_slug <- lapply(payload1, extractor) %>% 
        dplyr::bind_rows() 
      
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
      
      roster <- dplyr::bind_cols(roster, school_info) %>%
        make_baseballr_data("NCAA Baseball Roster data from stats.ncaa.org",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
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