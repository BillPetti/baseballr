#' @rdname ncaa_roster
#' @title **Get NCAA Baseball Rosters**
#' @param team_id NCAA id for a school
#' @param year The year of interest
#' @param ... Additional arguments passed to an underlying function like httr.
#' @return A data frame containing roster information, including
#' IDs and urls for each player (if available)
#' 
#'  |col_name      |types     |description                                  |
#'  |:-------------|:---------|:--------------------------------------------|
#'  |player_name   |character |Player name.                                 |
#'  |class         |character |Academic class/year (Fr, So, Jr, Sr).        |
#'  |player_id     |character |stats.ncaa.org player identifier.            |
#'  |season        |numeric   |Season (4-digit year).                       |
#'  |number        |character |Jersey number.                               |
#'  |position      |character |Primary fielding position.                   |
#'  |player_url    |character |Full stats.ncaa.org url for the player page. |
#'  |team_name     |character |Team name.                                   |
#'  |conference    |character |Conference name.                             |
#'  |team_id       |numeric   |Team NCAA id.                                |
#'  |division      |numeric   |NCAA division (1, 2, 3).                     |
#'  |conference_id |numeric   |Conference identifier.                       |
#'  
#' @importFrom tibble tibble
#' @import rvest
#' @details
#' Live usage (reads `stats.ncaa.org`, which is behind Akamai bot protection and
#' needs the optional `chromote` + Google Chrome browser fallback, so it is shown
#' here rather than as a runnable example):
#'
#' ```r
#' ncaa_roster(team_id = 104, year = 2023)
#' ```
#' @export

ncaa_roster <- function(team_id = NULL, year, ...){
  if (is.null(team_id)) {
    cli::cli_abort("Enter valid team_id")
  }
  if (is.null(year)) {
    cli::cli_abort("Enter valid year as a number (YYYY)")
  }
  
  season_ids <- load_ncaa_baseball_season_ids()
  
  id <- season_ids %>% 
    dplyr::filter(.data$season == {{year}}) %>% 
    dplyr::select("id")
  
  ncaa_teams_lookup <- load_ncaa_baseball_teams()
  
  school_info <- ncaa_teams_lookup %>% 
    dplyr::filter(.data$team_id == {{team_id}} & .data$year == {{year}}) %>%
    dplyr::distinct()
  
  url <- paste0("https://stats.ncaa.org/team/", team_id, "/roster/", id)
  
  
  tryCatch(
    expr = {
      roster_resp <- request_with_proxy(url = url, ...)
      
      check_status(roster_resp)
      
      payload <- roster_resp %>% 
        httr2::resp_body_string() %>% 
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
      
      roster <- payload_table %>% 
        dplyr::bind_cols(url_slug) %>% 
        dplyr::rename(
          "player_name" = "Player",
          "class" = "Yr",
          "position" = "Pos",
          "games_played" = "GP",
          "games_started" = "GS",
          "number" = "Jersey")
      roster <- roster %>%
        dplyr::mutate(
          season =  {{year}},
          player_id = gsub(".*stats_player_seq=\\s*", "", .data$url_slug),
          player_url = ifelse(is.na(.data$player_id), NA, paste0("https://stats.ncaa.org", .data$url_slug))) %>%
        dplyr::select(
          "player_name", 
          "class", 
          "player_id", 
          "season",
          "number", 
          "position", 
          "player_url")
      
      school_info <- school_info %>%
        dplyr::slice(rep(1:n(), each = nrow(roster)))
      
      roster <- roster %>% 
        dplyr::bind_cols(school_info) %>%
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
#' @inheritParams ncaa_roster
#' @inherit ncaa_roster return
#' @keywords legacy
#' @export
ncaa_baseball_roster <- ncaa_roster

#' @rdname get_ncaa_baseball_roster
#' @title **(legacy) Get NCAA Baseball Rosters**
#' @inheritParams ncaa_baseball_roster
#' @inherit ncaa_roster return
#' @keywords legacy
#' @export
get_ncaa_baseball_roster <- ncaa_baseball_roster