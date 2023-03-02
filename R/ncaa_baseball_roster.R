#' @rdname ncaa_baseball_roster
#' @title **Get NCAA Baseball Rosters**
#' @param team_id NCAA id for a school
#' @param year The year of interest
#' @param ... Additional arguments passed to an underlying function like httr.
#' @return A data frame containing roster information, including
#' IDs and urls for each player (if available)
#' 
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |player_name   |character |
#'  |class         |character |
#'  |player_id     |character |
#'  |season        |numeric   |
#'  |number        |character |
#'  |position      |character |
#'  |player_url    |character |
#'  |team_name     |character |
#'  |conference    |character |
#'  |team_id       |numeric   |
#'  |division      |numeric   |
#'  |conference_id |numeric   |
#'  
#' @importFrom tibble tibble
#' @import rvest
#' @export
#' @examples
#' \donttest{
#'   try(ncaa_baseball_roster(team_id = 104, year = 2021))
#' }

ncaa_baseball_roster <- function(team_id = NA_integer_, year, ...){
  
  season_ids <- load_ncaa_baseball_season_ids()
  
  id <- season_ids %>% 
    dplyr::filter(.data$season == {{year}}) %>% 
    dplyr::select("id")
  
  ncaa_teams_lookup <- load_ncaa_baseball_teams()
  
  school_info <- ncaa_teams_lookup %>% 
    dplyr::filter(.data$team_id == {{team_id}} & .data$year == {{year}}) %>%
    dplyr::distinct()
  
  url <- paste0("https://stats.ncaa.org/team/", team_id, "/roster/", id)
  
  headers <- httr::add_headers(.headers = .ncaa_headers())
  
  tryCatch(
    expr = {
      roster_resp <- request_with_proxy(url = url, ..., headers)
      
      check_status(roster_resp)
      
      payload <- roster_resp %>% 
        httr::content(as = "text", encoding = "UTF-8") %>% 
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
#' @inheritParams ncaa_baseball_roster
#' @return A data frame containing roster information, including
#' IDs and urls for each player (if available)
#' @keywords legacy
#' @export
get_ncaa_baseball_roster <- ncaa_baseball_roster