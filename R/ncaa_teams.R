#' @title **Scrape NCAA baseball Teams (Division I, II, and III)**
#' @description This function allows the user to obtain NCAA teams by year and division
#' @param year The season for which data should be returned, in the form of "YYYY". Years currently available: 2002 onward.
#' @param division Division - 1, 2, 3
#' @param ... Additional arguments passed to an underlying function like httr.
#' @return A data frame with the following variables
#' 
#'    |col_name      |types     |
#'    |:-------------|:---------|
#'    |team_id       |character |
#'    |team_name     |character |
#'    |team_url      |character |
#'    |conference_id |character |
#'    |conference    |character |
#'    |division      |numeric   |
#'    |year          |numeric   |
#'    |season_id     |character |
#'    
#' @import dplyr
#' @import rvest
#' @importFrom stringr str_split
#' @export
#' @details 
#' ```r
#' ncaa_teams(2023, 1)
#' ```

ncaa_teams <- function(year = most_recent_ncaa_baseball_season(), division = 1, ...) {
  
  if (year < 2002) {
    stop('you must provide a year that is equal to or greater than 2002')
  }
  
  df <- data.frame()
  
  headers <- httr::add_headers(.headers = .ncaa_headers())
  tryCatch(
    expr = {
      
      
      url <- paste0("http://stats.ncaa.org/team/inst_team_list?academic_year=",
                    year,
                    "&conf_id=-1",
                    "&division=", division,
                    "&sport_code=MBA")
      
      resp <- request_with_proxy(url = url, ..., headers)
      
      data_read <- resp %>% 
        httr::content(as = "text", encoding = "UTF-8") %>% 
        xml2::read_html()      
      
      team_urls <- data_read %>% 
        rvest::html_elements("table") %>% 
        rvest::html_elements("a") %>% 
        rvest::html_attr("href")
      
      team_names <- data_read %>% 
        rvest::html_elements("table") %>% 
        rvest::html_elements("a") %>% 
        rvest::html_text()
      
      conference_names <- ((data_read %>% 
                        rvest::html_elements(".level2"))[[4]] %>% 
                       rvest::html_elements("a") %>% 
                       rvest::html_text())[-1]
      
      conference_ids <- (data_read %>% 
                           rvest::html_elements(".level2"))[[4]] %>% 
        rvest::html_elements("a") %>% 
        rvest::html_attr("href") %>% 
        stringr::str_extract("javascript:changeConference\\(\\d+\\)") %>% 
        stringr::str_subset("javascript:changeConference\\(\\d+\\)") %>% 
        stringr::str_extract("\\d+")
      
      conference_df <- data.frame(conference = conference_names, conference_id = conference_ids)
      
      conferences_team_df <- lapply(conference_df$conference_id, function(x){
        conf_team_urls <- paste0("http://stats.ncaa.org/team/inst_team_list?academic_year=",
                                 year,
                                 "&conf_id=", x,
                                 "&division=", division,
                                 "&sport_code=MBA")
        resp <- request_with_proxy(url = conf_team_urls, ..., headers)
        
        team_urls <- resp %>% 
          httr::content(as = "text", encoding = "UTF-8") %>% 
          xml2::read_html() %>% 
          rvest::html_elements("table") %>% 
          rvest::html_elements("a") %>% 
          rvest::html_attr("href")
        
        team_names <- resp %>% 
          httr::content(as = "text", encoding = "UTF-8") %>% 
          xml2::read_html() %>% 
          rvest::html_elements("table") %>% 
          rvest::html_elements("a") %>% 
          rvest::html_text()
        
        data <- data.frame(team_url = team_urls, 
                           team_name = team_names, 
                           division = division, 
                           year = year,
                           conference_id = x)
        data <- data %>% 
          dplyr::left_join(conference_df, by = c("conference_id"))
        Sys.sleep(5)
        return(data)
      })
      
      conferences_team_df <- rbindlist_with_attrs(conferences_team_df)
      
      conferences_team_df$team_id <- conferences_team_df$team_url %>% 
        stringr::str_extract("(\\d+)\\/(\\d+)", group = 1)
      
      conferences_team_df$season_id <- conferences_team_df$team_url %>% 
        stringr::str_extract("(\\d+)\\/(\\d+)", group = 2)
      
      df <- as.data.frame(conferences_team_df)
      
      df <- df %>%
        dplyr::select(
          "team_id", 
          "team_name",
          "team_url", 
          "conference_id",
          "conference", 
          "division",
          "year",
          "season_id") %>% 
        make_baseballr_data("NCAA Baseball Teams data from stats.ncaa.org",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(df)
}
