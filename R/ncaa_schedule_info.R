#' @rdname ncaa_schedule_info
#' @title **Get Schedule and Results for NCAA Baseball Teams**
#' @param teamid The team's unique NCAA id.
#' @param year The season (i.e. use 2016 for the 2015-2016 season,
#' etc.)
#' @return A data frame with the following fields: date, opponent,
#' result, score, innings (if more than regulation), and the url
#' for the game itself.
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |date          |character |
#'  |opponent      |character |
#'  |result        |character |
#'  |score         |character |
#'  |innings       |character |
#'  |opponent_slug |character |
#'  |slug          |character |
#'  |game_info_url |character |
#' @importFrom tibble tibble rownames_to_column
#' @importFrom tidyr separate
#' @importFrom stringr str_trim str_extract
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(ncaa_schedule_info(teamid = 736, year = 2021))
#' }

ncaa_schedule_info <- function(teamid = NULL, year = NULL){
  
  id <- subset(baseballr::ncaa_season_id_lu, baseballr::ncaa_season_id_lu$season == year, select = id)
  
  school_info <- baseballr::ncaa_team_lu %>% 
    dplyr::filter(.data$school_id == teamid, .data$year == year) %>%
    dplyr::select(-.data$year) %>%
    dplyr::distinct()
  
  url <- paste0("https://stats.ncaa.org/team/", teamid, "/", id)
  
  tryCatch(
    expr={
      payload <- url %>% xml2::read_html()
      
      
      
      if(year>2018){
        sched_html <- payload %>%
          rvest::html_elements("fieldset") %>%
          rvest::html_elements("table") 
        sched_1 <- (payload %>%
                      rvest::html_elements("fieldset") %>%
                      rvest::html_elements("table")) [[1]] %>%
          rvest::html_elements("tr")
        sched_1 <- sched_1[2:length(sched_1)]
        sched_1 <- sched_1[c(seq(1,length(sched_1),2))]
        sched <- sched_html %>%
          rvest::html_table() %>%
          as.data.frame() 
        sched <- sched %>%
          dplyr::filter(.data$Date != "") %>% 
          dplyr::select(-dplyr::any_of("Attendance"))
      }else{
        sched_html <- payload %>% 
          rvest::html_element("td:nth-child(1) > table") 
        sched_1 <- (payload  %>% 
                      rvest::html_element("td:nth-child(1) > table")) %>%
          rvest::html_elements("tr")
        sched_1 <- sched_1[3:length(sched_1)]
        sched <- sched_html %>%
          rvest::html_table() %>%
          as.data.frame() 
        colnames(sched) <- c("Date", "Opponent", "Result") 
        sched <- sched[3:nrow(sched),]   
      }
      
      
      sched <- sched %>%
        dplyr::filter(.data$Date != "")
      
      game_extractor <- function(x){
        data.frame(slug = ifelse(
          is.null(
            (x %>%
               rvest::html_elements("td:nth-child(3)") %>% 
               rvest::html_elements("a.skipMask"))), 
          NA_character_,
          (x %>%
             rvest::html_elements("td:nth-child(3)") %>% 
             rvest::html_elements("a.skipMask")) %>% 
            html_attr("href")
        ))
      }
      slugs <- lapply(sched_1, game_extractor) %>% 
        dplyr::bind_rows() 
      
      sched$opponent_slug <- sched_html %>%
        rvest::html_elements("td:nth-child(2)")%>%
        rvest::html_element("a") %>%
        rvest::html_attr("href")
      sched <- dplyr::bind_cols(sched, slugs)
      sched <- sched %>%
        dplyr::filter(!(.data$Result %in% c("Canceled","Ppd")))
      
      sched <- sched %>%
        dplyr::mutate(
          Date = substr(.data$Date,1,10),
          game_info_url = ifelse(!is.na(.data$slug), paste0("https://stats.ncaa.org", .data$slug), NA_character_))
      
      suppressWarnings(
        sched <- sched %>%
          tidyr::separate(.data$Result, into = c("Result", "Score", "Innings"),
                          sep = " ") %>%
          # dplyr::filter(.data$Result != "RPI") %>% 
          dplyr::mutate(
            Innings = stringr::str_extract(.data$Innings,"\\d+")
          )
        
      )
      sched <- sched %>% 
        janitor::clean_names() %>%
        make_baseballr_data("NCAA Baseball Schedule Info data from stats.ncaa.org",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(sched)
}

#' @rdname get_ncaa_schedule_info
#' @title **(legacy) Get Schedule and Results for NCAA Baseball Teams**
#' @inheritParams ncaa_schedule_info
#' @return A data frame with the following fields: date, opponent,
#' result, score, innings (if more than regulation), and the url
#' for the game itself.
#' @keywords legacy
#' @export
get_ncaa_schedule_info <- ncaa_schedule_info