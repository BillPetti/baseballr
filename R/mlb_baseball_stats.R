#' @rdname mlb_baseball_stats
#' @title **MLB Baseball Stats** 
#' @return Returns a tibble with the following columns:
#' 
#'  |col_name          |types     |
#'  |:-----------------|:---------|
#'  |stat_name         |character |
#'  |stat_lookup_param |character |
#'  |is_counting       |logical   |
#'  |stat_label        |character |
#'  |stat_group        |character |
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names 
#' @importFrom dplyr rename 
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @importFrom tibble tibble 
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(mlb_baseball_stats())
#' }
mlb_baseball_stats <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/baseballStats")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      baseball_stats <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        tidyr::unnest(.data$stat_groups, names_sep = "_")
      baseball_stats <- baseball_stats[,1:5] %>% 
        dplyr::rename(
          stat_name = .data$name,
          stat_lookup_param = .data$lookup_param,
          stat_label = .data$label,
          stat_group = .data$stat_groups_displayName) %>%
        make_baseballr_data("MLB Baseball Stats data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(baseball_stats)
}

