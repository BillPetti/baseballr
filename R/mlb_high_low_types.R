#' @title **MLB Stat High/Low Types** 
#' @return Returns a tibble with the following columns
#'  |col_name          |types     |
#'  |:-----------------|:---------|
#'  |stat_name         |character |
#'  |stat_lookup_param |character |
#'  |is_counting       |logical   |
#'  |stat_label        |character |
#'  |stat_groups       |list      |
#'  |org_types         |list      |
#'  |high_low_types    |list      |
#' @export
#' @examples \donttest{
#'   try(mlb_high_low_types())
#' }
mlb_high_low_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/highLow/types")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      high_low_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        tidyr::unnest_wider("stat_groups", names_sep = "_") %>% 
        dplyr::rename(
          "stat_name" = "name",
          "stat_lookup_param" = "lookup_param",
          "stat_label" = "label",
          "stat_groups" = "stat_groups_displayName") %>% 
        dplyr::select(-"streak_levels")  %>%
        make_baseballr_data("MLB High Low Types data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  return(high_low_types)
}

