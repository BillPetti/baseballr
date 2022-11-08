#' @title **MLB Stat Groups** 
#' @return Returns a tibble with the following columns
#'  |col_name               |types     |
#'  |:----------------------|:---------|
#'  |stat_group_name        |character |
#' @export
#' @examples \donttest{
#'   try(mlb_stat_groups())
#' }
mlb_stat_groups <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/statGroups")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      stat_groups <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          "stat_group_name" = "display_name") %>%
        make_baseballr_data("MLB Stat Groups data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(stat_groups)
}

