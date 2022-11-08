#' @title **MLB Awards** 
#' @return Returns a tibble with the following columns
#'  |col_name          |types     |
#'  |:-----------------|:---------|
#'  |award_id          |character |
#'  |award_name        |character |
#'  |award_description |character |
#'  |sort_order        |integer   |
#'  |notes             |character |
#'  |sport_id          |integer   |
#'  |sport_link        |character |
#'  |league_id         |integer   |
#'  |league_link       |character |
#' @export
#' @examples \donttest{
#'   try(mlb_awards())
#' }
mlb_awards <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/awards")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  awards <- jsonlite::fromJSON(jsonlite::toJSON(resp$awards), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    dplyr::rename(
      "award_id" = "id",
      "award_name" = "name",
      "award_description" = "description") %>%
    make_baseballr_data("MLB Awards data from MLB.com",Sys.time())
  
    },
  error = function(e) {
    message(glue::glue("{Sys.time()}: Invalid arguments provided"))
  },
  finally = {
  }
  )
  return(awards)
}
