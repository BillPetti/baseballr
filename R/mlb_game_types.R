#' @title **MLB Game Types** 
#' @return Returns a tibble with the following columns
#'  |col_name              |types     |
#'  |:---------------------|:---------|
#'  |game_type_id          |character |
#'  |game_type_description |character |
#' @export
#' @examples \donttest{
#'   try(mlb_game_types())
#' }
mlb_game_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/gameTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      game_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          game_type_id = .data$id,
          game_type_description = .data$description) %>%
        make_baseballr_data("MLB Game Types data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(game_types)
}

