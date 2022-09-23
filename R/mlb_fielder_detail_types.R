#' @title **MLB Fielder Detail Types** 
#' @return Returns a tibble with the following columns
#'  |col_name  |types     |
#'  |:---------|:---------|
#'  |stat_name |character |
#'  |code      |character |
#'  |names     |character |
#'  |chance    |logical   |
#'  |error     |logical   |
#' @export
#' @examples \donttest{
#'   try(mlb_fielder_detail_types())
#' }
mlb_fielder_detail_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/fielderDetailTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      fielder_detail_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame()
      fielder_detail_types$names <- lapply(fielder_detail_types$names, function(x){
        data.frame(names = ifelse(!is.character(x),NA_character_,x))}) %>% 
        dplyr::bind_rows() %>%
        make_baseballr_data("MLB Fielder Detail data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(fielder_detail_types)
}

