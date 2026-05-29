#' @title **MLB Fielder Detail Types** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name  |types     |description                                       |
#'  |:---------|:---------|:-------------------------------------------------|
#'  |stat_name |character |Internal fielder detail stat name.                |
#'  |code      |character |Fielder detail type code.                         |
#'  |names     |list      |Associated detail names for the type.             |
#'  |chance    |logical   |Whether the detail counts as a fielding chance.   |
#'  |error     |logical   |Whether the detail counts as an error.            |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_fielder_detail_types())
#' }
mlb_fielder_detail_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/fielderDetailTypes")
  query_params <- list()
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  fielder_detail_types <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      fielder_detail_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame()
      fielder_detail_types$names <- lapply(fielder_detail_types$names, function(x){
        data.frame(names = ifelse(!is.character(x),NA_character_,x))}) |> 
        dplyr::bind_rows() |>
        make_baseballr_data("MLB Fielder Detail data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(fielder_detail_types)
}

