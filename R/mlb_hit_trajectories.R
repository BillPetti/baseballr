#' @title **MLB Hit Trajectories** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name                   |types     |description                                                          |
#'  |:--------------------------|:---------|:--------------------------------------------------------------------|
#'  |hit_trajectory_code        |character |Hit trajectory code (e.g. 'bunt_grounder', 'bunt_popup').            |
#'  |hit_trajectory_description |character |Hit trajectory description (e.g. 'Bunt - Ground Ball', 'Bunt - Popup').|
#'
#' @export
#' @examples \donttest{
#'   try(mlb_hit_trajectories())
#' }
mlb_hit_trajectories <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/hitTrajectories")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  hit_trajectories <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      hit_trajectories <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "hit_trajectory_code" = "code",
          "hit_trajectory_description" = "description") |>
        make_baseballr_data("MLB Hit Trajectories data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(hit_trajectories)
}

