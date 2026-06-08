#' @rdname mlb_baseball_stats
#' @title **MLB Baseball Stats** 
#' @return Returns a tibble with the following columns:
#' 
#'  |col_name          |types     |description                                      |
#'  |:-----------------|:---------|:------------------------------------------------|
#'  |stat_name         |character |Internal stat name.                              |
#'  |stat_lookup_param |character |Lookup parameter/abbreviation for the stat.      |
#'  |is_counting       |logical   |Whether the stat is a counting stat.             |
#'  |stat_label        |character |Human-readable stat label.                       |
#'  |stat_group        |character |Stat group (e.g. hitting, pitching, fielding).   |
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
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  baseball_stats <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      baseball_stats <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        tidyr::unnest("stat_groups", names_sep = "_")
      baseball_stats <- baseball_stats[,1:5] |> 
        dplyr::rename(
          "stat_name" = "name",
          "stat_lookup_param" = "lookup_param",
          "stat_label" = "label",
          "stat_group" = "stat_groups_displayName") |>
        make_baseballr_data("MLB Baseball Stats data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(baseball_stats)
}

