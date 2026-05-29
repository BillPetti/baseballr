#' @title **Find MLB Seasons**
#'
#' @param sport_id The sport_id to return season information for.
#' @param with_game_type_dates with_game_type_dates to return season information
#' @importFrom jsonlite fromJSON
#' @return Returns a tibble with the following columns:
#' 
#'  |col_name                    |types     |description                                       |
#'  |:---------------------------|:---------|:-------------------------------------------------|
#'  |season_id                   |character |Season year identifier.                           |
#'  |has_wildcard                |logical   |Whether the season has a wild card round.         |
#'  |pre_season_start_date       |character |Pre-season start date.                            |
#'  |pre_season_end_date         |character |Pre-season end date.                              |
#'  |season_start_date           |character |Season start date.                                |
#'  |spring_start_date           |character |Spring training start date.                       |
#'  |spring_end_date             |character |Spring training end date.                         |
#'  |regular_season_start_date   |character |Regular season start date.                        |
#'  |last_date1st_half           |character |Last date of the first half.                      |
#'  |all_star_date               |character |All-Star Game date.                               |
#'  |first_date2nd_half          |character |First date of the second half.                    |
#'  |regular_season_end_date     |character |Regular season end date.                          |
#'  |post_season_start_date      |character |Post-season start date.                           |
#'  |post_season_end_date        |character |Post-season end date.                             |
#'  |season_end_date             |character |Season end date.                                  |
#'  |offseason_start_date        |character |Off-season start date.                            |
#'  |off_season_end_date         |character |Off-season end date.                              |
#'  |season_level_gameday_type   |character |Season-level Gameday data feed type.              |
#'  |game_level_gameday_type     |character |Game-level Gameday data feed type.                |
#'  |qualifier_plate_appearances |numeric   |Plate appearances per team game to qualify.       |
#'  |qualifier_outs_pitched      |integer   |Outs pitched per team game to qualify.            |
#'  
#' @export
#'
#' @examples \donttest{
#'  try(mlb_seasons(sport_id = 1))
#' }

mlb_seasons <- function(sport_id = 1,
                        with_game_type_dates = TRUE){
  mlb_endpoint <- mlb_stats_endpoint("v1/seasons")
  query_params <- list(
    withGameTypeDates = ifelse(with_game_type_dates==TRUE, 'true','false'),
    sportId = sport_id
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  
  seasons <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      
      seasons <- jsonlite::fromJSON(jsonlite::toJSON(resp$seasons),flatten = TRUE) |> 
        as.data.frame() |>
        janitor::clean_names() |>
        make_baseballr_data("MLB Seasons data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(seasons)
  
}
