#' @rdname mlb_pbp_timecodes
#' @title **Acquire time codes for Major and Minor League games**
#'
#' @param game_pk The date for which you want to find game_pk values for MLB games
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes time codes from the game_pk requested
#' 
#'  |col_name                       |types     |
#'  |:------------------------------|:---------|
#'  |timecodes  (MMDDYYYY_HHMMSS)   |numeric   |
#'  
#' @export
#' @examples \donttest{
#'   mlb_pbp_timecodes(game_pk = 632970)
#' }

mlb_pbp_timecodes <- function(game_pk) {
  
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1.1/game/{game_pk}/feed/live/timestamps"))
  
  timecodes <- mlb_endpoint %>% 
    mlb_api_call() %>% 
    as.data.frame() %>% 
    dplyr::rename(timecodes = .data$.)
  
  return(timecodes)
}
