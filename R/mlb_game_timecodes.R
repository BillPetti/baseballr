#' @rdname mlb_game_timecodes
#' @title **Acquire time codes for Major and Minor League games**
#'
#' @param game_pk The game_pk for the game requested
#' @importFrom jsonlite fromJSON
#' @return Returns a tibble that includes time codes from the game_pk requested
#'
#'  |col_name  |types     |description                                          |
#'  |:---------|:---------|:----------------------------------------------------|
#'  |timecodes |character |Play snapshot time code in 'YYYYMMDD_HHMMSS' format.  |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_game_timecodes(game_pk = 632970))
#' }

mlb_game_timecodes <- function(game_pk) {
  
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1.1/game/{game_pk}/feed/live/timestamps"))
  
  timecodes <- NULL
  tryCatch(
    expr = {
      timecodes <- mlb_endpoint |>
        mlb_api_call() |>
        as.data.frame() |>
        # Rename by position: as.data.frame() of the timestamp vector names the
        # column after the piped expression, which varies by R version (was
        # ".", now "_data"), so a literal "." rename silently failed.
        dplyr::rename("timecodes" = 1) |>
        make_baseballr_data("MLB Game Timecodes data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(timecodes)
}
