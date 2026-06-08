#' @title **MLB Pitch Codes** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name             |types     |description                                                  |
#'  |:--------------------|:---------|:------------------------------------------------------------|
#'  |pitch_code           |character |Single-character code identifying the pitch result.          |
#'  |pitch_description    |character |Human-readable description of the pitch code.                |
#'  |swing_status         |logical   |Whether the code represents a swing.                         |
#'  |swing_miss_status    |logical   |Whether the code represents a swinging strike (miss).        |
#'  |swing_contact_status |logical   |Whether the code represents a swing that made contact.       |
#'  |sort_order           |integer   |Display sort order for the pitch code.                       |
#'  |strike_status        |logical   |Whether the code counts as a strike.                         |
#'  |ball_status          |logical   |Whether the code counts as a ball.                           |
#'  |pitch_status         |logical   |Whether the code represents a pitch (vs. non-pitch event).   |
#'  |pitch_result_text    |character |Display text for the pitch result.                           |
#'  |bunt_attempt_status  |logical   |Whether the code represents a bunt attempt.                  |
#'  |contact_status       |logical   |Whether the code represents bat-ball contact.                |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_pitch_codes())
#' }
mlb_pitch_codes <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/pitchCodes")
  query_params <- list()
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  pitch_codes <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      pitch_codes <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "pitch_code" = "code",
          "pitch_description" = "description") |>
        make_baseballr_data("MLB Pitch Codes data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(pitch_codes)
}

