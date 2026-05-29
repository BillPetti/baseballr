#' @rdname mlb_game_content
#' @title **Retrieve additional game content for major and minor league games**
#' @param game_pk The unique game_pk identifier for the game
#' @return Returns a tibble of game content data with the following columns:
#' 
#'  |col_name            |types     |description                                       |
#'  |:-------------------|:---------|:-------------------------------------------------|
#'  |title               |character |Media/EPG title (e.g. MLBTV).                     |
#'  |call_letters        |character |Broadcaster call letters.                         |
#'  |espn_auth_required  |logical   |Whether ESPN authentication is required.          |
#'  |tbs_auth_required   |logical   |Whether TBS authentication is required.           |
#'  |espn2auth_required  |logical   |Whether ESPN2 authentication is required.         |
#'  |game_date           |character |Game date associated with the content.            |
#'  |content_id          |character |Content identifier.                               |
#'  |fs1auth_required    |logical   |Whether FS1 authentication is required.           |
#'  |media_id            |character |Media identifier.                                 |
#'  |media_feed_type     |character |Media feed type (HOME/AWAY/NATIONAL).             |
#'  |mlbn_auth_required  |logical   |Whether MLB Network authentication is required.   |
#'  |fox_auth_required   |logical   |Whether FOX authentication is required.           |
#'  |media_feed_sub_type |character |Media feed sub-type code.                         |
#'  |free_game           |logical   |Whether the broadcast is a free game.             |
#'  |epg_id              |integer   |Electronic programming guide identifier.          |
#'  |media_state         |character |Media state (e.g. MEDIA_ARCHIVE).                 |
#'  |abc_auth_required   |logical   |Whether ABC authentication is required.           |
#'  |rendition_name      |character |Media rendition name.                             |
#'  |description         |character |Content description.                              |
#'  |language            |character |Broadcast language.                               |
#'  |type                |character |Content/media type.                               |
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
#'   try(mlb_game_content(game_pk = 566001))
#' }

mlb_game_content <- function(game_pk) {
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/game/{game_pk}/content"))
  query_params <- list()
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  epg <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      media <- resp$media
      epg <- media$epg |> 
        tidyr::unnest("items") |> 
        as.data.frame() |> 
        janitor::clean_names() |> 
        dplyr::rename(
          "epg_id" = "id") |>
        make_baseballr_data("MLB Game Content data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(epg)
}
