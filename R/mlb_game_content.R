#' @rdname mlb_game_content
#' @title **Retrieve additional game content for major and minor league games**
#' @param game_pk The unique game_pk identifier for the game
#' @return Returns a data frame with the following columns
#'  |col_name            |types     |
#'  |:-------------------|:---------|
#'  |title               |character |
#'  |epg_id              |integer   |
#'  |content_id          |character |
#'  |media_id            |character |
#'  |media_state         |character |
#'  |media_feed_type     |character |
#'  |media_feed_sub_type |character |
#'  |call_letters        |character |
#'  |fox_auth_required   |logical   |
#'  |tbs_auth_required   |logical   |
#'  |espn_auth_required  |logical   |
#'  |fs1auth_required    |logical   |
#'  |mlbn_auth_required  |logical   |
#'  |free_game           |logical   |
#'  |type                |character |
#'  |description         |character |
#'  |rendition_name      |character |
#'  |language            |character |
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr spread
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @export
#' @examples \donttest{
#'   try(mlb_game_content(game_pk = 566001))
#' }

mlb_game_content <- function(game_pk) {
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/game/{game_pk}/content"))
  query_params <- list()
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      media <- resp$media
      epg <- media$epg %>% 
        tidyr::unnest(.data$items) %>% 
        as.data.frame() %>% 
        janitor::clean_names() %>% 
        dplyr::rename(
          epg_id =.data$id
        ) %>%
        make_baseballr_data("MLB Game Content data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(epg)
}
