#' @title **Find MLB All-Star Ballots**
#' @param league_id League ID for league all-star ballot of interest.
#' @param season The season of the all-star ballot.
#' @return Returns a data frame with the following columns:
#'   |col_name                      |types     |
#'   |:-----------------------------|:---------|
#'   |player_id                     |integer   |
#'   |full_name                     |character |
#'   |link                          |character |
#'   |first_name                    |character |
#'   |last_name                     |character |
#'   |primary_number                |character |
#'   |birth_date                    |character |
#'   |current_age                   |integer   |
#'   |birth_city                    |character |
#'   |birth_state_province          |character |
#'   |birth_country                 |character |
#'   |height                        |character |
#'   |weight                        |integer   |
#'   |active                        |logical   |
#'   |use_name                      |character |
#'   |middle_name                   |character |
#'   |boxscore_name                 |character |
#'   |nick_name                     |character |
#'   |gender                        |character |
#'   |is_player                     |logical   |
#'   |is_verified                   |logical   |
#'   |draft_year                    |integer   |
#'   |mlb_debut_date                |character |
#'   |name_first_last               |character |
#'   |name_slug                     |character |
#'   |first_last_name               |character |
#'   |last_first_name               |character |
#'   |last_init_name                |character |
#'   |init_last_name                |character |
#'   |full_fml_name                 |character |
#'   |full_lfm_name                 |character |
#'   |strike_zone_top               |numeric   |
#'   |strike_zone_bottom            |numeric   |
#'   |pronunciation                 |character |
#'   |name_matrilineal              |character |
#'   |name_title                    |character |
#'   |primary_position_code         |character |
#'   |primary_position_name         |character |
#'   |primary_position_type         |character |
#'   |primary_position_abbreviation |character |
#'   |bat_side_code                 |character |
#'   |bat_side_description          |character |
#'   |pitch_hand_code               |character |
#'   |pitch_hand_description        |character |
#'   |league_id                     |numeric   |
#'   |season                        |numeric   |
#' @export
#' @examples \donttest{
#'  try(mlb_all_star_ballots(league_id = 103, season = 2021))
#' }
mlb_all_star_ballots <- function(league_id = NULL, 
                                  season = NULL){
  

  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/league/{league_id}/allStarBallot"))
  query_params <- list(
    season = season
  )

  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)

  tryCatch(
    expr = {
      resp <- mlb_endpoint %>%
        mlb_api_call()
      ballot <- jsonlite::fromJSON(jsonlite::toJSON(resp$people), flatten=TRUE) %>%
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::mutate(
          league_id = as.numeric(league_id),
          season = as.numeric(season)) %>% 
        dplyr::rename(
          player_id = .data$id)
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(ballot)
}
