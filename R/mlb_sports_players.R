#' @title **MLB Sport Players**
#' @param sport_id The sport_id to return information for.
#' @param season The season to return information for.
#' @return Returns a tibble with the following columns:
#' 
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
#'   |pronunciation                 |character |
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
#'   |birth_state_province          |character |
#'   |draft_year                    |integer   |
#'   |name_matrilineal              |character |
#'   |name_title                    |character |
#'   |last_played_date              |character |
#'   |current_team_id               |integer   |
#'   |current_team_name             |character |
#'   |current_team_link             |character |
#'   |primary_position_code         |character |
#'   |primary_position_name         |character |
#'   |primary_position_type         |character |
#'   |primary_position_abbreviation |character |
#'   |bat_side_code                 |character |
#'   |bat_side_description          |character |
#'   |pitch_hand_code               |character |
#'   |pitch_hand_description        |character |
#'  
#' @export
#' @examples \donttest{
#'   try(mlb_sports_players(sport_id = 1, season = 2021))
#' }
mlb_sports_players <- function(sport_id = 1,
                               season = 2021){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/sports/{sport_id}/players"))
  query_params <- list(
    season = season
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      
      players <- resp$people %>% 
        jsonlite::toJSON() %>% 
        jsonlite::fromJSON(flatten = TRUE) %>% 
        as.data.frame() %>%  
        janitor::clean_names() %>% 
        dplyr::rename(
          player_id = .data$id) %>%
        make_baseballr_data("MLB Sports - Players data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(players)
}

