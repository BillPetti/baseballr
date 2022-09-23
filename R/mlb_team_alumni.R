#' @title **MLB Team Alumni**
#' @param team_id Team ID to return information and ranking for a particular statistic for a particular team.
#' @param stat_group Stat group to return information and ranking for a particular statistic in a particular group.
#' @param season Year to return information and ranking for a particular statistic in a given year. 
#' 
#' @return Returns a tibble with the following columns
#'    |col_name                      |types     |
#'    |:-----------------------------|:---------|
#'    |player_id                     |integer   |
#'    |player_full_name              |character |
#'    |link                          |character |
#'    |first_name                    |character |
#'    |last_name                     |character |
#'    |primary_number                |character |
#'    |birth_date                    |character |
#'    |current_age                   |integer   |
#'    |birth_city                    |character |
#'    |birth_country                 |character |
#'    |height                        |character |
#'    |weight                        |integer   |
#'    |active                        |logical   |
#'    |use_name                      |character |
#'    |middle_name                   |character |
#'    |boxscore_name                 |character |
#'    |nick_name                     |character |
#'    |gender                        |character |
#'    |name_matrilineal              |character |
#'    |is_player                     |logical   |
#'    |is_verified                   |logical   |
#'    |pronunciation                 |character |
#'    |mlb_debut_date                |character |
#'    |name_first_last               |character |
#'    |name_slug                     |character |
#'    |first_last_name               |character |
#'    |last_first_name               |character |
#'    |last_init_name                |character |
#'    |init_last_name                |character |
#'    |full_fml_name                 |character |
#'    |full_lfm_name                 |character |
#'    |strike_zone_top               |numeric   |
#'    |strike_zone_bottom            |numeric   |
#'    |alumni_last_season            |character |
#'    |birth_state_province          |character |
#'    |draft_year                    |integer   |
#'    |primary_position_code         |character |
#'    |primary_position_name         |character |
#'    |primary_position_type         |character |
#'    |primary_position_abbreviation |character |
#'    |bat_side_code                 |character |
#'    |bat_side_description          |character |
#'    |pitch_hand_code               |character |
#'    |pitch_hand_description        |character |

#' @export
#' @examples \donttest{
#'   try(mlb_team_alumni(team_id = 137, stat_group = 'hitting', season = 2021))
#' }
mlb_team_alumni <- function(team_id = NULL,
                            stat_group = NULL,
                            season = NULL){
  
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/teams/{team_id}/alumni"))
  query_params <- list(
    group = stat_group,
    season = season
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      team_alumni <- jsonlite::fromJSON(jsonlite::toJSON(resp[['people']]), flatten = TRUE)  
      team_alumni$season <- NULL
      team_alumni <- team_alumni %>% 
        janitor::clean_names() %>% 
        as.data.frame()  %>% 
        dplyr::rename(
          player_id = .data$id,
          player_full_name = .data$full_name) %>%
        make_baseballr_data("MLB Team Alumni data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(team_alumni)
}

