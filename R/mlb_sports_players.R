#' @title **MLB Sport Players**
#' @param sport_id The sport_id to return information for.
#' @param season The season to return information for.
#' @return Returns a tibble with the following columns:
#'
#'   |col_name                      |types     |description                                          |
#'   |:-----------------------------|:---------|:----------------------------------------------------|
#'   |player_id                     |integer   |MLBAM player ID.                                     |
#'   |full_name                     |character |Player full name.                                    |
#'   |link                          |character |API link to the player resource.                     |
#'   |first_name                    |character |Player first name.                                   |
#'   |last_name                     |character |Player last name.                                    |
#'   |primary_number                |character |Primary uniform number.                              |
#'   |birth_date                    |character |Date of birth (YYYY-MM-DD).                          |
#'   |current_age                   |integer   |Current age in years.                                |
#'   |birth_city                    |character |City of birth.                                       |
#'   |birth_country                 |character |Country of birth.                                    |
#'   |height                        |character |Listed height (feet and inches).                     |
#'   |weight                        |integer   |Listed weight in pounds.                             |
#'   |active                        |logical   |Whether the player is currently active.              |
#'   |use_name                      |character |Preferred first name for display.                    |
#'   |use_last_name                 |character |Preferred last name for display.                     |
#'   |middle_name                   |character |Player middle name.                                  |
#'   |boxscore_name                 |character |Name as shown in box scores.                         |
#'   |nick_name                     |character |Player nickname.                                     |
#'   |gender                        |character |Player gender code.                                  |
#'   |is_player                     |logical   |Whether the person is a player.                      |
#'   |is_verified                   |logical   |Whether the profile is verified.                     |
#'   |pronunciation                 |character |Phonetic pronunciation of the name.                  |
#'   |mlb_debut_date                |character |MLB debut date (YYYY-MM-DD).                         |
#'   |name_first_last               |character |Name in first-last order.                            |
#'   |name_slug                     |character |URL slug for the player.                             |
#'   |first_last_name               |character |First and last name display.                         |
#'   |last_first_name               |character |Last, first name display.                            |
#'   |last_init_name                |character |Last name with first initial.                        |
#'   |init_last_name                |character |First initial with last name.                        |
#'   |full_fml_name                 |character |Full first-middle-last name.                         |
#'   |full_lfm_name                 |character |Full last-first-middle name.                         |
#'   |strike_zone_top               |numeric   |Top of the player's strike zone (feet).              |
#'   |strike_zone_bottom            |numeric   |Bottom of the player's strike zone (feet).           |
#'   |birth_state_province          |character |State or province of birth.                          |
#'   |draft_year                    |integer   |Year the player was drafted.                         |
#'   |name_matrilineal              |character |Matrilineal (maternal) surname.                      |
#'   |last_played_date              |character |Date of last MLB appearance.                         |
#'   |name_title                    |character |Name title prefix.                                   |
#'   |name_suffix                   |character |Name suffix (e.g., Jr., III).                        |
#'   |death_date                    |character |Date of death (YYYY-MM-DD).                          |
#'   |death_city                    |character |City of death.                                       |
#'   |death_country                 |character |Country of death.                                    |
#'   |current_team_id               |integer   |Current team MLBAM ID.                               |
#'   |current_team_name             |character |Current team name.                                   |
#'   |current_team_link             |character |API link to the current team.                        |
#'   |primary_position_code         |character |Primary position code.                               |
#'   |primary_position_name         |character |Primary position name.                               |
#'   |primary_position_type         |character |Primary position type.                               |
#'   |primary_position_abbreviation |character |Primary position abbreviation.                       |
#'   |bat_side_code                 |character |Batting side code (L/R/S).                           |
#'   |bat_side_description          |character |Batting side description.                             |
#'   |pitch_hand_code               |character |Throwing hand code (L/R).                            |
#'   |pitch_hand_description        |character |Throwing hand description.                            |
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
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  players <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      
      players <- resp$people |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE) |> 
        as.data.frame() |>  
        janitor::clean_names() |> 
        dplyr::rename(
          "player_id" = "id") |>
        make_baseballr_data("MLB Sports - Players data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(players)
}

