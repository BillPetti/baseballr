#' @title **MLB Team Alumni**
#' @param team_id Team ID to return information and ranking for a particular statistic for a particular team.
#' @param stat_group Stat group to return information and ranking for a particular statistic in a particular group.
#' @param season Year to return information and ranking for a particular statistic in a given year. 
#' 
#' @return Returns a tibble with the following columns
#'
#'    |col_name                      |types     |description                                  |
#'    |:-----------------------------|:---------|:--------------------------------------------|
#'    |player_id                     |integer   |MLBAM player ID.                             |
#'    |player_full_name              |character |Player full name.                            |
#'    |link                          |character |API link to the player resource.             |
#'    |first_name                    |character |Player first name.                           |
#'    |last_name                     |character |Player last name.                            |
#'    |primary_number                |character |Primary uniform number.                      |
#'    |birth_date                    |character |Date of birth (YYYY-MM-DD).                  |
#'    |current_age                   |integer   |Current age in years.                        |
#'    |birth_city                    |character |City of birth.                               |
#'    |birth_country                 |character |Country of birth.                            |
#'    |height                        |character |Listed height (feet and inches).             |
#'    |weight                        |integer   |Listed weight in pounds.                     |
#'    |active                        |logical   |Whether the player is currently active.      |
#'    |use_name                      |character |Preferred first name for display.            |
#'    |use_last_name                 |character |Preferred last name for display.             |
#'    |middle_name                   |character |Player middle name.                          |
#'    |boxscore_name                 |character |Name as shown in box scores.                 |
#'    |nick_name                     |character |Player nickname.                             |
#'    |gender                        |character |Player gender code.                          |
#'    |name_matrilineal              |character |Matrilineal (maternal) surname.              |
#'    |is_player                     |logical   |Whether the person is a player.              |
#'    |is_verified                   |logical   |Whether the profile is verified.             |
#'    |pronunciation                 |character |Phonetic pronunciation of the name.          |
#'    |last_played_date              |character |Date of last MLB appearance.                 |
#'    |mlb_debut_date                |character |MLB debut date (YYYY-MM-DD).                 |
#'    |name_first_last               |character |Name in first-last order.                    |
#'    |name_slug                     |character |URL slug for the player.                     |
#'    |first_last_name               |character |First and last name display.                 |
#'    |last_first_name               |character |Last, first name display.                    |
#'    |last_init_name                |character |Last name with first initial.                |
#'    |init_last_name                |character |First initial with last name.                |
#'    |full_fml_name                 |character |Full first-middle-last name.                 |
#'    |full_lfm_name                 |character |Full last-first-middle name.                 |
#'    |strike_zone_top               |numeric   |Top of the player's strike zone (feet).      |
#'    |strike_zone_bottom            |numeric   |Bottom of the player's strike zone (feet).   |
#'    |alumni_last_season            |character |Last season the player was with the team.    |
#'    |birth_state_province          |character |State or province of birth.                  |
#'    |draft_year                    |integer   |Year the player was drafted.                 |
#'    |primary_position_code         |character |Primary position code.                       |
#'    |primary_position_name         |character |Primary position name.                       |
#'    |primary_position_type         |character |Primary position type.                       |
#'    |primary_position_abbreviation |character |Primary position abbreviation.               |
#'    |bat_side_code                 |character |Batting side code (L/R/S).                   |
#'    |bat_side_description          |character |Batting side description.                    |
#'    |pitch_hand_code               |character |Throwing hand code (L/R).                    |
#'    |pitch_hand_description        |character |Throwing hand description.                   |

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
  
  team_alumni <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      team_alumni <- jsonlite::fromJSON(jsonlite::toJSON(resp[['people']]), flatten = TRUE)  
      team_alumni$season <- NULL
      team_alumni <- team_alumni |> 
        janitor::clean_names() |> 
        as.data.frame()  |> 
        dplyr::rename(
          "player_id" = "id",
          "player_full_name" = "full_name") |>
        make_baseballr_data("MLB Team Alumni data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(team_alumni)
}

