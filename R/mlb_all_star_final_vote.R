#' @title **Find MLB All-Star Final Vote**
#' @param league_id League ID for league all-star ballot of interest.
#' @param season The season of the all-star ballot.
#' @return Returns a tibble with the following columns:
#'
#'   |col_name                      |types     |description                                       |
#'   |:-----------------------------|:---------|:-------------------------------------------------|
#'   |player_id                     |integer   |MLB player ID.                                    |
#'   |full_name                     |character |Player full name.                                 |
#'   |link                          |character |MLB Stats API relative resource link.             |
#'   |first_name                    |character |Player first name.                                |
#'   |last_name                     |character |Player last name.                                 |
#'   |primary_number                |character |Player uniform number.                            |
#'   |birth_date                    |character |Birth date (YYYY-MM-DD).                          |
#'   |current_age                   |integer   |Current age in years.                             |
#'   |birth_city                    |character |City of birth.                                    |
#'   |birth_country                 |character |Country of birth.                                 |
#'   |height                        |character |Height (feet and inches).                         |
#'   |weight                        |integer   |Weight in pounds.                                 |
#'   |active                        |logical   |Whether the player is currently active.           |
#'   |use_name                      |character |Preferred first name.                             |
#'   |use_last_name                 |character |Preferred last name.                              |
#'   |middle_name                   |character |Player middle name.                               |
#'   |boxscore_name                 |character |Name as shown in box scores.                      |
#'   |nick_name                     |character |Player nickname.                                  |
#'   |gender                        |character |Player gender.                                    |
#'   |name_matrilineal              |character |Maternal family name.                             |
#'   |is_player                     |logical   |Whether the person is a player.                   |
#'   |is_verified                   |logical   |Whether the player profile is verified.           |
#'   |pronunciation                 |character |Phonetic name pronunciation.                      |
#'   |last_played_date              |character |Date of last MLB game played.                     |
#'   |mlb_debut_date                |character |MLB debut date (YYYY-MM-DD).                      |
#'   |name_first_last               |character |Name in first-last order.                         |
#'   |name_slug                     |character |URL-friendly name slug.                           |
#'   |first_last_name               |character |First and last name.                              |
#'   |last_first_name               |character |Name in last, first order.                        |
#'   |last_init_name                |character |Last name with first initial.                     |
#'   |init_last_name                |character |First initial with last name.                     |
#'   |full_fml_name                 |character |Full name (first-middle-last).                    |
#'   |full_lfm_name                 |character |Full name (last-first-middle).                    |
#'   |strike_zone_top               |numeric   |Top of the player's strike zone (feet).           |
#'   |strike_zone_bottom            |numeric   |Bottom of the player's strike zone (feet).        |
#'   |birth_state_province          |character |State or province of birth.                       |
#'   |name_title                    |character |Name title.                                       |
#'   |name_suffix                   |character |Name suffix (e.g. Jr., III).                      |
#'   |draft_year                    |integer   |Year the player was drafted.                      |
#'   |primary_position_code         |character |Primary fielding position code.                   |
#'   |primary_position_name         |character |Primary fielding position name.                   |
#'   |primary_position_type         |character |Primary position type (e.g. Infielder).           |
#'   |primary_position_abbreviation |character |Primary position abbreviation.                    |
#'   |bat_side_code                 |character |Batting side code (L/R/S).                        |
#'   |bat_side_description          |character |Batting side description.                          |
#'   |pitch_hand_code               |character |Throwing hand code (L/R).                         |
#'   |pitch_hand_description        |character |Throwing hand description.                        |
#'   |league_id                     |numeric   |MLB league ID.                                    |
#'   |season                        |numeric   |Season (YYYY).                                    |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_all_star_final_vote(league_id = 103, season = 2021))
#' }
mlb_all_star_final_vote <- function(league_id = NULL, 
                                  season = NULL){
  

  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/league/{league_id}/allStarFinalVote"))
  query_params <- list(
    season = season
  )

  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)

  final_vote <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |>
        mlb_api_call()
      final_vote <- jsonlite::fromJSON(jsonlite::toJSON(resp$people), flatten = TRUE) |>
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::mutate(
          league_id = as.numeric(league_id),
          season = as.numeric(season)) |> 
        dplyr::rename(
          "player_id" = "id") |>
        make_baseballr_data("MLB All-Star Final Votes data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(final_vote)
}
