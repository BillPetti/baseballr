#' @title **Find Biographical Information for MLB Players**
#' @param person_ids MLBAMIDs for players of interest. Multiple IDs should be provided in a vector separated by a comma.
#' @return Returns a tibble with the following columns:
#'
#'  |col_name                       |types     |description                                                       |
#'  |:------------------------------|:---------|:-----------------------------------------------------------------|
#'  |id                             |integer   |MLB person id (MLBAMID).                                          |
#'  |full_name                      |character |Player full name.                                                 |
#'  |link                           |character |API relative link to the player.                                  |
#'  |first_name                     |character |Player first name.                                                |
#'  |last_name                      |character |Player last name.                                                 |
#'  |primary_number                 |character |Player primary jersey number.                                     |
#'  |birth_date                     |character |Player birth date (YYYY-MM-DD).                                  |
#'  |current_age                    |integer   |Player current age in years.                                      |
#'  |birth_city                     |character |Player birth city.                                                |
#'  |birth_state_province           |character |Player birth state or province.                                   |
#'  |birth_country                  |character |Player birth country.                                             |
#'  |height                         |character |Player height (e.g. "6' 2\"").                                  |
#'  |weight                         |integer   |Player weight in pounds.                                          |
#'  |active                         |logical   |Whether the player is currently active.                           |
#'  |use_name                       |character |Player preferred display first name.                              |
#'  |use_last_name                  |character |Player preferred display last name.                               |
#'  |middle_name                    |character |Player middle name.                                               |
#'  |boxscore_name                  |character |Player short box score name.                                      |
#'  |nick_name                      |character |Player nickname.                                                  |
#'  |gender                         |character |Player gender code (e.g. 'M').                                   |
#'  |is_player                      |logical   |Whether the person is classified as a player.                     |
#'  |is_verified                    |logical   |Whether the player profile is verified.                           |
#'  |draft_year                     |integer   |Year the player was drafted.                                      |
#'  |mlb_debut_date                 |character |Player MLB debut date (YYYY-MM-DD).                              |
#'  |name_first_last                |character |Player name in first-last order.                                  |
#'  |name_slug                      |character |URL slug for the player (name plus id).                           |
#'  |first_last_name                |character |Player name in first-last order.                                  |
#'  |last_first_name                |character |Player name in last, first order.                                 |
#'  |last_init_name                 |character |Player name as last, first initial.                               |
#'  |init_last_name                 |character |Player name as first initial last.                                |
#'  |full_fml_name                  |character |Player full first-middle-last name.                               |
#'  |full_lfm_name                  |character |Player full last, first-middle name.                              |
#'  |strike_zone_top                |numeric   |Top of the player's strike zone (feet).                          |
#'  |strike_zone_bottom             |numeric   |Bottom of the player's strike zone (feet).                       |
#'  |pronunciation                  |character |Phonetic pronunciation of the player's name.                      |
#'  |last_played_date               |character |Date the player last played (YYYY-MM-DD), if inactive.           |
#'  |primary_position_code          |character |Primary position code.                                            |
#'  |primary_position_name          |character |Primary position name (e.g. 'First Base').                       |
#'  |primary_position_type          |character |Primary position type (e.g. 'Infielder').                        |
#'  |primary_position_abbreviation  |character |Primary position abbreviation (e.g. '1B').                       |
#'  |bat_side_code                  |character |Batting side code (e.g. 'R').                                   |
#'  |bat_side_description           |character |Batting side description (e.g. 'Right').                         |
#'  |pitch_hand_code                |character |Throwing hand code (e.g. 'R').                                  |
#'  |pitch_hand_description         |character |Throwing hand description (e.g. 'Right').                        |
#' @export
#' @examples \donttest{
#'   try(mlb_people(person_ids = 502671))
#'   try(mlb_people(person_ids = c(502671,605151)))
#' }
mlb_people <- function(person_ids = NULL){
  person_ids <- paste(person_ids, collapse = ',')
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/people/"))
  query_params <- list(
    personIds = person_ids
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  people <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |>
        mlb_api_call()
      people <- jsonlite::fromJSON(jsonlite::toJSON(resp$people), flatten = TRUE) |>
        janitor::clean_names() |>
        make_baseballr_data("MLB People data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(people)
}
