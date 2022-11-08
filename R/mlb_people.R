#' @title **Find Biographical Information for MLB Players**
#' @param person_ids MLBAMIDs for players of interest. Multiple IDs should be provided in a vector separated by a comma.
#' @return Returns a tibble with the following columns:
#'
#'  |col_name                       |types     |
#'  |:------------------------------|:---------|
#'  |id                             |integer   |
#'  |full_name                      |character |
#'  |link                           |character |
#'  |first_name                     |character |
#'  |last_name                      |character |
#'  |primary_number                 |character |
#'  |birth_date                     |character |
#'  |current_age                    |integer   |
#'  |birth_city                     |character |
#'  |birth_state_province           |character |
#'  |birth_country                  |character |
#'  |height                         |character |
#'  |weight                         |integer   |
#'  |active                         |logical   |
#'  |use_name                       |character |
#'  |middle_name                    |character |
#'  |boxscore_name                  |character |
#'  |nick_name                      |character |
#'  |gender                         |character |
#'  |is_player                      |logical   |
#'  |is_verified                    |logical   |
#'  |draft_year                     |integer   |
#'  |mlb_debut_date                 |character |
#'  |name_first_last                |character |
#'  |name_slug                      |character |
#'  |first_last_name                |character |
#'  |last_first_name                |character |
#'  |last_init_name                 |character |
#'  |init_last_name                 |character |
#'  |full_fml_name                  |character |
#'  |full_lfm_name                  |character |
#'  |strike_zone_top                |numeric   |
#'  |strike_zone_bottom             |numeric   |
#'  |pronunciation                  |character |
#'  |primary_position_code          |character |
#'  |primary_position_name          |character |
#'  |primary_position_type          |character |
#'  |primary_position_abbreviation  |character |
#'  |bat_side_code                  |character |
#'  |bat_side_description           |character |
#'  |pitch_hand_code                |character |
#'  |pitch_hand_description         |character |
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
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>%
        mlb_api_call()
      people <- jsonlite::fromJSON(jsonlite::toJSON(resp$people), flatten = TRUE) %>%
        janitor::clean_names() %>%
        make_baseballr_data("MLB People data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(people)
}
