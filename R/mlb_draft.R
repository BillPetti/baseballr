#' @rdname mlb_draft 
#' @title **Retrieve draft pick information by year**
#' @param year The year for which to return data
#' @return Returns a data frame with information for every draft pick in every round for the year requested
#'  |col_name                             |types     |
#'  |:------------------------------------|:---------|
#'  |bis_player_id                        |integer   |
#'  |pick_round                           |character |
#'  |pick_number                          |integer   |
#'  |round_pick_number                    |integer   |
#'  |rank                                 |integer   |
#'  |pick_value                           |character |
#'  |signing_bonus                        |character |
#'  |scouting_report                      |character |
#'  |blurb                                |character |
#'  |headshot_link                        |character |
#'  |is_drafted                           |logical   |
#'  |is_pass                              |logical   |
#'  |year                                 |character |
#'  |home_city                            |character |
#'  |home_state                           |character |
#'  |home_country                         |character |
#'  |school_name                          |character |
#'  |school_school_class                  |character |
#'  |school_country                       |character |
#'  |school_state                         |character |
#'  |person_id                            |integer   |
#'  |person_full_name                     |character |
#'  |person_link                          |character |
#'  |person_first_name                    |character |
#'  |person_last_name                     |character |
#'  |person_primary_number                |character |
#'  |person_birth_date                    |character |
#'  |person_current_age                   |integer   |
#'  |person_birth_city                    |character |
#'  |person_birth_state_province          |character |
#'  |person_birth_country                 |character |
#'  |person_height                        |character |
#'  |person_weight                        |integer   |
#'  |person_active                        |logical   |
#'  |person_use_name                      |character |
#'  |person_middle_name                   |character |
#'  |person_boxscore_name                 |character |
#'  |person_gender                        |character |
#'  |person_is_player                     |logical   |
#'  |person_is_verified                   |logical   |
#'  |person_draft_year                    |integer   |
#'  |person_name_first_last               |character |
#'  |person_name_slug                     |character |
#'  |person_first_last_name               |character |
#'  |person_last_first_name               |character |
#'  |person_last_init_name                |character |
#'  |person_init_last_name                |character |
#'  |person_full_fml_name                 |character |
#'  |person_full_lfm_name                 |character |
#'  |person_strike_zone_top               |numeric   |
#'  |person_strike_zone_bottom            |numeric   |
#'  |person_pronunciation                 |character |
#'  |person_name_title                    |character |
#'  |person_mlb_debut_date                |character |
#'  |person_name_matrilineal              |character |
#'  |person_primary_position_code         |character |
#'  |person_primary_position_name         |character |
#'  |person_primary_position_type         |character |
#'  |person_primary_position_abbreviation |character |
#'  |person_bat_side_code                 |character |
#'  |person_bat_side_description          |character |
#'  |person_pitch_hand_code               |character |
#'  |person_pitch_hand_description        |character |
#'  |team_id                              |integer   |
#'  |team_name                            |character |
#'  |team_link                            |character |
#'  |team_all_star_status                 |character |
#'  |team_spring_league_id                |integer   |
#'  |team_spring_league_name              |character |
#'  |team_spring_league_link              |character |
#'  |team_spring_league_abbreviation      |character |
#'  |draft_type_code                      |character |
#'  |draft_type_description               |character |
#' @export 
#' @examples \donttest{
#'   try(mlb_draft(year = 2020))
#' }
mlb_draft <- function(year) {

  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/draft/{year}"))
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      
      draft_table <- resp$drafts$rounds$picks
      
      draft_table <- draft_table %>%
        dplyr::bind_rows()
      draft_table <- jsonlite::fromJSON(jsonlite::toJSON(draft_table), flatten = TRUE) %>%
        janitor::clean_names() %>%
        make_baseballr_data("MLB Draft data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(draft_table)
}

#' @rdname get_draft_mlb
#' @title **(legacy) Retrieve draft pick information by year**
#' @inheritParams mlb_draft
#' @return Returns a data frame with information for every draft pick in every round for the year requested
#' @keywords legacy
#' @export
get_draft_mlb <- mlb_draft
