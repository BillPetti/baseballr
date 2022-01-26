#' @rdname mlb_draft 
#' @title **Retrieve draft pick information by year**
#' @param year The year for which to return data
#' @return Returns a data frame with information for every draft pick in every round for the year requested
#'  |col_name                             |types     |
#'  |:------------------------------------|:---------|
#'  |bis_player_id                        |integer   |
#'  |pick_round                           |character |
#'  |pick_number                          |integer   |
#'  |headshot_link                        |character |
#'  |is_drafted                           |logical   |
#'  |is_pass                              |logical   |
#'  |school_name                          |character |
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
#'  |person_draft_year                    |integer   |
#'  |person_last_played_date              |logical   |
#'  |person_mlb_debut_date                |character |
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
#'  |person_nick_name                     |character |
#'  |person_death_date                    |logical   |
#'  |person_death_city                    |logical   |
#'  |person_death_state_province          |logical   |
#'  |person_death_country                 |logical   |
#'  |person_pronunciation                 |character |
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
#'  |person_name_title                    |character |
#'  |school_school_class                  |character |
#'  |rank                                 |integer   |
#'  |pick_value                           |character |
#'  |signing_bonus                        |character |
#'  |scouting_report                      |character |
#'  |blurb                                |character |
#'  |home_city                            |character |
#'  |home_state                           |character |
#'  |home_country                         |character |
#'  |school_city                          |character |
#'  |school_country                       |character |
#'  |school_state                         |character |
#'  |round_pick_number                    |integer   |
#'  |year                                 |character |
#'  |person_gender                        |character |
#'  |person_is_player                     |logical   |
#'  |person_is_verified                   |logical   |
#'  |draft_type_code                      |character |
#'  |draft_type_description               |character |
#'  |person_name_matrilineal              |character |
#' @export
mlb_draft <- function(year) {

  api_call <- paste0("http://statsapi.mlb.com/api/v1/draft/", year)

  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)

  draft_table <- payload$drafts$rounds$picks

  draft_table <- draft_table %>%
    dplyr::bind_rows()

  column_structure_draft_mlb[1,] <- NA

  draft_table_filled <- column_structure_draft_mlb %>% 
    dplyr::bind_rows(draft_table) %>%
    dplyr::filter(!is.na(.data$bisPlayerId)) %>%
    janitor::clean_names()

  return(draft_table_filled)
}

#' @rdname get_draft_mlb
#' @title **(legacy) Retrieve draft pick information by year**
#' @inheritParams mlb_draft
#' @return Returns a data frame with information for every draft pick in every round for the year requested
#' @keywords legacy
#' @export
get_draft_mlb <- mlb_draft
