#' @rdname mlb_draft_latest
#' @title **Retrieve latest draft information by year**
#' @param year The year for which to return data
#' @return Returns a data frame with the latest draft information for the year requested:
#'
#'  |col_name                             |types     |
#'  |:------------------------------------|:---------|
#'  |bis_player_id                        |integer   |
#'  |pick_round                           |character |
#'  |pick_number                          |integer   |
#'  |round_pick_number                    |integer   |
#'  |rank                                 |integer   |
#'  |pick_value                           |character |
#'  |signing_bonus                        |character |
#'  |home_city                            |character |
#'  |home_state                           |character |
#'  |home_country                         |character |
#'  |scouting_report                      |character |
#'  |school_name                          |character |
#'  |school_school_class                  |character |
#'  |school_country                       |character |
#'  |school_state                         |character |
#'  |blurb                                |character |
#'  |headshot_link                        |character |
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
#'  |person_primary_position_code         |character |
#'  |person_primary_position_name         |character |
#'  |person_primary_position_type         |character |
#'  |person_primary_position_abbreviation |character |
#'  |person_use_name                      |character |
#'  |person_middle_name                   |character |
#'  |person_boxscore_name                 |character |
#'  |person_gender                        |character |
#'  |person_is_player                     |logical   |
#'  |person_is_verified                   |logical   |
#'  |person_draft_year                    |integer   |
#'  |person_bat_side_code                 |character |
#'  |person_bat_side_description          |character |
#'  |person_pitch_hand_code               |character |
#'  |person_pitch_hand_description        |character |
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
#'  |team_id                              |integer   |
#'  |team_name                            |character |
#'  |team_link                            |character |
#'  |team_season                          |integer   |
#'  |team_venue_id                        |integer   |
#'  |team_venue_name                      |character |
#'  |team_venue_link                      |character |
#'  |team_spring_venue_id                 |integer   |
#'  |team_spring_venue_link               |character |
#'  |team_team_code                       |character |
#'  |team_file_code                       |character |
#'  |team_abbreviation                    |character |
#'  |team_team_name                       |character |
#'  |team_location_name                   |character |
#'  |team_first_year_of_play              |character |
#'  |team_league_id                       |integer   |
#'  |team_league_name                     |character |
#'  |team_league_link                     |character |
#'  |team_division_id                     |integer   |
#'  |team_division_name                   |character |
#'  |team_division_link                   |character |
#'  |team_sport_id                        |integer   |
#'  |team_sport_link                      |character |
#'  |team_sport_name                      |character |
#'  |team_short_name                      |character |
#'  |team_franchise_name                  |character |
#'  |team_club_name                       |character |
#'  |team_spring_league_id                |integer   |
#'  |team_spring_league_name              |character |
#'  |team_spring_league_link              |character |
#'  |team_spring_league_abbreviation      |character |
#'  |team_all_star_status                 |character |
#'  |team_active                          |logical   |
#'  |draft_type_code                      |character |
#'  |draft_type_description               |character |
#'  |is_drafted                           |logical   |
#'  |is_pass                              |logical   |
#'  |year                                 |character |
#' @export
#' @examples \donttest{
#'  try(mlb_draft_latest(year = 2020))
#' }
mlb_draft_latest <- function(year) {

  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/draft/{year}/latest"))

  resp <- mlb_endpoint %>%
    mlb_api_call()

  draft_latest <- jsonlite::fromJSON(jsonlite::toJSON(resp$pick), flatten = TRUE) %>%
    as.data.frame() %>%
    janitor::clean_names()


  return(draft_latest)
}
