#' @rdname mlb_draft_latest
#' @title **Retrieve latest draft information by year**
#' @param year The year for which to return data
#' @return Returns a tibble with the latest draft information for the year requested:
#'
#'  |col_name                             |types     |description                                       |
#'  |:------------------------------------|:---------|:-------------------------------------------------|
#'  |pick_round                           |character |Draft round.                                      |
#'  |pick_number                          |integer   |Overall pick number.                              |
#'  |display_pick_number                  |integer   |Pick number as displayed.                         |
#'  |round_pick_number                    |integer   |Pick number within the round.                     |
#'  |pick_value                           |character |Assigned slot/pick value (dollars).               |
#'  |signing_bonus                        |character |Signing bonus (dollars).                          |
#'  |home_city                            |character |Prospect home city.                               |
#'  |home_state                           |character |Prospect home state.                              |
#'  |home_country                         |character |Prospect home country.                            |
#'  |school_name                          |character |School name.                                      |
#'  |school_school_class                  |character |School class (e.g. 4YR JR, HS SR).                |
#'  |school_country                       |character |School country.                                   |
#'  |school_state                         |character |School state.                                     |
#'  |headshot_link                        |character |URL to the player's headshot image.               |
#'  |person_id                            |integer   |MLB player ID.                                    |
#'  |person_full_name                     |character |Player full name.                                 |
#'  |person_link                          |character |MLB Stats API relative player link.               |
#'  |person_first_name                    |character |Player first name.                                |
#'  |person_last_name                     |character |Player last name.                                 |
#'  |person_primary_number                |character |Player uniform number.                            |
#'  |person_birth_date                    |character |Birth date (YYYY-MM-DD).                          |
#'  |person_current_age                   |integer   |Current age in years.                             |
#'  |person_birth_city                    |character |City of birth.                                    |
#'  |person_birth_state_province          |character |State or province of birth.                       |
#'  |person_birth_country                 |character |Country of birth.                                 |
#'  |person_height                        |character |Height (feet and inches).                         |
#'  |person_weight                        |integer   |Weight in pounds.                                 |
#'  |person_active                        |logical   |Whether the player is currently active.           |
#'  |person_primary_position_code         |character |Primary fielding position code.                   |
#'  |person_primary_position_name         |character |Primary fielding position name.                   |
#'  |person_primary_position_type         |character |Primary position type (e.g. Infielder).           |
#'  |person_primary_position_abbreviation |character |Primary position abbreviation.                    |
#'  |person_use_name                      |character |Preferred first name.                             |
#'  |person_use_last_name                 |character |Preferred last name.                              |
#'  |person_middle_name                   |character |Player middle name.                               |
#'  |person_boxscore_name                 |character |Name as shown in box scores.                      |
#'  |person_gender                        |character |Player gender.                                    |
#'  |person_is_player                     |logical   |Whether the person is a player.                   |
#'  |person_is_verified                   |logical   |Whether the player profile is verified.           |
#'  |person_draft_year                    |integer   |Year the player was drafted.                      |
#'  |person_mlb_debut_date                |character |MLB debut date (YYYY-MM-DD).                      |
#'  |person_bat_side_code                 |character |Batting side code (L/R/S).                        |
#'  |person_bat_side_description          |character |Batting side description.                          |
#'  |person_pitch_hand_code               |character |Throwing hand code (L/R).                         |
#'  |person_pitch_hand_description        |character |Throwing hand description.                        |
#'  |person_name_first_last               |character |Name in first-last order.                         |
#'  |person_name_slug                     |character |URL-friendly name slug.                           |
#'  |person_first_last_name               |character |First and last name.                              |
#'  |person_last_first_name               |character |Name in last, first order.                        |
#'  |person_last_init_name                |character |Last name with first initial.                     |
#'  |person_init_last_name                |character |First initial with last name.                     |
#'  |person_full_fml_name                 |character |Full name (first-middle-last).                    |
#'  |person_full_lfm_name                 |character |Full name (last-first-middle).                    |
#'  |person_strike_zone_top               |numeric   |Top of the player's strike zone (feet).           |
#'  |person_strike_zone_bottom            |numeric   |Bottom of the player's strike zone (feet).        |
#'  |person_xref_ids_xref_id              |character |Cross-reference ID from an external source.       |
#'  |person_xref_ids_xref_type            |character |Cross-reference source type (e.g. fangraphs).     |
#'  |person_xref_ids_season               |character |Season associated with the cross-reference ID.    |
#'  |team_spring_league_id                |integer   |Spring training league ID.                        |
#'  |team_spring_league_name              |character |Spring training league name.                      |
#'  |team_spring_league_link              |character |MLB Stats API relative spring league link.        |
#'  |team_spring_league_abbreviation      |character |Spring training league abbreviation.              |
#'  |team_all_star_status                 |character |Team all-star status flag.                        |
#'  |team_id                              |integer   |MLB team ID of the drafting team.                 |
#'  |team_name                            |character |Drafting team name.                               |
#'  |team_link                            |character |MLB Stats API relative team link.                 |
#'  |team_season                          |integer   |Team season (YYYY).                               |
#'  |team_venue_id                        |integer   |Home venue ID.                                    |
#'  |team_venue_name                      |character |Home venue name.                                  |
#'  |team_venue_link                      |character |MLB Stats API relative venue link.                |
#'  |team_spring_venue_id                 |integer   |Spring training venue ID.                         |
#'  |team_spring_venue_link               |character |MLB Stats API relative spring venue link.         |
#'  |team_team_code                       |character |Team code.                                        |
#'  |team_file_code                       |character |Team file code.                                   |
#'  |team_abbreviation                    |character |Team abbreviation.                                |
#'  |team_team_name                       |character |Team nickname.                                    |
#'  |team_location_name                   |character |Team location/city name.                          |
#'  |team_first_year_of_play              |character |First year the franchise played.                  |
#'  |team_league_id                       |integer   |MLB league ID.                                    |
#'  |team_league_name                     |character |League name.                                      |
#'  |team_league_link                     |character |MLB Stats API relative league link.               |
#'  |team_division_id                     |integer   |Division ID.                                      |
#'  |team_division_name                   |character |Division name.                                    |
#'  |team_division_link                   |character |MLB Stats API relative division link.             |
#'  |team_sport_id                        |integer   |MLB sport ID.                                     |
#'  |team_sport_link                      |character |MLB Stats API relative sport link.                |
#'  |team_sport_name                      |character |Sport name.                                       |
#'  |team_short_name                      |character |Team short name.                                  |
#'  |team_franchise_name                  |character |Franchise name.                                   |
#'  |team_club_name                       |character |Club name.                                        |
#'  |team_active                          |logical   |Whether the team is currently active.             |
#'  |draft_type_code                      |character |Draft type code.                                  |
#'  |draft_type_description               |character |Draft type description.                           |
#'  |is_drafted                           |logical   |Whether the prospect was drafted.                 |
#'  |is_pass                              |logical   |Whether the pick was a pass.                      |
#'  |year                                 |character |Draft year (YYYY).                                |
#' @export
#' @examples \donttest{
#'  try(mlb_draft_latest(year = 2020))
#' }
mlb_draft_latest <- function(year) {
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/draft/{year}/latest"))
  draft_latest <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |>
        mlb_api_call()
      
      draft_latest <- jsonlite::fromJSON(jsonlite::toJSON(resp$pick), flatten = TRUE) |>
        as.data.frame() |>
        janitor::clean_names() |>
        make_baseballr_data("MLB Draft (Latest) data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(draft_latest)
}
