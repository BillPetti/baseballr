#' @title **MLB Leagues** 
#' @param seasons Year(s) to return to return league information for.
#' @param sport_id The sport_id to return league information for.
#' @param league_id The league_id(s) to return league information for.
#' @return Returns a tibble with the following columns
#'
#'  |col_name                                     |types     |description                                              |
#'  |:--------------------------------------------|:---------|:--------------------------------------------------------|
#'  |league_id                                    |integer   |MLB league id (e.g. 103 for AL, 104 for NL).            |
#'  |league_name                                  |character |League name (e.g. 'American League').                   |
#'  |league_link                                  |character |API relative link to the league.                        |
#'  |league_abbreviation                          |character |League abbreviation (e.g. 'AL').                        |
#'  |league_name_short                            |character |Short league name (e.g. 'American').                    |
#'  |league_season_state                          |character |Season state (e.g. 'offseason', 'regular').             |
#'  |league_has_wild_card                         |logical   |Whether the league uses a wild card.                     |
#'  |league_has_split_season                      |logical   |Whether the league has a split season.                   |
#'  |league_num_games                             |integer   |Scheduled number of regular season games.                |
#'  |league_has_playoff_points                    |logical   |Whether the league awards playoff points.                |
#'  |league_num_teams                             |integer   |Number of teams in the league.                           |
#'  |league_num_wildcard_teams                    |integer   |Number of wild card teams.                               |
#'  |league_season                               |character |Season year for the league record.                       |
#'  |league_org_code                              |character |Organization code (e.g. 'AL').                          |
#'  |league_conferences_in_use                    |logical   |Whether conferences are used.                            |
#'  |league_divisions_in_use                      |logical   |Whether divisions are used.                              |
#'  |league_sort_order                            |integer   |Display sort order for the league.                       |
#'  |league_active                                |logical   |Whether the league is active.                            |
#'  |season_date_info_season_id                   |character |Season identifier for the date info block.               |
#'  |season_date_info_pre_season_start_date       |character |Preseason start date (YYYY-MM-DD).                       |
#'  |season_date_info_pre_season_end_date         |character |Preseason end date (YYYY-MM-DD).                         |
#'  |season_date_info_season_start_date           |character |Season start date (YYYY-MM-DD).                          |
#'  |season_date_info_spring_start_date           |character |Spring training start date (YYYY-MM-DD).                 |
#'  |season_date_info_spring_end_date             |character |Spring training end date (YYYY-MM-DD).                   |
#'  |season_date_info_regular_season_start_date   |character |Regular season start date (YYYY-MM-DD).                  |
#'  |season_date_info_last_date1st_half           |character |Last date of the first half (YYYY-MM-DD).                |
#'  |season_date_info_all_star_date               |character |All-Star Game date (YYYY-MM-DD).                         |
#'  |season_date_info_first_date2nd_half          |character |First date of the second half (YYYY-MM-DD).              |
#'  |season_date_info_regular_season_end_date     |character |Regular season end date (YYYY-MM-DD).                    |
#'  |season_date_info_post_season_start_date      |character |Postseason start date (YYYY-MM-DD).                      |
#'  |season_date_info_post_season_end_date        |character |Postseason end date (YYYY-MM-DD).                        |
#'  |season_date_info_season_end_date             |character |Season end date (YYYY-MM-DD).                            |
#'  |season_date_info_offseason_start_date        |character |Offseason start date (YYYY-MM-DD).                       |
#'  |season_date_info_off_season_end_date         |character |Offseason end date (YYYY-MM-DD).                         |
#'  |season_date_info_season_level_gameday_type   |character |Season-level Gameday data type code.                     |
#'  |season_date_info_game_level_gameday_type     |character |Game-level Gameday data type code.                       |
#'  |season_date_info_qualifier_plate_appearances |numeric   |Plate appearances per game needed to qualify.            |
#'  |season_date_info_qualifier_outs_pitched      |integer   |Outs pitched per game needed to qualify.                 |
#'  |sport_id                                     |integer   |Sport id associated with the league (1 for MLB).         |
#'  |sport_link                                   |character |API relative link to the sport.                          |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_league(seasons = 2021, sport_id = 1))
#' }
mlb_league <- function(seasons = NULL,
                       sport_id = NULL, 
                       league_id = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/league")
  query_params <- list(
    seasons = seasons, 
    leagueIds = league_id,
    sportId = sport_id
  )
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  leagues <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      
      leagues <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)$leagues |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "league_id" = "id",
          "league_name" = "name",
          "league_link" = "link",
          "league_abbreviation" = "abbreviation",
          "league_name_short" = "name_short",
          "league_season_state" = "season_state",
          "league_has_wild_card" = "has_wild_card",
          "league_has_split_season" = "has_split_season",
          "league_num_games" = "num_games",
          "league_has_playoff_points" = "has_playoff_points",
          "league_num_teams" = "num_teams",
          "league_num_wildcard_teams" = "num_wildcard_teams",
          "league_season" = "season",
          "league_org_code" = "org_code",
          "league_conferences_in_use" = "conferences_in_use",
          "league_divisions_in_use" = "divisions_in_use",
          "league_sort_order" = "sort_order",
          "league_active" = "active") |>
        make_baseballr_data("MLB League data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(leagues)
}

