#' @title **MLB Stats Leaders**
#' @param leader_categories League leader category to return information and ranking for a particular statistic.
#' @param player_pool There are 4 different types of player pools to return statistics for a particular player pool across a sport. 
#' Acceptable values include: All, Qualified, Rookies, or Qualified_rookies
#' @param leader_game_types Game type to return information and ranking for a particular statistic in a particular game type.
#' @param sit_codes Situation code to return information and ranking for a particular statistic in a particular game type.
#' @param position Position to return statistics for a given position. Default to "Qualified" player pool
#' Acceptable values include: 
#'    - P
#'    - C
#'    - 1B
#'    - 2B
#'    - 3B
#'    - SS
#'    - LF
#'    - CF
#'    - RF
#'    - DH
#'    - PH
#'    - PR
#'    - BR
#'    - OF
#'    - IF
#'    - SP
#'    - RP
#'    - CP
#'    - UT
#'    - UI
#'    - UO
#'    - RHP
#'    - LHP
#'    - RHS
#'    - LHS
#'    - LHR
#'    - RHR
#'    - B
#'    - X
#' @param stat_group Stat group to return information and ranking for a particular statistic in a particular group.
#' @param season Year to return information and ranking for a particular statistic in a given year. 
#' @param league_id League ID to return statistics for a given league. Default to "Qualified" player pool.
#' @param sport_id The sport_id to return information and ranking information for.
#' @param start_date Start date to return information and ranking for a particular statistic for a particular date range. Format: MM/DD/YYYY
#'  *start_date must be coupled with end_date and byDateRange stat_type*
#' @param end_date End date to return information and ranking for a particular statistic for a particular date range. Format: MM/DD/YYYY
#'   *end_date must be coupled with start_date and byDateRange stat_type*
#' @param stat_type The stat_type to return information and ranking for a particular statistic for a particular stat type.
#' @param limit A limit to limit return to a particular number of records.
#' @return Returns a tibble with the following columns
#'   |col_name              |types     |
#'   |:---------------------|:---------|
#'   |leader_category       |character |
#'   |rank                  |integer   |
#'   |value                 |character |
#'   |season                |character |
#'   |num_teams             |integer   |
#'   |team_id               |integer   |
#'   |team_name             |character |
#'   |team_link             |character |
#'   |league_id             |integer   |
#'   |league_name           |character |
#'   |league_link           |character |
#'   |person_id             |integer   |
#'   |person_full_name      |character |
#'   |person_link           |character |
#'   |person_first_name     |character |
#'   |person_last_name      |character |
#'   |sport_id              |integer   |
#'   |sport_link            |character |
#'   |sport_abbreviation    |character |
#'   |stat_group            |character |
#'   |total_splits          |integer   |
#'   |game_type_id          |character |
#'   |game_type_description |character |
#' @export
#' @examples \donttest{
#'  try(mlb_stats_leaders(leader_categories='homeRuns',sport_id=1, season = 2021))
#' }
mlb_stats_leaders <- function(leader_categories = NULL,
                              player_pool=NULL,
                              leader_game_types = NULL,
                              sit_codes = NULL,
                              position = NULL,
                              stat_group = NULL,
                              season = NULL,
                              league_id = NULL,
                              sport_id = NULL,
                              start_date = NULL,
                              end_date = NULL,
                              stat_type = NULL,
                              limit = 1000){
  
  sport_id <- paste(sport_id, collapse = ',')
  mlb_endpoint <- mlb_stats_endpoint("v1/stats/leaders")
  query_params <- list(
    leaderCategories = leader_categories,
    playerPool = player_pool,
    leaderGameTypes = leader_game_types,
    sitCodes = sit_codes,
    position = position,
    statGroup = stat_group,
    season = season,
    leagueId = league_id,
    sportId = sport_id,
    startDate = start_date,
    endDate = end_date,
    statType = stat_type,
    limit = limit
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      stats_leaders <- jsonlite::fromJSON(jsonlite::toJSON(resp[['leagueLeaders']]), flatten = TRUE)  
      stats_leaders$season <- NULL
      stats_leaders <- stats_leaders %>% 
        tidyr::unnest("leaders") %>% 
        janitor::clean_names()  %>% 
        as.data.frame() %>%
        make_baseballr_data("MLB Stats Leaders data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(stats_leaders)
}

