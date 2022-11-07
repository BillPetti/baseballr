#' @title **MLB Team Individual Stats**
#' @param stat_type Stat type to return statistics for.
#' @param game_type Game type to return information for a particular statistic in a particular game type.
#' @param team_id Team ID to return information and ranking for a particular statistic for a particular team.
#' @param stat_group Stat group to return information and ranking for a particular statistic in a particular group.
#' @param season Year to return information and ranking for a particular statistic in a given year. 
#' @param sport_ids The sport_id(s) to return information and ranking information for.
#' 
#' @return Returns a tibble with the following columns
#'    |col_name                |types     |
#'    |:-----------------------|:---------|
#'    |season                  |character |
#'    |games_played            |integer   |
#'    |ground_outs             |integer   |
#'    |air_outs                |integer   |
#'    |runs                    |integer   |
#'    |doubles                 |integer   |
#'    |triples                 |integer   |
#'    |home_runs               |integer   |
#'    |strike_outs             |integer   |
#'    |base_on_balls           |integer   |
#'    |intentional_walks       |integer   |
#'    |hits                    |integer   |
#'    |hit_by_pitch            |integer   |
#'    |avg                     |character |
#'    |at_bats                 |integer   |
#'    |obp                     |character |
#'    |slg                     |character |
#'    |ops                     |character |
#'    |caught_stealing         |integer   |
#'    |stolen_bases            |integer   |
#'    |stolen_base_percentage  |character |
#'    |ground_into_double_play |integer   |
#'    |number_of_pitches       |integer   |
#'    |plate_appearances       |integer   |
#'    |total_bases             |integer   |
#'    |rbi                     |integer   |
#'    |left_on_base            |integer   |
#'    |sac_bunts               |integer   |
#'    |sac_flies               |integer   |
#'    |babip                   |character |
#'    |ground_outs_to_airouts  |character |
#'    |catchers_interference   |integer   |
#'    |at_bats_per_home_run    |character |
#'    |team_id                 |integer   |
#'    |team_name               |character |
#'    |team_link               |character |
#'    |type_display_name       |character |
#'    |group_display_name      |character |
#' @export
#' @examples \donttest{
#'   try(mlb_team_stats(team_id = 137, stat_type = 'season', stat_group = 'hitting', season = 2021))
#' }
mlb_team_stats <- function(team_id = NULL,
                           stat_type = NULL,
                           game_type = NULL,
                           stat_group = NULL,
                           season = NULL,
                           sport_ids = NULL){
  
  sport_ids <- paste(sport_ids, collapse = ',')
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/teams/{team_id}/stats"))
  query_params <- list(
    stats = stat_type,
    gameType = game_type,
    group = stat_group,
    season = season,
    sportIds = sport_ids
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      stats_leaders <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)  
      stats_leaders$season <- NULL
      stats <- stats_leaders %>% 
        tidyr::unnest(.data$splits) %>% 
        janitor::clean_names()  %>% 
        as.data.frame() %>% 
        dplyr::select(-.data$exemptions) %>%
        make_baseballr_data("MLB Team Stats data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  colnames(stats)<-gsub("stat_", "", colnames(stats))
  return(stats)
}

