## #' @title **MLB Stats Streaks**
## #' @param streak_type The streak_type to return streak information for player in a particular season. 
## #' Acceptable options include:
## #'  - hittingStreakOverall
## #'  - hittingStreakHome
## #'  - hittingStreakAway
## #'  - onBaseOverall
## #'  - onBaseHome
## #'  - onBaseAway
## #' @param streak_span The streak_span to return the span of streak for player in a particular season. 
## #' Acceptable options include:
## #'  - career
## #'  - season
## #'  - currentStreak
## #'  - currentStreakInSeason
## #'  - notable
## #'  - notableInSeason
## #' @param game_type Game type to return information for a particular statistic in a particular game type.
## #' @param season Year to return information and ranking for a particular statistic in a given year. 
## #' @param sport_id The sport_id to return information and ranking information for.
## #' @param limit A limit to limit return to a particular number of records.
## #' 
## #' @return Returns a tibble with the following columns
## #'   |col_name                |types     |
## #'   |:-----------------------|:---------|
## #'   |total_splits            |integer   |
## #'   |season                  |character |
## #'   |num_teams               |integer   |
## #'   |rank                    |integer   |
## #'   |games_played            |integer   |
## #'   |ground_outs             |integer   |
## #'   |air_outs                |integer   |
## #'   |runs                    |integer   |
## #'   |doubles                 |integer   |
## #'   |triples                 |integer   |
## #'   |home_runs               |integer   |
## #'   |strike_outs             |integer   |
## #'   |base_on_balls           |integer   |
## #'   |intentional_walks       |integer   |
## #'   |hits                    |integer   |
## #'   |hit_by_pitch            |integer   |
## #'   |avg                     |character |
## #'   |at_bats                 |integer   |
## #'   |obp                     |character |
## #'   |slg                     |character |
## #'   |ops                     |character |
## #'   |caught_stealing         |integer   |
## #'   |stolen_bases            |integer   |
## #'   |stolen_base_percentage  |character |
## #'   |ground_into_double_play |integer   |
## #'   |number_of_pitches       |integer   |
## #'   |plate_appearances       |integer   |
## #'   |total_bases             |integer   |
## #'   |rbi                     |integer   |
## #'   |left_on_base            |integer   |
## #'   |sac_bunts               |integer   |
## #'   |sac_flies               |integer   |
## #'   |babip                   |character |
## #'   |ground_outs_to_airouts  |character |
## #'   |catchers_interference   |integer   |
## #'   |at_bats_per_home_run    |character |
## #'   |team_id                 |integer   |
## #'   |team_name               |character |
## #'   |team_link               |character |
## #'   |player_id               |integer   |
## #'   |player_full_name        |character |
## #'   |player_link             |character |
## #'   |player_first_name       |character |
## #'   |player_last_name        |character |
## #'   |league_id               |integer   |
## #'   |league_name             |character |
## #'   |league_link             |character |
## #'   |sport_id                |integer   |
## #'   |sport_link              |character |
## #'   |sport_abbreviation      |character |
## #'   |position_code           |character |
## #'   |position_name           |character |
## #'   |position_type           |character |
## #'   |position_abbreviation   |character |
## #'   |splits_tied_with_offset |list      |
## #'   |splits_tied_with_limit  |list      |
## #'   |player_pool             |character |
## #'   |type_display_name       |character |
## #'   |group_display_name      |character |
## #' @export
## #' @examples \donttest{
## #'  mlb_stats_steaks(streak_type = 'hittingStreakOverall', streak_span  = 'currentStreak', season = 2021)
## #' }
## mlb_stats_streaks <- function(
##   streak_type = NULL,
##   streak_span = NULL,
##   game_type = NULL,
##   season = NULL,
##   sport_id = 1,
##   limit = 1000){
##   
##   mlb_endpoint <- mlb_stats_endpoint("v1/stats/streaks")
##   query_params <- list(
##     streakType = streak_type,
##     streakSpan = streak_span,
##     gameType = game_type,
##     season = season,
##     sportId = sport_id,
##     limit = limit
##   )
##   
##   mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
##   
##   resp <- mlb_endpoint %>% 
##     mlb_api_call()
##   stats_leaders <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)  
##   stats_leaders$season <- NULL
##   stats <- stats_leaders %>% 
##     tidyr::unnest(.data$splits) %>% 
##     janitor::clean_names()  %>% 
##     as.data.frame() %>% 
##     dplyr::select(-.data$exemptions)
##   colnames(stats)<-gsub("stat_", "", colnames(stats))
##   return(stats)
## }
## 
## 