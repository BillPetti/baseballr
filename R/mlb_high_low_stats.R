#' @rdname mlb_high_low_stats
#' @title **Acquire time codes for Major and Minor League games**
#'
#' @param org_type The organization type for return information (*Required*). Valid values include:
#' - PLAYER
#' - TEAM
#' - DIVISION
#' - LEAGUE
#' - SPORT
#' @param season The season for which you want to return information (*Required*).
#' @param sort_stat The stat to sort the return  (*Required*). Valid values can be found from 'stat_lookup_param' below
#' 
#'  |stat_name                |stat_lookup_param    |is_counting |stat_label               |stat_groups                  |org_types    |high_low_types       |
#'  |:------------------------|:--------------------|:-----------|:------------------------|:----------------------------|:------------|:--------------------|
#'  |at_bats                  |atBats               |TRUE        |At bats                  |hitting , pitching           |PLAYER, TEAM |PLAYER, TEAM  , GAME |
#'  |total_plate_appearances  |plateAppearances     |TRUE        |Total plate appearances  |hitting                      |PLAYER, TEAM |PLAYER, TEAM  , GAME |
#'  |runs                     |runs                 |TRUE        |Runs                     |hitting                      |PLAYER, TEAM |PLAYER, TEAM  , GAME |
#'  |runs_batted_in           |rbi                  |TRUE        |Runs batted in           |hitting                      |PLAYER, TEAM |PLAYER, TEAM  , GAME |
#'  |home_team_runs           |runs                 |TRUE        |Home team runs           |hitting                      |TEAM         |GAME                 |
#'  |away_team_runs           |runs                 |TRUE        |Away team runs           |hitting                      |TEAM         |GAME                 |
#'  |hits                     |hits                 |TRUE        |Hits                     |hitting                      |PLAYER, TEAM |PLAYER, TEAM  , GAME |
#'  |hits_risp                |hitsRisp             |TRUE        |Hits risp                |hitting                      |PLAYER, TEAM |PLAYER, TEAM         |
#'  |home_team_hits           |hits                 |TRUE        |Home team hits           |hitting                      |TEAM         |GAME                 |
#'  |away_team_hits           |hits                 |TRUE        |Away team hits           |hitting                      |TEAM         |GAME                 |
#'  |total_bases              |totalBases           |TRUE        |Total bases              |hitting , pitching           |PLAYER, TEAM |PLAYER, TEAM         |
#'  |doubles                  |doubles              |TRUE        |Doubles                  |hitting , pitching           |PLAYER, TEAM |PLAYER, TEAM  , GAME |
#'  |triples                  |triples              |TRUE        |Triples                  |hitting                      |PLAYER, TEAM |PLAYER, TEAM  , GAME |
#'  |home_runs                |homeRuns             |TRUE        |Home runs                |hitting , pitching           |PLAYER, TEAM |PLAYER, TEAM         |
#'  |extra_base_hits          |extraBaseHits        |TRUE        |Extra base hits          |hitting                      |PLAYER, TEAM |PLAYER, TEAM         |
#'  |walks                    |baseOnBalls          |TRUE        |Walks                    |hitting , pitching           |PLAYER, TEAM |PLAYER, TEAM         |
#'  |strikeouts               |strikeouts           |TRUE        |Strikeouts               |hitting , pitching           |PLAYER, TEAM |PLAYER, TEAM         |
#'  |stolen_bases             |stolenBases          |TRUE        |Stolen bases             |hitting                      |PLAYER, TEAM |PLAYER, TEAM         |
#'  |caught_stealing          |caughtStealing       |TRUE        |Caught stealing          |hitting , pitching, fielding |PLAYER, TEAM |PLAYER, TEAM         |
#'  |sacrifice_flies          |sacFlies             |TRUE        |Sacrifice flies          |hitting                      |PLAYER, TEAM |PLAYER, TEAM         |
#'  |sacrifice_bunts          |sacBunts             |TRUE        |Sacrifice bunts          |hitting                      |PLAYER, TEAM |PLAYER, TEAM         |
#'  |hit_by_pitches           |hitByPitch           |TRUE        |Hit by pitches           |hitting , pitching           |PLAYER, TEAM |PLAYER, TEAM         |
#'  |left_on_base             |leftOnBase           |TRUE        |Left on base             |hitting                      |PLAYER, TEAM |PLAYER, TEAM         |
#'  |ground_into_double_plays |groundIntoDoublePlay |TRUE        |Ground into double plays |hitting , pitching           |PLAYER, TEAM |PLAYER, TEAM         |
#'  |strikes                  |strikes              |TRUE        |Strikes                  |pitching                     |PLAYER, TEAM |PLAYER, TEAM  , GAME |
#'  |pitches                  |pitchesThrown        |TRUE        |Pitches                  |pitching                     |PLAYER, TEAM |PLAYER, TEAM  , GAME |
#'  |balks                    |balks                |TRUE        |Balks                    |pitching                     |PLAYER, TEAM |PLAYER, TEAM  , GAME |
#'  |innings_pitched          |inningsPitched       |TRUE        |Innings pitched          |pitching                     |PLAYER       |PLAYER               |
#'  |errors                   |errors               |TRUE        |Errors                   |fielding                     |TEAM         |                     |
#'  |home_team_errors         |errors               |TRUE        |Home team errors         |fielding                     |TEAM         |                     |
#'  |away_team_errors         |errors               |TRUE        |Away team errors         |fielding                     |TEAM         |                     |
#'  |chances                  |chances              |TRUE        |Chances                  |fielding                     |TEAM         |                     |
#'  |put_outs                 |putOuts              |TRUE        |Put outs                 |fielding                     |TEAM         |                     |
#'  |assists                  |assists              |TRUE        |Assists                  |fielding                     |TEAM         |                     |
#'  |double_plays             |doublePlays          |TRUE        |Double plays             |fielding                     |TEAM         |                     |
#'  |attendance               |attendance           |TRUE        |Attendance               |game                         |TEAM         |GAME                 |
#'  |game_time                |gameDuration         |TRUE        |Game time                |game                         |TEAM         |GAME                 |
#'  |delay_time               |gameDuration         |TRUE        |Delay time               |game                         |TEAM         |GAME                 |
#'  |longest                  |gameDuration         |TRUE        |Longest                  |game                         |TEAM         |                     |
#'  |shortest                 |gameDuration         |TRUE        |Shortest                 |game                         |TEAM         |                     |
#'  |inning                   |innings              |TRUE        |Inning                   |game                         |TEAM         |GAME                 |
#'  |win_streak               |winStreak            |TRUE        |Win streak               |streak                       |TEAM         |                     |
#'  |loss_streak              |lossStreak           |TRUE        |Loss streak              |streak                       |TEAM         |                     |
#' @param team_ids The team_id(s) for which to return information.
#' @param league_ids The league_id(s) for which to return information.
#' @param sport_ids The sport_id(s) for which to return information.
#' @param game_type The game_type for which to return information.
#' @param stat_group Stat group for which to return information. Valid values include:
#'  |stat_group      |
#'  |:---------------|
#'  |hitting         |
#'  |pitching        |
#'  |fielding        |
#'  |catching        |
#'  |running         |
#'  |game            |
#'  |team            |
#'  |streak          |
#' @param limit Number of records as the limit of the return.  
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame with the following columns 
#'   |col_name               |types     |
#'   |:----------------------|:---------|
#'   |total_splits           |integer   |
#'   |season                 |character |
#'   |date                   |character |
#'   |is_home                |logical   |
#'   |rank                   |integer   |
#'   |game_innings           |integer   |
#'   |stat_at_bats           |integer   |
#'   |team_id                |integer   |
#'   |team_name              |character |
#'   |team_link              |character |
#'   |opponent_id            |integer   |
#'   |opponent_name          |character |
#'   |opponent_link          |character |
#'   |game_pk                |integer   |
#'   |game_link              |character |
#'   |game_number            |integer   |
#'   |game_content_link      |character |
#'   |home_team_id           |integer   |
#'   |home_team_name         |character |
#'   |home_team_link         |character |
#'   |away_team_id           |integer   |
#'   |away_team_name         |character |
#'   |away_team_link         |character |
#'   |combined_stats         |logical   |
#'   |group_display_name     |character |
#'   |game_type_id           |character |
#'   |game_type_description  |character |
#'   |sort_stat_name         |character |
#'   |sort_stat_lookup_param |character |
#'   |sort_stat_is_counting  |logical   |
#'   |sort_stat_label        |character |
#'  
#' @export
#' @examples \donttest{
#'  mlb_high_low_stats(org_type = 'Team', season = 2020, sort_stat = 'atBats')
#' }

mlb_high_low_stats <- function(
  org_type, 
  season, 
  sort_stat,
  team_ids = NULL,
  league_ids = NULL, 
  sport_ids = NULL, 
  game_type = NULL, 
  stat_group = NULL,
  limit = NULL
  ) {
  org_type <- tolower(org_type)
  query_params <- list(
    season = season, 
    sortStat = sort_stat,
    teamIds = team_ids,
    leagueId = league_ids,
    sportIds = sport_ids,
    gameType = game_type,
    limit = limit
  )
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/highLow/{org_type}"))
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call() %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE)
  high_low_results <- resp$highLowResults %>% 
    as.data.frame() %>% 
    dplyr::select(-.data$season)
  high_low_results_splits <- high_low_results %>% 
    tidyr::unnest(.data$splits) %>% 
    janitor::clean_names() %>% 
    dplyr::select(
      -.data$exemptions,
      -.data$splits_tied_with_offset,
      -.data$splits_tied_with_limit, 
      -.data$sort_stat_stat_groups,
      -.data$sort_stat_high_low_types,
      -.data$sort_stat_org_types,
      -.data$sort_stat_streak_levels) %>% 
    dplyr::rename(
      game_number = .data$game_game_number,
      game_pk = .data$game_game_pk)
  return(high_low_results_splits)
}
