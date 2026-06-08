#' **Get ESPN MLB game data (Pbp, Team and Player Box)**
#' @author Saiem Gilani
#' @param game_id Game ID
#' @return A named list of data frames: Plays, Team, Player
#'
#'    `espn_mlb_game_all()` bundles three tibbles returned in one call. Each
#'    component is identical to the matching standalone function -- see those
#'    for the full column documentation:
#'
#'    - **Plays** -- play-by-play, identical to [espn_mlb_pbp()].
#'    - **Team** -- team box score, identical to [espn_mlb_team_box()].
#'    - **Player** -- player box score, identical to [espn_mlb_player_box()].
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @import rvest
#' @export
#' @keywords MLB Game
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#' try(espn_mlb_game_all(game_id = 401283399))
#' }
#'
espn_mlb_game_all <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  summary_url <- "http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?"

  ## Inputs
  ## game_id
  full_url <- paste0(
    summary_url,
    "event=",
    game_id
  )

  pbp <- list(Plays = NULL, Team = NULL, Player = NULL)
  resp <- NULL
  plays_df <- NULL
  team_box_score <- NULL
  player_box_score <- NULL

  #---- Fetch the summary endpoint (single outer tryCatch) -------------------
  tryCatch(
    expr = {
      res <- .retry_request(full_url)
      check_status(res)
      resp <- res %>%
        .resp_text()
    },
    error = function(e) .report_api_error(
      e,
      hint = "Could not fetch game summary for game_id = {game_id}",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )

  if (is.null(resp)) {
    return(pbp)
  }

  #---- Play-by-Play ------
  tryCatch(
    expr = {
      plays_df <- helper_espn_mlb_pbp(resp)

      if (is.null(plays_df)) {
        message(sprintf("%s: No play-by-play data for %s available!", Sys.time(), game_id))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no play-by-play data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  #---- Team Box ------
  tryCatch(
    expr = {
      team_box_score <- helper_espn_mlb_team_box(resp)

      if (is.null(team_box_score)) {
        message(sprintf("%s: No team box score data for %s available!", Sys.time(), game_id))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no team box score data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  #---- Player Box ------
  tryCatch(
    expr = {
      player_box_score <- helper_espn_mlb_player_box(resp)

      if (is.null(player_box_score)) {
        message(sprintf("%s: No player box score data for %s available!", Sys.time(), game_id))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no player box score data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )

  pbp <- c(list(plays_df), list(team_box_score), list(player_box_score))
  names(pbp) <- c("Plays", "Team", "Player")
  return(pbp)
}


#' **Get ESPN MLB PBP data**
#' @author Saiem Gilani
#' @param game_id Game ID
#' @return A play-by-play data frame.
#'
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |id |character |Play / record identifier. |
#'    |sequence_number |character |Play sequence number within the game. |
#'    |text |character |Text description of the play. |
#'    |away_score |integer |Away team run total after the play. |
#'    |home_score |integer |Home team run total after the play. |
#'    |scoring_play |logical |TRUE if the play scored a run. |
#'    |score_value |integer |Runs scored on the play. |
#'    |wallclock |character |Wall-clock timestamp of the play (ISO 8601). |
#'    |at_bat_id |character |Identifier of the at-bat the play belongs to. |
#'    |summary_type |character |Play summary type. |
#'    |outs |integer |Outs in the inning after the play. |
#'    |type |character |Type. |
#'    |bat_order |integer |Spot in the batting order (1-9; NA if not applicable). |
#'    |at_bat_pitch_number |integer |Pitch number within the at-bat. |
#'    |pitch_velocity |integer |Pitch velocity (mph). |
#'    |trajectory |character |Batted-ball trajectory. |
#'    |alternative_play |character |Alternative play. |
#'    |type_id |integer |Type id. |
#'    |type_text |character |Type text. |
#'    |type_type |character |Type type. |
#'    |type_alternative_text |character |Type alternative text. |
#'    |type_abbreviation |character |Type abbreviation. |
#'    |period_type |character |Period type ('inning'). |
#'    |period_number |integer |Inning number. |
#'    |period_display_value |character |Inning display value (e.g. 'Top 1st'). |
#'    |team_id |integer |Unique ESPN team identifier. |
#'    |pitch_count_balls |integer |Balls in the count when the pitch was thrown. |
#'    |pitch_count_strikes |integer |Strikes in the count when the pitch was thrown. |
#'    |result_count_balls |integer |Balls in the count after the pitch. |
#'    |result_count_strikes |integer |Strikes in the count after the pitch. |
#'    |bats_type |character |Bats type. |
#'    |bats_abbreviation |character |Bats abbreviation. |
#'    |bats_display_value |character |Bats display value. |
#'    |pitch_coordinate_x |integer |Pitch location x-coordinate. |
#'    |pitch_coordinate_y |integer |Pitch location y-coordinate. |
#'    |pitch_type_id |character |Pitch type identifier. |
#'    |pitch_type_text |character |Pitch type description (e.g. 'Four-seam FB'). |
#'    |pitch_type_abbreviation |character |Pitch type abbreviation. |
#'    |hit_coordinate_x |integer |Batted-ball location x-coordinate. |
#'    |hit_coordinate_y |integer |Batted-ball location y-coordinate. |
#'    |alternative_type_id |character |Alternative type id. |
#'    |alternative_type_text |character |Alternative type text. |
#'    |alternative_type_abbreviation |character |Alternative type abbreviation. |
#'    |alternative_type_alternative_text |character |Alternative type alternative text. |
#'    |alternative_type_type |character |Alternative type type. |
#'    |on_first_athlete_id |character |Athlete id of the runner on first base. |
#'    |on_second_athlete_id |character |Athlete id of the runner on second base. |
#'    |play_id |character |Play id. |
#'    |athlete_id_1 |integer |Primary participating athlete id (batter). |
#'    |athlete_id_2 |integer |Second participating athlete id (pitcher). |
#'    |athlete_id_3 |integer |Third participating athlete id (fielder). |
#'    |home_team_id |integer |Home team id. |
#'    |home_team_mascot |character |Home team mascot. |
#'    |home_team_name |character |Home team name. |
#'    |home_team_abbrev |character |Home team abbrev. |
#'    |home_team_logo |character |Home team logo. |
#'    |home_team_logo_dark |character |Home team logo dark. |
#'    |home_team_full_name |character |Home team full name. |
#'    |home_team_color |character |Home team color. |
#'    |home_team_alternate_color |character |Home team alternate color. |
#'    |home_team_score |integer |Home team score. |
#'    |home_team_winner |logical |Home team winner. |
#'    |home_team_record |character |Home team record. |
#'    |away_team_id |integer |Away team id. |
#'    |away_team_mascot |character |Away team mascot. |
#'    |away_team_name |character |Away team name. |
#'    |away_team_abbrev |character |Away team abbrev. |
#'    |away_team_logo |character |Away team logo. |
#'    |away_team_logo_dark |character |Away team logo dark. |
#'    |away_team_full_name |character |Away team full name. |
#'    |away_team_color |character |Away team color. |
#'    |away_team_alternate_color |character |Away team alternate color. |
#'    |away_team_score |integer |Away team score. |
#'    |away_team_winner |logical |Away team winner. |
#'    |away_team_record |character |Away team record. |
#'    |game_id |integer |Unique ESPN game/event identifier. |
#'    |season |integer |Season (4-digit year). |
#'    |season_type |integer |ESPN season type (1=pre, 2=regular, 3=postseason, 4=off-season). |
#'    |game_date |Date |Game date (YYYY-MM-DD). |
#'    |game_date_time |POSIXct |Game start date/time (US/Eastern). |
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @import rvest
#' @export
#' @keywords MLB PBP
#' @family ESPN MLB Functions
#'
#' @examples
#' \donttest{
#' try(espn_mlb_pbp(game_id = 401071880))
#' }
#'
espn_mlb_pbp <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  summary_url <-
    "http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?"

  ## Inputs
  ## game_id
  full_url <- paste0(
    summary_url,
    "event=",
    game_id
  )

  plays_df <- NULL

  #---- Play-by-Play ------
  tryCatch(
    expr = {
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      plays_df <- helper_espn_mlb_pbp(resp)

      if (is.null(plays_df)) {
        return(message(sprintf("%s: No play-by-play data for %s available!", Sys.time(), game_id)))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no play-by-play data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )

  return(plays_df)
}


#' **Get ESPN MLB team box scores**
#' @author Saiem Gilani
#' @param game_id Game ID
#' @return A team boxscore data frame
#'
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |game_id |integer |Unique ESPN game/event identifier. |
#'    |season |integer |Season (4-digit year). |
#'    |season_type |integer |ESPN season type (1=pre, 2=regular, 3=postseason, 4=off-season). |
#'    |game_date |Date |Game date (YYYY-MM-DD). |
#'    |game_date_time |POSIXct |Game start date/time (US/Eastern). |
#'    |team_id |integer |Unique ESPN team identifier. |
#'    |team_uid |character |ESPN universal team identifier (UID). |
#'    |team_slug |character |URL-safe team identifier. |
#'    |team_location |character |Team city / location. |
#'    |team_name |character |Team nickname (e.g. 'Yankees'). |
#'    |team_abbreviation |character |Short team abbreviation (e.g. 'NYY'). |
#'    |team_display_name |character |Full team display name (e.g. 'New York Yankees'). |
#'    |team_short_display_name |character |Short team display name. |
#'    |team_color |character |Team primary color (hex, no leading '#'). |
#'    |team_alternate_color |character |Team alternate color (hex). |
#'    |team_logo |character |Team logo image URL. |
#'    |batting_games_played |character |Team batting: batting games played. |
#'    |batting_team_games_played |character |Team batting: batting team games played. |
#'    |batting_hit_by_pitch |character |Team batting: batting hit by pitch. |
#'    |batting_ground_balls |character |Team batting: batting ground balls. |
#'    |batting_strikeouts |character |Team batting: batting strikeouts. |
#'    |batting_rb_is |character |Team batting: batting rb is. |
#'    |batting_sac_hits |character |Team batting: batting sac hits. |
#'    |batting_hits |character |Team batting: batting hits. |
#'    |batting_stolen_bases |character |Team batting: batting stolen bases. |
#'    |batting_walks |character |Team batting: batting walks. |
#'    |batting_catcher_interference |character |Team batting: batting catcher interference. |
#'    |batting_runs |character |Team batting: batting runs. |
#'    |batting_gid_ps |character |Team batting: batting gid ps. |
#'    |batting_sac_flies |character |Team batting: batting sac flies. |
#'    |batting_at_bats |character |Team batting: batting at bats. |
#'    |batting_home_runs |character |Team batting: batting home runs. |
#'    |batting_grand_slam_home_runs |character |Team batting: batting grand slam home runs. |
#'    |batting_runners_left_on_base |character |Team batting: batting runners left on base. |
#'    |batting_triples |character |Team batting: batting triples. |
#'    |batting_game_winning_rb_is |character |Team batting: batting game winning rb is. |
#'    |batting_intentional_walks |character |Team batting: batting intentional walks. |
#'    |batting_doubles |character |Team batting: batting doubles. |
#'    |batting_fly_balls |character |Team batting: batting fly balls. |
#'    |batting_caught_stealing |character |Team batting: batting caught stealing. |
#'    |batting_pitches |character |Team batting: batting pitches. |
#'    |batting_games_started |character |Team batting: batting games started. |
#'    |batting_pinch_at_bats |character |Team batting: batting pinch at bats. |
#'    |batting_pinch_hits |character |Team batting: batting pinch hits. |
#'    |batting_player_rating |character |Team batting: batting player rating. |
#'    |batting_is_qualified |character |Team batting: batting is qualified. |
#'    |batting_is_qualified_steals |character |Team batting: batting is qualified steals. |
#'    |batting_total_bases |character |Team batting: batting total bases. |
#'    |batting_plate_appearances |character |Team batting: batting plate appearances. |
#'    |batting_projected_home_runs |character |Team batting: batting projected home runs. |
#'    |batting_extra_base_hits |character |Team batting: batting extra base hits. |
#'    |batting_runs_created |character |Team batting: batting runs created. |
#'    |batting_avg |character |Team batting: batting average. |
#'    |batting_pinch_avg |character |Team batting: batting pinch avg. |
#'    |batting_slug_avg |character |Team batting: batting slug avg. |
#'    |batting_secondary_avg |character |Team batting: batting secondary avg. |
#'    |batting_on_base_pct |character |Team batting: batting on base pct. |
#'    |batting_ops |character |Team batting: batting ops. |
#'    |batting_ground_to_fly_ratio |character |Team batting: batting ground to fly ratio. |
#'    |batting_runs_created_per27outs |character |Team batting: batting runs created per27outs. |
#'    |batting_batter_rating |character |Team batting: batting batter rating. |
#'    |batting_at_bats_per_home_run |character |Team batting: batting at bats per home run. |
#'    |batting_stolen_base_pct |character |Team batting: batting stolen base pct. |
#'    |batting_pitches_per_plate_appearance |character |Team batting: batting pitches per plate appearance. |
#'    |batting_isolated_power |character |Team batting: batting isolated power. |
#'    |batting_walk_to_strikeout_ratio |character |Team batting: batting walk to strikeout ratio. |
#'    |batting_walks_per_plate_appearance |character |Team batting: batting walks per plate appearance. |
#'    |batting_secondary_avg_minus_ba |character |Team batting: batting secondary avg minus ba. |
#'    |batting_runs_produced |character |Team batting: batting runs produced. |
#'    |batting_runs_ratio |character |Team batting: batting runs ratio. |
#'    ...and 128 further ESPN stat columns (full batting / pitching / fielding stat set).
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @import rvest
#' @export
#' @keywords MLB Team Box
#' @family ESPN MLB Functions
#'
#' @examples
#' \donttest{
#' try(espn_mlb_team_box(game_id = 401071880))
#' }
#'
espn_mlb_team_box <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  summary_url <-
    "http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?"

  ## Inputs
  ## game_id
  full_url <- paste0(
    summary_url,
    "event=",
    game_id
  )

  #---- Team Box ------
  team_box_score <- NULL

  tryCatch(
    expr = {
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      team_box_score <- helper_espn_mlb_team_box(resp)

      if (is.null(team_box_score)) {
        return(message(sprintf("%s: No team box score data for %s available!", Sys.time(), game_id)))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no team box score data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(team_box_score)
}
#' **Get ESPN MLB player box scores**
#' @author Saiem Gilani
#' @param game_id Game ID
#' @return A player boxscore data frame
#'
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |game_id |integer |Unique ESPN game/event identifier. |
#'    |season |integer |Season (4-digit year). |
#'    |season_type |integer |ESPN season type (1=pre, 2=regular, 3=postseason, 4=off-season). |
#'    |game_date |Date |Game date (YYYY-MM-DD). |
#'    |game_date_time |POSIXct |Game start date/time (US/Eastern). |
#'    |stat_group |character |Statistic side this row reports: 'batting' or 'pitching'. |
#'    |team_id |integer |Unique ESPN team identifier. |
#'    |team_name |character |Team nickname (e.g. 'Yankees'). |
#'    |team_abbreviation |character |Short team abbreviation (e.g. 'NYY'). |
#'    |team_display_name |character |Full team display name (e.g. 'New York Yankees'). |
#'    |athlete_id |integer |Unique ESPN athlete identifier. |
#'    |athlete_display_name |character |Athlete display name (full). |
#'    |athlete_short_name |character |Athlete short display name. |
#'    |athlete_position_name |character |Athlete fielding position (e.g. 'Shortstop', 'Pitcher'). |
#'    |athlete_position_abbreviation |character |Position abbreviation (e.g. 'SS', 'P'). |
#'    |starter |logical |TRUE if the player started the game. |
#'    |bat_order |integer |Spot in the batting order (1-9; NA if not applicable). |
#'    |active |logical |TRUE if the row represents an active record. |
#'    |h_ab |character |Hits-for-At-bats line (e.g. '2-4'). |
#'    |ab |character |At-bats. |
#'    |r |character |Runs scored. |
#'    |h |character |Hits. |
#'    |rbi |character |Runs batted in. |
#'    |hr |character |Home runs. |
#'    |bb |character |Bases on balls (walks). |
#'    |k |character |Strikeouts. |
#'    |number_p |character |Pitches seen / thrown. |
#'    |avg |character |Batting average. |
#'    |obp |character |On-base percentage. |
#'    |slg |character |Slugging percentage. |
#'    |ip |character |Innings pitched. |
#'    |er |character |Earned runs. |
#'    |pc_st |character |Pitch count - strikes (e.g. '95-58'). |
#'    |era |character |Earned run average. |
#'    |pc |character |Pitch count. |
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @import rvest
#' @export
#' @keywords MLB Player Box
#' @family ESPN MLB Functions
#'
#' @examples
#' \donttest{
#' try(espn_mlb_player_box(game_id = 401071880))
#' }
#'
espn_mlb_player_box <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  summary_url <-
    "http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?"

  ## Inputs
  ## game_id
  full_url <- paste0(
    summary_url,
    "event=",
    game_id
  )

  #---- Player Box ------
  player_box_score <- NULL

  tryCatch(
    expr = {
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      player_box_score <- helper_espn_mlb_player_box(resp)

      if (is.null(player_box_score)) {
        return(message(sprintf("%s: No player box score data for %s available!", Sys.time(), game_id)))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no player box score data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(player_box_score)
}


#' **Get ESPN MLB game rosters**
#' @author Saiem Gilani
#' @param game_id Game ID
#' @return A game rosters data frame
#'
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |athlete_id |integer |Unique ESPN athlete identifier. |
#'    |athlete_uid |character |Athlete uid. |
#'    |athlete_guid |character |Athlete guid. |
#'    |athlete_type |character |Athlete type. |
#'    |sdr |integer |Sdr. |
#'    |first_name |character |First name. |
#'    |last_name |character |Last name. |
#'    |full_name |character |Full name. |
#'    |athlete_display_name |character |Athlete display name (full). |
#'    |short_name |character |Short display name. |
#'    |weight |integer |Weight. |
#'    |display_weight |character |Display weight. |
#'    |height |integer |Height. |
#'    |display_height |character |Display height. |
#'    |age |integer |Age. |
#'    |date_of_birth |character |Date of birth. |
#'    |debut_year |integer |Debut year. |
#'    |birth_place_city |character |Birth place city. |
#'    |birth_place_state |character |Birth place state. |
#'    |birth_place_country |character |Birth place country. |
#'    |slug |character |Slug. |
#'    |headshot_href |character |Headshot href. |
#'    |headshot_alt |character |Headshot alt. |
#'    |athlete_jersey_number |character |Athlete jersey number. |
#'    |position_id |integer |Position id. |
#'    |position_name |character |Position name. |
#'    |position_display_name |character |Position display name. |
#'    |position_abbreviation |character |Position abbreviation. |
#'    |position_leaf |logical |Position leaf. |
#'    |positions_ref |character |Positions ref. |
#'    |positions_id |character |Positions id. |
#'    |positions_name |character |Positions name. |
#'    |positions_display_name |character |Positions display name. |
#'    |positions_abbreviation |character |Positions abbreviation. |
#'    |positions_leaf |logical |Positions leaf. |
#'    |positions_parent_ref |character |Positions parent ref. |
#'    |positions_statistics_ref |character |Positions statistics ref. |
#'    |linked |logical |Linked. |
#'    |years |integer |Years. |
#'    |debut_year_2 |integer |Debut year 2. |
#'    |debut_ref |character |Debut ref. |
#'    |debut_ref_1 |character |Debut ref 1. |
#'    |active |logical |TRUE if the row represents an active record. |
#'    |status_id |integer |Status id. |
#'    |status_name |character |Game status (e.g. 'STATUS_FINAL'). |
#'    |status_type |character |Status type. |
#'    |status_abbreviation |character |Status abbreviation. |
#'    |bats_type |character |Bats type. |
#'    |bats_abbreviation |character |Bats abbreviation. |
#'    |bats_display_value |character |Bats display value. |
#'    |throws_type |character |Throws type. |
#'    |throws_abbreviation |character |Throws abbreviation. |
#'    |throws_display_value |character |Throws display value. |
#'    |starter |logical |TRUE if the player started the game. |
#'    |valid |logical |Valid. |
#'    |display_name |character |Display name. |
#'    |bat_order |integer |Spot in the batting order (1-9; NA if not applicable). |
#'    |record_number |integer |Record number. |
#'    |at_bats |list |At bats. |
#'    |positions |list |Positions. |
#'    |notes |list |Notes. |
#'    |subbed_in_did_sub |logical |Subbed in did sub. |
#'    |subbed_out_did_sub |logical |Subbed out did sub. |
#'    |team_id |integer |Unique ESPN team identifier. |
#'    |team_guid |character |ESPN team GUID. |
#'    |team_uid |character |ESPN universal team identifier (UID). |
#'    |team_sdr |character |Team sdr. |
#'    |team_slug |character |URL-safe team identifier. |
#'    |team_location |character |Team city / location. |
#'    |team_name |character |Team nickname (e.g. 'Yankees'). |
#'    |team_abbreviation |character |Short team abbreviation (e.g. 'NYY'). |
#'    |team_display_name |character |Full team display name (e.g. 'New York Yankees'). |
#'    |team_short_display_name |character |Short team display name. |
#'    |team_color |character |Team primary color (hex, no leading '#'). |
#'    |team_alternate_color |character |Team alternate color (hex). |
#'    |team_is_active |logical |Team is active. |
#'    |is_all_star |logical |Is all star. |
#'    |logo_href |character |Logo href. |
#'    |logo_dark_href |character |Logo dark href. |
#'    |logos_href_2 |character |Logos href 2. |
#'    |logos_href_3 |character |Logos href 3. |
#'    |logos_href_4 |character |Logos href 4. |
#'    |logos_width_4 |integer |Logos width 4. |
#'    |logos_height_4 |integer |Logos height 4. |
#'    |logos_alt_4 |character |Logos alt 4. |
#'    |logos_rel_full_4 |character |Logos rel full 4. |
#'    |logos_rel_primary_logo_on_white_color |character |Logos rel primary logo on white color. |
#'    |logos_last_updated_4 |character |Logos last updated 4. |
#'    |logos_href_5 |character |Logos href 5. |
#'    |logos_width_5 |integer |Logos width 5. |
#'    |logos_height_5 |integer |Logos height 5. |
#'    |logos_alt_5 |character |Logos alt 5. |
#'    |logos_rel_full_5 |character |Logos rel full 5. |
#'    |logos_rel_primary_logo_on_black_color |character |Logos rel primary logo on black color. |
#'    |logos_last_updated_5 |character |Logos last updated 5. |
#'    |logos_href_6 |character |Logos href 6. |
#'    |logos_width_6 |integer |Logos width 6. |
#'    |logos_height_6 |integer |Logos height 6. |
#'    |logos_alt_6 |character |Logos alt 6. |
#'    |logos_rel_full_6 |character |Logos rel full 6. |
#'    |logos_rel_primary_logo_on_primary_color |character |Logos rel primary logo on primary color. |
#'    |logos_last_updated_6 |character |Logos last updated 6. |
#'    |logos_href_7 |character |Logos href 7. |
#'    |logos_width_7 |integer |Logos width 7. |
#'    |logos_height_7 |integer |Logos height 7. |
#'    |logos_alt_7 |character |Logos alt 7. |
#'    |logos_rel_full_7 |character |Logos rel full 7. |
#'    |logos_rel_primary_logo_on_secondary_color |character |Logos rel primary logo on secondary color. |
#'    |logos_last_updated_7 |character |Logos last updated 7. |
#'    |logos_href_8 |character |Logos href 8. |
#'    |logos_width_8 |integer |Logos width 8. |
#'    |logos_height_8 |integer |Logos height 8. |
#'    |logos_alt_8 |character |Logos alt 8. |
#'    |logos_rel_full_8 |character |Logos rel full 8. |
#'    |logos_rel_primary_logo_black |character |Logos rel primary logo black. |
#'    |logos_last_updated_8 |character |Logos last updated 8. |
#'    |logos_href_9 |character |Logos href 9. |
#'    |logos_width_9 |integer |Logos width 9. |
#'    |logos_height_9 |integer |Logos height 9. |
#'    |logos_alt_9 |character |Logos alt 9. |
#'    |logos_rel_full_9 |character |Logos rel full 9. |
#'    |logos_rel_primary_logo_white |character |Logos rel primary logo white. |
#'    |logos_last_updated_9 |character |Logos last updated 9. |
#'    |logos_href_10 |character |Logos href 10. |
#'    |logos_width_10 |integer |Logos width 10. |
#'    |logos_height_10 |integer |Logos height 10. |
#'    |logos_alt_10 |character |Logos alt 10. |
#'    |logos_rel_full_10 |character |Logos rel full 10. |
#'    |logos_rel_secondary_logo_on_white_color |character |Logos rel secondary logo on white color. |
#'    |logos_last_updated_10 |character |Logos last updated 10. |
#'    |logos_href_11 |character |Logos href 11. |
#'    |logos_width_11 |integer |Logos width 11. |
#'    |logos_height_11 |integer |Logos height 11. |
#'    |logos_alt_11 |character |Logos alt 11. |
#'    |logos_rel_full_11 |character |Logos rel full 11. |
#'    |logos_rel_secondary_logo_on_black_color |character |Logos rel secondary logo on black color. |
#'    |logos_last_updated_11 |character |Logos last updated 11. |
#'    |logos_href_12 |character |Logos href 12. |
#'    |logos_width_12 |integer |Logos width 12. |
#'    |logos_height_12 |integer |Logos height 12. |
#'    |logos_alt_12 |character |Logos alt 12. |
#'    |logos_rel_full_12 |character |Logos rel full 12. |
#'    |logos_rel_secondary_logo_on_primary_color |character |Logos rel secondary logo on primary color. |
#'    |logos_last_updated_12 |character |Logos last updated 12. |
#'    |logos_href_13 |character |Logos href 13. |
#'    |logos_width_13 |integer |Logos width 13. |
#'    |logos_height_13 |integer |Logos height 13. |
#'    |logos_alt_13 |character |Logos alt 13. |
#'    |logos_rel_full_13 |character |Logos rel full 13. |
#'    |logos_rel_secondary_logo_on_secondary_color |character |Logos rel secondary logo on secondary color. |
#'    |logos_last_updated_13 |character |Logos last updated 13. |
#'    |logos_href_14 |character |Logos href 14. |
#'    |logos_width_14 |integer |Logos width 14. |
#'    |logos_height_14 |integer |Logos height 14. |
#'    |logos_alt_14 |character |Logos alt 14. |
#'    |logos_rel_full_14 |character |Logos rel full 14. |
#'    |logos_rel_secondary_logo_black |character |Logos rel secondary logo black. |
#'    |logos_last_updated_14 |character |Logos last updated 14. |
#'    |logos_href_15 |character |Logos href 15. |
#'    |logos_width_15 |integer |Logos width 15. |
#'    |logos_height_15 |integer |Logos height 15. |
#'    |logos_alt_15 |character |Logos alt 15. |
#'    |logos_rel_full_15 |character |Logos rel full 15. |
#'    |logos_rel_secondary_logo_white |character |Logos rel secondary logo white. |
#'    |logos_last_updated_15 |character |Logos last updated 15. |
#'    |game_id |integer |Unique ESPN game/event identifier. |
#'    |order |integer |Order. |
#'    |home_away |character |Venue label for the team ('home' or 'away'). |
#'    |winner |logical |Winner. |
#'    |nickname |character |Team nickname. |
#'    |draft_display_text |character |Draft display text. |
#'    |draft_round |integer |Draft round. |
#'    |draft_year |integer |Draft year. |
#'    |draft_selection |integer |Draft selection. |
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @import rvest
#' @export
#' @keywords MLB Game Roster
#' @family ESPN MLB Functions
#'
#' @examples
#' \donttest{
#' try(espn_mlb_game_rosters(game_id = 401283399))
#' }
espn_mlb_game_rosters <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  athlete_roster_df <- .empty_baseballr_data("ESPN MLB Game Roster Information from ESPN.com")

  tryCatch(
    expr = {
      play_base_url <- paste0(
        "https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/events/",
        game_id,
        "/competitions/",
        game_id,
        "/competitors/"
      )
      game_res <- .retry_request(play_base_url)
      # Check the result
      check_status(game_res)

      game_resp <- game_res %>%
        .resp_text()
      game_df <- jsonlite::fromJSON(game_resp)[["items"]] %>%
        jsonlite::toJSON() %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        dplyr::rename("team_statistics_href" = "statistics.$ref")

      colnames(game_df) <- gsub(".\\$ref", "_href", colnames(game_df))

      game_df <- game_df %>%
        dplyr::rename(
          "team_id" = "id",
          "team_uid" = "uid"
        )

      game_df$game_id <- game_id

      teams_df <- purrr::map_dfr(game_df$team_href, function(x) {
        res <- .retry_request(x)
        # Check the result
        check_status(res)

        team_df <- res %>%
          .resp_text() %>%
          jsonlite::fromJSON(
            simplifyDataFrame = FALSE,
            simplifyVector = FALSE,
            simplifyMatrix = FALSE
          )

        team_df[["links"]] <- NULL
        team_df[["injuries"]] <- NULL
        team_df[["record"]] <- NULL
        team_df[["athletes"]] <- NULL
        team_df[["venue"]] <- NULL
        team_df[["groups"]] <- NULL
        team_df[["ranks"]] <- NULL
        team_df[["statistics"]] <- NULL
        team_df[["leaders"]] <- NULL
        team_df[["links"]] <- NULL
        team_df[["notes"]] <- NULL
        team_df[["franchise"]] <- NULL
        team_df[["againstTheSpreadRecords"]] <- NULL
        team_df[["oddsRecords"]] <- NULL
        team_df[["college"]] <- NULL
        team_df[["transactions"]] <- NULL
        team_df[["leaders"]] <- NULL
        team_df[["depthCharts"]] <- NULL
        team_df[["awards"]] <- NULL
        team_df[["events"]] <- NULL

        team_df <- team_df %>%
          purrr::map_if(is.list, as.data.frame) %>%
          as.data.frame() %>%
          dplyr::select(
            -dplyr::any_of(
              c(
                "logos.width",
                "logos.height",
                "logos.alt",
                "logos.rel..full.",
                "logos.rel..default.",
                "logos.rel..scoreboard.",
                "logos.rel..scoreboard..1",
                "logos.rel..scoreboard.2",
                "logos.lastUpdated",
                "logos.width.1",
                "logos.height.1",
                "logos.alt.1",
                "logos.rel..full..1",
                "logos.rel..dark.",
                "logos.rel..dark..1",
                "logos.lastUpdated.1",
                "logos.width.2",
                "logos.height.2",
                "logos.alt.2",
                "logos.rel..full..2",
                "logos.rel..scoreboard.",
                "logos.lastUpdated.2",
                "logos.width.3",
                "logos.height.3",
                "logos.alt.3",
                "logos.rel..full..3",
                "logos.lastUpdated.3",
                "X.ref",
                "X.ref.1",
                "X.ref.2"
              )
            )
          ) %>%
          janitor::clean_names()

        colnames(team_df)[1:13] <- paste0("team_", colnames(team_df)[1:13])

        team_df <- team_df %>%
          dplyr::rename(
            "logo_href" = "logos_href",
            "logo_dark_href" = "logos_href_1"
          ) %>%
          dplyr::left_join(
            game_df %>%
              dplyr::select(
                "game_id",
                "team_id",
                "team_uid",
                "order",
                "homeAway",
                "winner",
                "roster_href"
              ),
            by = c(
              "team_id" = "team_id",
              "team_uid" = "team_uid"
            )
          )
      })

      ## Inputs
      ## game_id
      team_roster_df <- purrr::map_dfr(teams_df$team_id, function(x) {
        res <- .retry_request(paste0(play_base_url, x, "/roster"))

        # Check the result
        check_status(res)

        resp <- res %>%
          .resp_text()

        raw_play_df <- jsonlite::fromJSON(resp)[["entries"]]

        raw_play_df <- raw_play_df %>%
          jsonlite::toJSON() %>%
          jsonlite::fromJSON(flatten = TRUE) %>%
          dplyr::mutate(team_id = x) %>%
          dplyr::select(-"period", -"forPlayerId", -"active")

        raw_play_df <- raw_play_df %>%
          dplyr::left_join(teams_df, by = c("team_id" = "team_id"))
      })

      colnames(team_roster_df) <- gsub(
        ".\\$ref",
        "_href",
        colnames(team_roster_df)
      )

      athlete_roster_df <- purrr::map_dfr(
        team_roster_df$athlete_href,
        function(x) {
          res <- .retry_request(x)

          # Check the result
          check_status(res)

          resp <- res %>%
            .resp_text()

          raw_play_df <- jsonlite::fromJSON(resp, flatten = TRUE)
          raw_play_df[["links"]] <- NULL
          raw_play_df[["injuries"]] <- NULL
          raw_play_df[["teams"]] <- NULL
          raw_play_df[["team"]] <- NULL
          raw_play_df[["college"]] <- NULL
          raw_play_df[["proAthlete"]] <- NULL
          raw_play_df[["statistics"]] <- NULL
          raw_play_df[["notes"]] <- NULL
          raw_play_df[["eventLog"]] <- NULL
          raw_play_df[["$ref"]] <- NULL
          raw_play_df[["position"]][["$ref"]] <- NULL

          raw_play_df2 <- raw_play_df %>%
            jsonlite::toJSON() %>%
            jsonlite::fromJSON(flatten = TRUE) %>%
            as.data.frame() %>%
            dplyr::mutate(id = as.integer(.data$id)) %>%
            dplyr::rename(
              "athlete_id" = "id",
              "athlete_uid" = "uid",
              "athlete_guid" = "guid",
              "athlete_type" = "type",
              "athlete_display_name" = "displayName",
              "athlete_jersey_number" = "jersey"
            )

          raw_play_df2 <- raw_play_df2 %>%
            dplyr::left_join(team_roster_df, by = c("athlete_id" = "playerId"))
        }
      )

      colnames(athlete_roster_df) <- gsub(
        ".\\$ref",
        "_href",
        colnames(athlete_roster_df)
      )

      athlete_roster_df <- athlete_roster_df %>%
        janitor::clean_names() %>%
        dplyr::select(
          -dplyr::any_of(c(
            "x_ref",
            "x_ref_1",
            "contract_ref",
            "contract_ref_1",
            "contract_ref_2",
            "draft_ref",
            "draft_ref_1",
            "athlete_href",
            "position_ref",
            "position_href",
            "roster_href",
            "statistics_href"
          ))
        ) %>%
        dplyr::mutate_at(
          c(
            "game_id",
            "athlete_id",
            "team_id",
            "position_id",
            "status_id",
            "sdr"
          ),
          as.integer
        ) %>%
        make_baseballr_data(
          "ESPN MLB Game Roster Information from ESPN.com",
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no game roster data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(athlete_roster_df)
}


#' **Get ESPN MLB team names and IDs**
#' @author Saiem Gilani
#' @return A teams data frame
#'
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |abbreviation |character |Short abbreviation. |
#'    |alternate_color |character |Alternate color (hex). |
#'    |color |character |Primary color (hex). |
#'    |display_name |character |Display name. |
#'    |team_id |integer |Unique ESPN team identifier. |
#'    |team |character |Team. |
#'    |logo |character |Logo image URL. |
#'    |logo_dark |character |Dark-mode logo image URL. |
#'    |logos_href_3 |character |Logos href 3. |
#'    |logos_href_4 |character |Logos href 4. |
#'    |logos_href_5 |character |Logos href 5. |
#'    |logos_href_6 |character |Logos href 6. |
#'    |logos_href_7 |character |Logos href 7. |
#'    |logos_href_8 |character |Logos href 8. |
#'    |logos_href_9 |character |Logos href 9. |
#'    |logos_href_10 |character |Logos href 10. |
#'    |logos_href_11 |character |Logos href 11. |
#'    |logos_href_12 |character |Logos href 12. |
#'    |logos_href_13 |character |Logos href 13. |
#'    |logos_href_14 |character |Logos href 14. |
#'    |logos_href_15 |character |Logos href 15. |
#'    |logos_href_16 |character |Logos href 16. |
#'    |mascot |character |Team mascot. |
#'    |nickname |character |Team nickname. |
#'    |short_name |character |Short display name. |
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows row_number group_by mutate as_tibble ungroup
#' @importFrom tidyr unnest unnest_wider everything pivot_wider
#' @import rvest
#' @export
#' @keywords MLB Teams
#' @family ESPN MLB Functions
#'
#' @examples
#' \donttest{
#' try(espn_mlb_teams())
#' }
#'
espn_mlb_teams <- function() {
  .args <- .capture_args()
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  teams_url <- "http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/teams?limit=1000"

  teams <- .empty_baseballr_data("ESPN MLB Teams Information from ESPN.com")

  tryCatch(
    expr = {
      res <- .retry_request(teams_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      leagues <- jsonlite::fromJSON(resp)[["sports"]][["leagues"]][[1]][[
        "teams"
      ]][[1]][["team"]] %>%
        dplyr::group_by(.data$id) %>%
        tidyr::unnest_wider("logos", names_sep = "_") %>%
        tidyr::unnest_wider("logos_href", names_sep = "_") %>%
        dplyr::select(
          -"logos_width",
          -"logos_height",
          -"logos_alt",
          -"logos_rel"
        ) %>%
        dplyr::ungroup()

      if ("records" %in% colnames(leagues)) {
        records <- leagues$record
        records <- records %>%
          tidyr::unnest_wider("items") %>%
          tidyr::unnest_wider("stats", names_sep = "_") %>%
          dplyr::mutate(row = dplyr::row_number())
        stat <- records %>%
          dplyr::group_by(.data$row) %>%
          purrr::map_if(is.data.frame, list)
        stat <- lapply(stat$stats_1, function(x) {
          x %>%
            purrr::map_if(is.data.frame, list) %>%
            dplyr::as_tibble()
        })

        s <- lapply(stat, function(x) {
          tidyr::pivot_wider(x)
        })

        s <- tibble::tibble(g = s)
        stats <- s %>%
          tidyr::unnest_wider("g")

        records <- dplyr::bind_cols(records %>% dplyr::select("summary"), stats)
        leagues <- leagues %>%
          dplyr::select(-"record")
      }
      leagues <- leagues %>%
        dplyr::select(
          -"links",
          -"isActive",
          -"isAllStar",
          -"uid",
          -"slug"
        )
      teams <- leagues %>%
        dplyr::rename(
          "logo" = "logos_href_1",
          "logo_dark" = "logos_href_2",
          "mascot" = "name",
          "team" = "location",
          "team_id" = "id",
          "short_name" = "shortDisplayName",
          "alternate_color" = "alternateColor",
          "display_name" = "displayName"
        ) %>%
        janitor::clean_names() %>%
        dplyr::mutate(team_id = as.integer(.data$team_id)) %>%
        make_baseballr_data("ESPN MLB Teams Information from ESPN.com", Sys.time())
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no teams data available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(teams)
}


#' **Get ESPN MLB current team roster**
#'
#' @author Saiem Gilani
#' @param team_id Either numeric or character (YYYY)
#' @return A teams data frame
#'
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |team_id |integer |Unique ESPN team identifier. |
#'    |team_uid |character |ESPN universal team identifier (UID). |
#'    |team_slug |character |URL-safe team identifier. |
#'    |team_location |character |Team city / location. |
#'    |team_name |character |Team nickname (e.g. 'Yankees'). |
#'    |team_abbreviation |character |Short team abbreviation (e.g. 'NYY'). |
#'    |team_display_name |character |Full team display name (e.g. 'New York Yankees'). |
#'    |team_short_name |character |Team short name. |
#'    |team_color |character |Team primary color (hex, no leading '#'). |
#'    |team_alternate_color |character |Team alternate color (hex). |
#'    |team_is_active |logical |Team is active. |
#'    |franchise..ref |character |Franchise..ref. |
#'    |franchise.id |character |Franchise.id. |
#'    |franchise.uid |character |Franchise.uid. |
#'    |franchise.slug |character |Franchise.slug. |
#'    |franchise.location |character |Franchise.location. |
#'    |franchise.name |character |Franchise.name. |
#'    |franchise.abbreviation |character |Franchise.abbreviation. |
#'    |franchise.displayName |character |Franchise.displayName. |
#'    |franchise.shortDisplayName |character |Franchise.shortDisplayName. |
#'    |franchise.color |character |Franchise.color. |
#'    |franchise.isActive |logical |Franchise.isActive. |
#'    |franchise.venue..ref |character |Franchise.venue..ref. |
#'    |franchise.venue.id |character |Franchise.venue.id. |
#'    |franchise.venue.fullName |character |Franchise.venue.fullName. |
#'    |franchise.venue.shortName |character |Franchise.venue.shortName. |
#'    |franchise.venue.address.city |character |Franchise.venue.address.city. |
#'    |franchise.venue.address.state |character |Franchise.venue.address.state. |
#'    |franchise.venue.address.zipCode |character |Franchise.venue.address.zipCode. |
#'    |franchise.venue.grass |logical |Franchise.venue.grass. |
#'    |franchise.venue.indoor |logical |Franchise.venue.indoor. |
#'    |franchise.venue.images.href |character |Franchise.venue.images.href. |
#'    |franchise.venue.images.width |integer |Franchise.venue.images.width. |
#'    |franchise.venue.images.height |integer |Franchise.venue.images.height. |
#'    |franchise.venue.images.alt |character |Franchise.venue.images.alt. |
#'    |franchise.venue.images.rel |list |Franchise.venue.images.rel. |
#'    |franchise..ref.1 |character |Franchise..ref.1. |
#'    |franchise..ref.2 |character |Franchise..ref.2. |
#'    |standingSummary |character |StandingSummary. |
#'    |logo |character |Logo image URL. |
#'    |logo_dark |character |Dark-mode logo image URL. |
#'    |group_id |integer |Group id. |
#'    |parent_group_id |integer |Parent group id. |
#'    |group_is_conference |logical |Group is conference. |
#'    |conference_id |integer |Conference id. |
#'    |athlete_id |integer |Unique ESPN athlete identifier. |
#'    |athlete_uid |character |Athlete uid. |
#'    |athlete_guid |character |Athlete guid. |
#'    |athlete_type |character |Athlete type. |
#'    |athlete_first_name |character |Athlete first name. |
#'    |athlete_last_name |character |Athlete last name. |
#'    |athlete_full_name |character |Athlete full name. |
#'    |athlete_display_name |character |Athlete display name (full). |
#'    |athlete_short_name |character |Athlete short display name. |
#'    |athlete_weight |integer |Athlete weight. |
#'    |athlete_display_weight |character |Athlete display weight. |
#'    |athlete_height |integer |Athlete height. |
#'    |athlete_display_height |character |Athlete display height. |
#'    |athlete_age |integer |Athlete age. |
#'    |athlete_date_of_birth |character |Athlete date of birth. |
#'    |athlete_slug |character |Athlete slug. |
#'    |athlete_jersey |character |Athlete jersey. |
#'    |athlete_linked |logical |Athlete linked. |
#'    |athlete_active |logical |Athlete active. |
#'    |athlete_positions |list |Athlete positions. |
#'    |athlete_hot_zones |list |Athlete hot zones. |
#'    |athlete_debut_year |integer |Athlete debut year. |
#'    |athlete_nickname |character |Athlete nickname. |
#'    |athlete_middle_name |character |Athlete middle name. |
#'    |athlete_citizenship |character |Athlete citizenship. |
#'    |athlete_alternate_ids_sdr |character |Athlete alternate ids sdr. |
#'    |athlete_birth_place_city |character |Athlete birth place city. |
#'    |athlete_birth_place_state |character |Athlete birth place state. |
#'    |athlete_birth_place_country |character |Athlete birth place country. |
#'    |athlete_position_id |character |Athlete position id. |
#'    |athlete_position_name |character |Athlete fielding position (e.g. 'Shortstop', 'Pitcher'). |
#'    |athlete_position_display_name |character |Athlete position display name. |
#'    |athlete_position_abbreviation |character |Position abbreviation (e.g. 'SS', 'P'). |
#'    |athlete_position_leaf |logical |Athlete position leaf. |
#'    |athlete_experience_years |integer |Athlete experience years. |
#'    |athlete_debut_year_2 |integer |Athlete debut year 2. |
#'    |athlete_status_id |character |Athlete status id. |
#'    |athlete_status_name |character |Athlete status name. |
#'    |athlete_status_type |character |Athlete status type. |
#'    |athlete_status_abbreviation |character |Athlete status abbreviation. |
#'    |athlete_bats_type |character |Athlete bats type. |
#'    |athlete_bats_abbreviation |character |Athlete bats abbreviation. |
#'    |athlete_bats_display_value |character |Athlete bats display value. |
#'    |athlete_throws_type |character |Athlete throws type. |
#'    |athlete_throws_abbreviation |character |Athlete throws abbreviation. |
#'    |athlete_throws_display_value |character |Athlete throws display value. |
#'    |athlete_headshot_href |character |Athlete headshot href. |
#'    |athlete_draft_display_text |character |Athlete draft display text. |
#'    |athlete_draft_round |integer |Athlete draft round. |
#'    |athlete_draft_year |integer |Athlete draft year. |
#'    |athlete_draft_selection |integer |Athlete draft selection. |
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows row_number group_by mutate as_tibble ungroup
#' @importFrom tidyr unnest unnest_wider everything pivot_wider
#' @import rvest
#' @export
#' @keywords MLB Team Roster
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#' try(espn_mlb_team_current_roster(team_id = 13))
#' }
espn_mlb_team_current_roster <- function(team_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  team_roster_full <- .empty_baseballr_data("ESPN MLB Team Current Roster Information from ESPN.com")

  tryCatch(
    expr = {
      teams_base_url <- sprintf(
        "http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/teams/%s?enable=roster",
        team_id
      )

      res <- .retry_request(teams_base_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      team_roster <- resp %>%
        jsonlite::fromJSON() %>%
        purrr::pluck("team") %>%
        purrr::discard_at(c("links", "nextEvent", "record"))

      players <- team_roster %>%
        purrr::pluck("athletes") %>%
        purrr::discard_at(c("links", "injuries", "teams")) %>%
        jsonlite::toJSON() %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        dplyr::as_tibble() %>%
        dplyr::select(
          -dplyr::any_of(c(
            "headshot.width",
            "headshot.height",
            "headshot.alt",
            "headshot.rel",
            "teams",
            "flag.alt",
            "flag.rel",
            "flag.width",
            "flag.height"
          ))
        )

      colnames(players) <- paste0("athlete_", colnames(players))

      players <- players %>%
        dplyr::mutate(
          athlete_id = as.integer(.data$athlete_id),
          team_id = as.integer(team_id)
        ) %>%
        janitor::clean_names()

      conference_info <- team_roster %>%
        purrr::pluck("groups") %>%
        dplyr::as_tibble() %>%
        jsonlite::toJSON() %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        dplyr::rename(dplyr::any_of(c(
          "group_id" = "id",
          "group_is_conference" = "isConference",
          "parent_group_id" = "parent"
        ))) %>%
        dplyr::mutate(
          group_id = as.integer(.data$group_id),
          conference_id = .data$group_id,
          parent_group_id = as.integer(.data$parent_group_id)
        )

      team_roster_full <- team_roster %>%
        purrr::discard_at(c("groups", "athletes")) %>%
        jsonlite::toJSON() %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        data.frame() %>%
        dplyr::slice(1:2) %>%
        dplyr::mutate(
          logo = .data$logos.href[1],
          logo_dark = .data$logos.href[2]
        ) %>%
        dplyr::slice(1) %>%
        dplyr::select(
          -dplyr::any_of(c(
            "logos.href",
            "logos.width",
            "logos.height",
            "logos.alt",
            "logos.rel",
            "logos.lastUpdated"
          ))
        ) %>%
        dplyr::rename(dplyr::any_of(c(
          "team_id" = "id",
          "team_uid" = "uid",
          "team_guid" = "guid",
          "team_slug" = "slug",
          "team_is_active" = "isActive",
          "team_type" = "type",
          "team_display_name" = "displayName",
          "team_short_name" = "shortDisplayName",
          "team_location" = "location",
          "team_name" = "name",
          "team_nickname" = "nickname",
          "team_abbreviation" = "abbreviation",
          "team_color" = "color",
          "team_alternate_color" = "alternateColor"
        ))) %>%
        dplyr::mutate(team_id = as.integer(.data$team_id)) %>%
        dplyr::bind_cols(conference_info) %>%
        dplyr::left_join(players, by = c("team_id" = "team_id")) %>%
        make_baseballr_data(
          "ESPN MLB Team Current Roster Information from ESPN.com",
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no team current roster data available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(team_roster_full)
}


#' **Get ESPN MLB schedule for a specific year**
#'
#' @param season Either numeric or character (YYYYMMDD)
#' @return Returns a tibble with scoreboard data
#'
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |matchup |character |Full game matchup name. |
#'    |matchup_short |character |Short matchup name. |
#'    |season |integer |Season (4-digit year). |
#'    |season_type |integer |ESPN season type (1=pre, 2=regular, 3=postseason, 4=off-season). |
#'    |season_slug |character |Season slug. |
#'    |game_id |integer |Unique ESPN game/event identifier. |
#'    |game_uid |character |ESPN game UID. |
#'    |game_date |Date |Game date (YYYY-MM-DD). |
#'    |attendance |integer |Game attendance. |
#'    |play_by_play_available |logical |TRUE if play-by-play is available. |
#'    |was_suspended |logical |Was suspended. |
#'    |notes |logical |Notes. |
#'    |status_name |character |Game status (e.g. 'STATUS_FINAL'). |
#'    |broadcast_market |character |Broadcast market ('national'/'home'/'away'). |
#'    |broadcast_name |character |Broadcast network name. |
#'    |batting_leader_value |numeric |Team batting: batting leader value. |
#'    |batting_leader_stat |character |Team batting: batting leader stat. |
#'    |batting_leader_name |character |Team batting: batting leader name. |
#'    |batting_leader_shortname |character |Team batting: batting leader shortname. |
#'    |batting_leader_headshot |character |Team batting: batting leader headshot. |
#'    |batting_leader_team_id |character |Team batting: batting leader team id. |
#'    |batting_leader_pos |character |Team batting: batting leader pos. |
#'    |home_run_leader_value |logical |Home run leader value. |
#'    |home_run_leader_stat |logical |Home run leader stat. |
#'    |home_run_leader_name |logical |Home run leader name. |
#'    |home_run_leader_shortname |logical |Home run leader shortname. |
#'    |home_run_leader_headshot |logical |Home run leader headshot. |
#'    |home_run_leader_team_id |logical |Home run leader team id. |
#'    |home_run_leader_pos |logical |Home run leader pos. |
#'    |rbi_leader_value |logical |Rbi leader value. |
#'    |rbi_leader_stat |logical |Rbi leader stat. |
#'    |rbi_leader_name |logical |Rbi leader name. |
#'    |rbi_leader_shortname |logical |Rbi leader shortname. |
#'    |rbi_leader_headshot |logical |Rbi leader headshot. |
#'    |rbi_leader_team_id |logical |Rbi leader team id. |
#'    |rbi_leader_pos |logical |Rbi leader pos. |
#'    |start_date |character |Start date. |
#'    |broadcast |character |Broadcast network. |
#'    |highlights |logical |Highlights. |
#'    |game_date_time |POSIXct |Game start date/time (US/Eastern). |
#'    |home_team_name |character |Home team name. |
#'    |home_team_logo |character |Home team logo. |
#'    |home_team_abb |character |Home team abb. |
#'    |home_team_id |integer |Home team id. |
#'    |home_team_location |character |Home team location. |
#'    |home_team_full_name |character |Home team full name. |
#'    |home_team_color |character |Home team color. |
#'    |home_score |integer |Home team run total after the play. |
#'    |home_win |integer |Home win. |
#'    |home_record |character |Home record. |
#'    |away_team_name |character |Away team name. |
#'    |away_team_logo |character |Away team logo. |
#'    |away_team_abb |character |Away team abb. |
#'    |away_team_id |integer |Away team id. |
#'    |away_team_location |character |Away team location. |
#'    |away_team_full_name |character |Away team full name. |
#'    |away_team_color |character |Away team color. |
#'    |away_score |integer |Away team run total after the play. |
#'    |away_win |integer |Away win. |
#'    |away_record |character |Away record. |
#'
#' @import utils
#' @importFrom dplyr select rename any_of mutate
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest_wider unchop hoist
#' @importFrom lubridate with_tz ymd_hm
#' @import rvest
#' @export
#' @family ESPN MLB Functions
#' @examples
#'
#' # Get schedule from date 2022-11-17 (returns 1000 results, max allowable.)
#' \donttest{
#' try(espn_mlb_scoreboard(season = 20230423))
#' }
espn_mlb_scoreboard <- function(season) {
  .args <- mget(setdiff(names(formals()), "..."))
  max_year <- substr(Sys.Date(), 1, 4)

  if (!(as.integer(substr(season, 1, 4)) > 2001)) {
    message(paste("Error: Season must be between 2001 and", max_year + 1))
  }

  # year > 2000
  season <- as.character(season)

  season_dates <- season

  schedule_api <- sprintf(
    "http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/scoreboard?limit=1000&dates=%s",
    season_dates
  )

  tryCatch(
    expr = {
      res <- .retry_request(schedule_api)

      # Check the result
      check_status(res)

      raw_sched <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON(
          simplifyDataFrame = FALSE,
          simplifyVector = FALSE,
          simplifyMatrix = FALSE
        )

      mlb_data <- raw_sched[["events"]] %>%
        tibble::tibble(data = .data$.) %>%
        tidyr::unnest_wider("data") %>%
        tidyr::unchop("competitions") %>%
        dplyr::select(
          -"id",
          -"uid",
          -"date",
          -"status"
        ) %>%
        tidyr::unnest_wider("competitions") %>%
        dplyr::rename(
          "matchup" = "name",
          "matchup_short" = "shortName",
          "game_id" = "id",
          "game_uid" = "uid",
          "game_date" = "date"
        ) %>%
        tidyr::hoist("status", status_name = list("type", "name")) %>%
        dplyr::select(
          !dplyr::any_of(
            c(
              "timeValid",
              "neutralSite",
              "conferenceCompetition",
              "recent",
              "venue",
              "type"
            )
          )
        ) %>%
        tidyr::unnest_wider("season", names_sep = "_") %>%
        dplyr::rename("season" = "season_year") %>%
        dplyr::select(-dplyr::any_of("status"))

      mlb_data <- mlb_data %>%
        dplyr::mutate(
          game_date_time = lubridate::ymd_hm(substr(
            .data$game_date,
            1,
            nchar(.data$game_date) - 1
          )) %>%
            lubridate::with_tz(tzone = "America/New_York"),
          game_date = as.Date(substr(.data$game_date_time, 1, 10))
        )

      mlb_data <- mlb_data %>%
        tidyr::hoist(
          "competitors",
          homeAway = list(1, "homeAway")
        )
      mlb_data <- mlb_data %>%
        tidyr::hoist(
          "competitors",
          team1_team_name = list(1, "team", "name"),
          team1_team_logo = list(1, "team", "logo"),
          team1_team_abb = list(1, "team", "abbreviation"),
          team1_team_id = list(1, "team", "id"),
          team1_team_location = list(1, "team", "location"),
          team1_team_full = list(1, "team", "displayName"),
          team1_team_color = list(1, "team", "color"),
          team1_score = list(1, "score"),
          team1_win = list(1, "winner"),
          team1_record = list(1, "records", 1, "summary"),
          # away team
          team2_team_name = list(2, "team", "name"),
          team2_team_logo = list(2, "team", "logo"),
          team2_team_abb = list(2, "team", "abbreviation"),
          team2_team_id = list(2, "team", "id"),
          team2_team_location = list(2, "team", "location"),
          team2_team_full = list(2, "team", "displayName"),
          team2_team_color = list(2, "team", "color"),
          team2_score = list(2, "score"),
          team2_win = list(2, "winner"),
          team2_record = list(2, "records", 1, "summary")
        )

      mlb_data <- mlb_data %>%
        dplyr::mutate(
          home_team_name = ifelse(
            .data$homeAway == "home",
            .data$team1_team_name,
            .data$team2_team_name
          ),
          home_team_logo = ifelse(
            .data$homeAway == "home",
            .data$team1_team_logo,
            .data$team2_team_logo
          ),
          home_team_abb = ifelse(
            .data$homeAway == "home",
            .data$team1_team_abb,
            .data$team2_team_abb
          ),
          home_team_id = ifelse(
            .data$homeAway == "home",
            .data$team1_team_id,
            .data$team2_team_id
          ),
          home_team_location = ifelse(
            .data$homeAway == "home",
            .data$team1_team_location,
            .data$team2_team_location
          ),
          home_team_full_name = ifelse(
            .data$homeAway == "home",
            .data$team1_team_full,
            .data$team2_team_full
          ),
          home_team_color = ifelse(
            .data$homeAway == "home",
            .data$team1_team_color,
            .data$team2_team_color
          ),
          home_score = ifelse(
            .data$homeAway == "home",
            .data$team1_score,
            .data$team2_score
          ),
          home_win = ifelse(
            .data$homeAway == "home",
            .data$team1_win,
            .data$team2_win
          ),
          home_record = ifelse(
            .data$homeAway == "home",
            .data$team1_record,
            .data$team2_record
          ),
          away_team_name = ifelse(
            .data$homeAway == "away",
            .data$team1_team_name,
            .data$team2_team_name
          ),
          away_team_logo = ifelse(
            .data$homeAway == "away",
            .data$team1_team_logo,
            .data$team2_team_logo
          ),
          away_team_abb = ifelse(
            .data$homeAway == "away",
            .data$team1_team_abb,
            .data$team2_team_abb
          ),
          away_team_id = ifelse(
            .data$homeAway == "away",
            .data$team1_team_id,
            .data$team2_team_id
          ),
          away_team_location = ifelse(
            .data$homeAway == "away",
            .data$team1_team_location,
            .data$team2_team_location
          ),
          away_team_full_name = ifelse(
            .data$homeAway == "away",
            .data$team1_team_full,
            .data$team2_team_full
          ),
          away_team_color = ifelse(
            .data$homeAway == "away",
            .data$team1_team_color,
            .data$team2_team_color
          ),
          away_score = ifelse(
            .data$homeAway == "away",
            .data$team1_score,
            .data$team2_score
          ),
          away_win = ifelse(
            .data$homeAway == "away",
            .data$team1_win,
            .data$team2_win
          ),
          away_record = ifelse(
            .data$homeAway == "away",
            .data$team1_record,
            .data$team2_record
          )
        )

      mlb_data <- mlb_data %>%
        dplyr::mutate_at(
          c(
            "game_id",
            "home_team_id",
            "home_win",
            "away_team_id",
            "away_win",
            "home_score",
            "away_score"
          ),
          as.integer
        )
      mlb_data <- mlb_data %>%
        dplyr::select(
          -dplyr::any_of(dplyr::starts_with("team1")),
          -dplyr::any_of(dplyr::starts_with("team2")),
          -dplyr::any_of(c("homeAway"))
        )

      if ("leaders" %in% names(mlb_data)) {
        schedule_out <- mlb_data %>%
          tidyr::hoist(
            "leaders",
            # batting-average leader (category 1)
            batting_leader_value = list(1, "leaders", 1, "value"),
            batting_leader_stat = list(1, "leaders", 1, "displayValue"),
            batting_leader_name = list(
              1,
              "leaders",
              1,
              "athlete",
              "displayName"
            ),
            batting_leader_shortname = list(
              1,
              "leaders",
              1,
              "athlete",
              "shortName"
            ),
            batting_leader_headshot = list(
              1,
              "leaders",
              1,
              "athlete",
              "headshot"
            ),
            batting_leader_team_id = list(1, "leaders", 1, "team", "id"),
            batting_leader_pos = list(
              1,
              "leaders",
              1,
              "athlete",
              "position",
              "abbreviation"
            ),
            # home-runs leader (category 2)
            home_run_leader_value = list(2, "leaders", 1, "value"),
            home_run_leader_stat = list(2, "leaders", 1, "displayValue"),
            home_run_leader_name = list(
              2,
              "leaders",
              1,
              "athlete",
              "displayName"
            ),
            home_run_leader_shortname = list(
              2,
              "leaders",
              1,
              "athlete",
              "shortName"
            ),
            home_run_leader_headshot = list(
              2,
              "leaders",
              1,
              "athlete",
              "headshot"
            ),
            home_run_leader_team_id = list(2, "leaders", 1, "team", "id"),
            home_run_leader_pos = list(
              2,
              "leaders",
              1,
              "athlete",
              "position",
              "abbreviation"
            ),
            # RBIs leader (category 3)
            rbi_leader_value = list(3, "leaders", 1, "value"),
            rbi_leader_stat = list(3, "leaders", 1, "displayValue"),
            rbi_leader_name = list(
              3,
              "leaders",
              1,
              "athlete",
              "displayName"
            ),
            rbi_leader_shortname = list(
              3,
              "leaders",
              1,
              "athlete",
              "shortName"
            ),
            rbi_leader_headshot = list(
              3,
              "leaders",
              1,
              "athlete",
              "headshot"
            ),
            rbi_leader_team_id = list(3, "leaders", 1, "team", "id"),
            rbi_leader_pos = list(
              3,
              "leaders",
              1,
              "athlete",
              "position",
              "abbreviation"
            ),
          )

        if (
          "broadcasts" %in%
            names(schedule_out) &&
            !any(is.na(schedule_out[["broadcasts"]]))
        ) {
          schedule_out %>%
            tidyr::hoist(
              "broadcasts",
              broadcast_market = list(1, "market"),
              broadcast_name = list(1, "names", 1)
            ) %>%
            dplyr::select(!where(is.list)) %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              "ESPN MLB Scoreboard Information from ESPN.com",
              Sys.time()
            )
        } else {
          schedule_out %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              "ESPN MLB Scoreboard Information from ESPN.com",
              Sys.time()
            )
        }
      } else {
        if (
          "broadcasts" %in%
            names(mlb_data) &&
            !any(is.na(mlb_data[["broadcasts"]]))
        ) {
          mlb_data %>%
            tidyr::hoist(
              "broadcasts",
              broadcast_market = list(1, "market"),
              broadcast_name = list(1, "names", 1)
            ) %>%
            dplyr::select(!where(is.list)) %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              "ESPN MLB Scoreboard Information from ESPN.com",
              Sys.time()
            )
        } else {
          mlb_data %>%
            dplyr::select(!where(is.list)) %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              "ESPN MLB Scoreboard Information from ESPN.com",
              Sys.time()
            )
        }
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no scoreboard data available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
}

#' **Get ESPN MLB's Standings**
#'
#' @param year Either numeric or character (YYYY)
#' @return A standings data frame
#'
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |team_id |integer |Unique ESPN team identifier. |
#'    |team |character |Team. |
#'    |otlosses |character |Otlosses. |
#'    |otwins |character |Otwins. |
#'    |avgpointsagainst |numeric |Avgpointsagainst. |
#'    |avgpointsfor |numeric |Avgpointsfor. |
#'    |clincher |numeric |Clincher. |
#'    |differential |numeric |Differential. |
#'    |divisionwinpercent |numeric |Divisionwinpercent. |
#'    |gamesbehind |numeric |Gamesbehind. |
#'    |gamesplayed |character |Gamesplayed. |
#'    |leaguewinpercent |numeric |Leaguewinpercent. |
#'    |losses |numeric |Losses. |
#'    |playoffseed |numeric |Playoffseed. |
#'    |pointdifferential |character |Pointdifferential. |
#'    |points |character |Points. |
#'    |pointsagainst |character |Pointsagainst. |
#'    |pointsfor |character |Pointsfor. |
#'    |streak |numeric |Streak. |
#'    |ties |character |Ties. |
#'    |winpercent |numeric |Winpercent. |
#'    |wins |numeric |Wins. |
#'    |divisiongamesbehind |character |Divisiongamesbehind. |
#'    |divisionpercent |character |Divisionpercent. |
#'    |divisiontied |character |Divisiontied. |
#'    |homelosses |character |Homelosses. |
#'    |hometies |character |Hometies. |
#'    |homewins |character |Homewins. |
#'    |magicnumberdivision |character |Magicnumberdivision. |
#'    |magicnumberwildcard |character |Magicnumberwildcard. |
#'    |playoffpercent |character |Playoffpercent. |
#'    |roadlosses |character |Roadlosses. |
#'    |roadties |character |Roadties. |
#'    |roadwins |character |Roadwins. |
#'    |wildcardpercent |character |Wildcardpercent. |
#'    |total |character |Total. |
#'    |home |character |Home. |
#'    |road |character |Road. |
#'    |intradivision |character |Intradivision. |
#'    |intraleague |character |Intraleague. |
#'    |lasttengames |character |Lasttengames. |
#'
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr select rename
#' @importFrom tidyr pivot_wider
#' @importFrom data.table rbindlist
#' @export
#' @keywords MLB Standings
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#' try(espn_mlb_standings(year = 2021))
#' }
espn_mlb_standings <- function(year) {
  .args <- mget(setdiff(names(formals()), "..."))
  standings_url <- "https://site.web.api.espn.com/apis/v2/sports/baseball/mlb/standings?region=us&lang=en&contentorigin=espn&type=0&level=1&sort=winpercent%3Adesc%2Cwins%3Adesc%2Cgamesbehind%3Aasc&"

  ## Inputs
  ## year
  full_url <- paste0(
    standings_url,
    "season=",
    year
  )

  standings <- .empty_baseballr_data("ESPN MLB Standings Information from ESPN.com")

  tryCatch(
    expr = {
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      raw_standings <- jsonlite::fromJSON(resp)[["standings"]]

      # Create a dataframe of all MLB teams by extracting from the raw_standings file

      teams <- raw_standings[["entries"]][["team"]]

      teams <- teams %>%
        dplyr::select("id", "displayName") %>%
        dplyr::rename(
          "team_id" = "id",
          "team" = "displayName"
        )

      # creating a dataframe of the MLB raw standings table from ESPN

      standings_df <- raw_standings[["entries"]][["stats"]]

      standings_data <- data.table::rbindlist(
        standings_df,
        fill = TRUE,
        idcol = T
      )

      # Use the following code to replace NA's in the dataframe with the correct corresponding values and removing all unnecessary columns

      standings_data$value <- ifelse(
        is.na(standings_data$value) & !is.na(standings_data$summary),
        standings_data$summary,
        standings_data$value
      )

      standings_data <- standings_data %>%
        dplyr::select(
          ".id",
          "type",
          "value"
        )

      # Use pivot_wider to transpose the dataframe so that we now have a standings row for each team

      standings_data <- standings_data %>%
        tidyr::pivot_wider(names_from = "type", values_from = "value")

      standings_data <- standings_data %>%
        dplyr::select(-".id")

      # joining the 2 dataframes together to create a standings table

      standings <- cbind(teams, standings_data) %>%
        dplyr::mutate(team_id = as.integer(.data$team_id)) %>%
        dplyr::mutate_at(
          c(
            "avgpointsagainst",
            "avgpointsfor",
            "clincher",
            "differential",
            "divisionwinpercent",
            "gamesbehind",
            "leaguewinpercent",
            "losses",
            "playoffseed",
            "streak",
            "winpercent",
            "wins"
          ),
          as.numeric
        )
      standings <- standings %>%
        make_baseballr_data(
          "ESPN MLB Standings Information from ESPN.com",
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no standings data available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(standings)
}


#' **Get ESPN MLB's Betting information**
#'
#' @param game_id  Game ID
#' @returns Returns a named list of data frames: pickcenter, againstTheSpread, predictor
#'
#'    **pickcenter**
#'
#'
#'    |col_name                             |types     |description                                  |
#'    |:------------------------------------|:---------|:--------------------------------------------|
#'    |details                              |character |Details.                                     |
#'    |over_under                           |numeric   |Over under.                                  |
#'    |spread                               |numeric   |Spread.                                      |
#'    |provider_id                          |integer   |Unique identifier for provider.              |
#'    |provider_name                        |character |Provider name.                               |
#'    |provider_priority                    |integer   |Provider priority.                           |
#'    |away_team_odds_favorite              |logical   |Away team's team odds favorite.              |
#'    |away_team_odds_underdog              |logical   |Away team's team odds underdog.              |
#'    |away_team_odds_money_line            |integer   |Away team's team odds money line.            |
#'    |away_team_odds_spread_odds           |numeric   |Away team's team odds spread odds.           |
#'    |away_team_odds_team_id               |integer   |Unique identifier for away team odds team.   |
#'    |away_team_odds_win_percentage        |numeric   |Away team odds win percentage (0-1 decimal). |
#'    |away_team_odds_average_score         |numeric   |Away team's team odds average score.         |
#'    |away_team_odds_money_line_odds       |numeric   |Away team's team odds money line odds.       |
#'    |away_team_odds_spread_return         |numeric   |Away team's team odds spread return.         |
#'    |away_team_odds_spread_record_wins    |integer   |Away team's team odds spread record wins.    |
#'    |away_team_odds_spread_record_losses  |integer   |Away team's team odds spread record losses.  |
#'    |away_team_odds_spread_record_pushes  |integer   |Away team's team odds spread record pushes.  |
#'    |away_team_odds_spread_record_summary |character |Away team's team odds spread record summary. |
#'    |home_team_odds_favorite              |logical   |Home team's team odds favorite.              |
#'    |home_team_odds_underdog              |logical   |Home team's team odds underdog.              |
#'    |home_team_odds_money_line            |integer   |Home team's team odds money line.            |
#'    |home_team_odds_spread_odds           |numeric   |Home team's team odds spread odds.           |
#'    |home_team_odds_team_id               |integer   |Unique identifier for home team odds team.   |
#'    |home_team_odds_win_percentage        |numeric   |Home team odds win percentage (0-1 decimal). |
#'    |home_team_odds_average_score         |numeric   |Home team's team odds average score.         |
#'    |home_team_odds_money_line_odds       |numeric   |Home team's team odds money line odds.       |
#'    |home_team_odds_spread_return         |numeric   |Home team's team odds spread return.         |
#'    |home_team_odds_spread_record_wins    |integer   |Home team's team odds spread record wins.    |
#'    |home_team_odds_spread_record_losses  |integer   |Home team's team odds spread record losses.  |
#'    |home_team_odds_spread_record_pushes  |integer   |Home team's team odds spread record pushes.  |
#'    |home_team_odds_spread_record_summary |character |Home team's team odds spread record summary. |
#'    |game_id                              |integer   |Unique game identifier.                      |
#'
#'    **againstTheSpread**
#'
#'
#'    |col_name     |types     |description                             |
#'    |:------------|:---------|:---------------------------------------|
#'    |id           |integer   |Id.                                     |
#'    |uid          |character |ESPN UID string (universal identifier). |
#'    |display_name |character |Display name.                           |
#'    |abbreviation |character |Short abbreviation.                     |
#'    |logo         |character |Team or league logo URL.                |
#'    |logos        |list      |Logos.                                  |
#'    |records      |list      |Records.                                |
#'    |game_id      |integer   |Unique game identifier.                 |
#'    |team_id      |integer   |Unique team identifier.                 |
#'
#'    **predictor**
#'
#'
#'    |col_name                  |types   |description                          |
#'    |:-------------------------|:-------|:------------------------------------|
#'    |game_id                   |integer |Unique game identifier.              |
#'    |home_team_id              |integer |Unique identifier for the home team. |
#'    |away_team_id              |integer |Unique identifier for the away team. |
#'    |away_team_game_projection |numeric |Away team's team game projection.    |
#'    |away_team_chance_loss     |numeric |Away team's team chance loss.        |
#'
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr select rename
#' @export
#' @keywords MLB Betting
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#' try(espn_mlb_betting(game_id = 401283399))
#' }
espn_mlb_betting <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  summary_url <-
    "http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?"

  ## Inputs
  ## game_id
  full_url <- paste0(
    summary_url,
    "event=",
    game_id
  )

  pickcenter <- data.frame()
  againstTheSpread <- data.frame()
  predictor_df <- data.frame()

  betting <- NULL

  tryCatch(
    expr = {
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      raw_summary <- jsonlite::fromJSON(resp)
      if ("pickcenter" %in% names(raw_summary)) {
        pickcenter <- jsonlite::fromJSON(
          jsonlite::toJSON(raw_summary$pickcenter),
          flatten = TRUE
        ) %>%
          janitor::clean_names() %>%
          dplyr::select(-dplyr::any_of("links")) %>%
          dplyr::mutate(game_id = as.integer(game_id)) %>%
          dplyr::mutate(dplyr::across(dplyr::any_of(c(
            "provider_id",
            "away_team_odds_team_id",
            "home_team_odds_team_id"
          )), as.integer)) %>%
          make_baseballr_data(
            "ESPN MLB Pickcenter Information from ESPN.com",
            Sys.time()
          )
      }
      if ("againstTheSpread" %in% names(raw_summary)) {
        againstTheSpread <- jsonlite::fromJSON(jsonlite::toJSON(
          raw_summary$againstTheSpread
        )) %>%
          janitor::clean_names()
        teams <- againstTheSpread$team %>%
          dplyr::select(-dplyr::any_of("links")) %>%
          janitor::clean_names()
        records <- againstTheSpread$records

        teams$records <- records
        againstTheSpread <- teams %>%
          dplyr::mutate(
            game_id = as.integer(game_id),
            id = as.integer(.data$id),
            team_id = as.integer(.data$id)
          ) %>%
          make_baseballr_data(
            "ESPN MLB Against the Spread Information from ESPN.com",
            Sys.time()
          )
      }
      if ("predictor" %in% names(raw_summary)) {
        predictor_df <- data.frame(
          game_id = as.integer(game_id),
          home_team_id = as.integer(raw_summary$predictor$homeTeam$id),
          away_team_id = as.integer(raw_summary$predictor$awayTeam$id),
          away_team_game_projection = as.numeric(
            raw_summary$predictor$awayTeam$gameProjection
          ),
          away_team_chance_loss = as.numeric(
            raw_summary$predictor$awayTeam$teamChanceLoss
          )
        )
        predictor_df <- predictor_df %>%
          make_baseballr_data(
            "ESPN MLB Predictor Information from ESPN.com",
            Sys.time()
          )
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no betting data available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  betting <- c(list(pickcenter), list(againstTheSpread), list(predictor_df))
  names(betting) <- c("pickcenter", "againstTheSpread", "predictor")
  return(betting)
}


#' @title
#' **Get ESPN MLB team stats data**
#' @author Saiem Gilani
#' @param team_id Team ID
#' @param year Year
#' @param season_type (character, default: regular): Season type - regular or postseason
#' @param total (boolean, default: FALSE): Totals
#' @return Returns a tibble with the team stats data
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |team_id |integer |Unique ESPN team identifier. |
#'    |team_guid |character |ESPN team GUID. |
#'    |team_uid |character |ESPN universal team identifier (UID). |
#'    |team_sdr |character |Team sdr. |
#'    |team_slug |character |URL-safe team identifier. |
#'    |team_location |character |Team city / location. |
#'    |team_name |character |Team nickname (e.g. 'Yankees'). |
#'    |team_abbreviation |character |Short team abbreviation (e.g. 'NYY'). |
#'    |team_display_name |character |Full team display name (e.g. 'New York Yankees'). |
#'    |team_short_display_name |character |Short team display name. |
#'    |team_color |character |Team primary color (hex, no leading '#'). |
#'    |team_alternate_color |character |Team alternate color (hex). |
#'    |team_is_active |logical |Team is active. |
#'    |is_all_star |logical |Is all star. |
#'    |logo_href |character |Logo href. |
#'    |logo_dark_href |character |Logo dark href. |
#'    |logos_href_2 |character |Logos href 2. |
#'    |logos_href_3 |character |Logos href 3. |
#'    |logos_href_4 |character |Logos href 4. |
#'    |logos_width_4 |integer |Logos width 4. |
#'    |logos_height_4 |integer |Logos height 4. |
#'    |logos_alt_4 |character |Logos alt 4. |
#'    |logos_rel_full_4 |character |Logos rel full 4. |
#'    |logos_rel_primary_logo_on_white_color |character |Logos rel primary logo on white color. |
#'    |logos_last_updated_4 |character |Logos last updated 4. |
#'    |logos_href_5 |character |Logos href 5. |
#'    |logos_width_5 |integer |Logos width 5. |
#'    |logos_height_5 |integer |Logos height 5. |
#'    |logos_alt_5 |character |Logos alt 5. |
#'    |logos_rel_full_5 |character |Logos rel full 5. |
#'    |logos_rel_primary_logo_on_black_color |character |Logos rel primary logo on black color. |
#'    |logos_last_updated_5 |character |Logos last updated 5. |
#'    |logos_href_6 |character |Logos href 6. |
#'    |logos_width_6 |integer |Logos width 6. |
#'    |logos_height_6 |integer |Logos height 6. |
#'    |logos_alt_6 |character |Logos alt 6. |
#'    |logos_rel_full_6 |character |Logos rel full 6. |
#'    |logos_rel_primary_logo_on_primary_color |character |Logos rel primary logo on primary color. |
#'    |logos_last_updated_6 |character |Logos last updated 6. |
#'    |logos_href_7 |character |Logos href 7. |
#'    |logos_width_7 |integer |Logos width 7. |
#'    |logos_height_7 |integer |Logos height 7. |
#'    |logos_alt_7 |character |Logos alt 7. |
#'    |logos_rel_full_7 |character |Logos rel full 7. |
#'    |logos_rel_primary_logo_on_secondary_color |character |Logos rel primary logo on secondary color. |
#'    |logos_last_updated_7 |character |Logos last updated 7. |
#'    |logos_href_8 |character |Logos href 8. |
#'    |logos_width_8 |integer |Logos width 8. |
#'    |logos_height_8 |integer |Logos height 8. |
#'    |logos_alt_8 |character |Logos alt 8. |
#'    |logos_rel_full_8 |character |Logos rel full 8. |
#'    |logos_rel_primary_logo_black |character |Logos rel primary logo black. |
#'    |logos_last_updated_8 |character |Logos last updated 8. |
#'    |logos_href_9 |character |Logos href 9. |
#'    |logos_width_9 |integer |Logos width 9. |
#'    |logos_height_9 |integer |Logos height 9. |
#'    |logos_alt_9 |character |Logos alt 9. |
#'    |logos_rel_full_9 |character |Logos rel full 9. |
#'    |logos_rel_primary_logo_white |character |Logos rel primary logo white. |
#'    |logos_last_updated_9 |character |Logos last updated 9. |
#'    ...and 212 further ESPN stat columns (full batting / pitching / fielding stat set).
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @export
#' @keywords MLB Team Stats
#' @family ESPN MLB Functions
#'
#' @examples
#' \donttest{
#' try(espn_mlb_team_stats(team_id = 13, year = 2020))
#' }
espn_mlb_team_stats <- function(
    team_id,
    year,
    season_type = "regular",
    total = FALSE) {
  .args <- mget(setdiff(names(formals()), "..."))
  if (!(tolower(season_type) %in% c("regular", "postseason"))) {
    # Check if season_type is appropriate, if not regular
    cli::cli_abort("Enter valid season_type: regular or postseason")
  }
  s_type <- ifelse(season_type == "postseason", 3, 2)

  base_url <- "https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/seasons/"

  totals <- ifelse(total == TRUE, 0, "")
  full_url <- paste0(
    base_url,
    year,
    "/types/",
    s_type,
    "/teams/",
    team_id,
    "/statistics/",
    totals
  )

  df <- data.frame()
  tryCatch(
    expr = {
      # Create the GET request and set response as res
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)

      # Get the content and return result as data.frame
      df <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON(
          simplifyDataFrame = FALSE,
          simplifyVector = FALSE,
          simplifyMatrix = FALSE
        )

      team_url <- df[["team"]][["$ref"]]

      # Create the GET request and set response as res
      team_res <- .retry_request(team_url)

      # Check the result
      check_status(team_res)

      team_df <- team_res %>%
        .resp_text() %>%
        jsonlite::fromJSON(
          simplifyDataFrame = FALSE,
          simplifyVector = FALSE,
          simplifyMatrix = FALSE
        )

      team_df[["links"]] <- NULL
      team_df[["injuries"]] <- NULL
      team_df[["record"]] <- NULL
      team_df[["athletes"]] <- NULL
      team_df[["venue"]] <- NULL
      team_df[["groups"]] <- NULL
      team_df[["ranks"]] <- NULL
      team_df[["statistics"]] <- NULL
      team_df[["leaders"]] <- NULL
      team_df[["links"]] <- NULL
      team_df[["notes"]] <- NULL
      team_df[["franchise"]] <- NULL
      team_df[["againstTheSpreadRecords"]] <- NULL
      team_df[["oddsRecords"]] <- NULL
      team_df[["college"]] <- NULL
      team_df[["transactions"]] <- NULL
      team_df[["leaders"]] <- NULL
      team_df[["depthCharts"]] <- NULL
      team_df[["awards"]] <- NULL
      team_df[["events"]] <- NULL

      team_df <- team_df %>%
        purrr::map_if(is.list, as.data.frame) %>%
        as.data.frame() %>%
        dplyr::select(
          -dplyr::any_of(
            c(
              "logos.width",
              "logos.height",
              "logos.alt",
              "logos.rel..full.",
              "logos.rel..default.",
              "logos.rel..scoreboard.",
              "logos.rel..scoreboard..1",
              "logos.rel..scoreboard.2",
              "logos.lastUpdated",
              "logos.width.1",
              "logos.height.1",
              "logos.alt.1",
              "logos.rel..full..1",
              "logos.rel..dark.",
              "logos.rel..dark..1",
              "logos.lastUpdated.1",
              "logos.width.2",
              "logos.height.2",
              "logos.alt.2",
              "logos.rel..full..2",
              "logos.rel..scoreboard.",
              "logos.lastUpdated.2",
              "logos.width.3",
              "logos.height.3",
              "logos.alt.3",
              "logos.rel..full..3",
              "logos.lastUpdated.3",
              "X.ref",
              "X.ref.1",
              "X.ref.2"
            )
          )
        ) %>%
        janitor::clean_names()

      colnames(team_df)[1:13] <- paste0("team_", colnames(team_df)[1:13])

      team_df <- team_df %>%
        dplyr::rename(
          "logo_href" = "logos_href",
          "logo_dark_href" = "logos_href_1"
        )

      df <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON() %>%
        purrr::pluck("splits") %>%
        purrr::pluck("categories") %>%
        tidyr::unnest("stats", names_sep = "_")
      df <- df %>%
        dplyr::mutate(
          stats_category_name = paste0(.data$name, "_", .data$stats_name)
        ) %>%
        dplyr::select(
          "stats_category_name",
          "stats_value"
        ) %>%
        tidyr::pivot_wider(
          names_from = "stats_category_name",
          values_from = "stats_value",
          values_fn = dplyr::first
        ) %>%
        janitor::clean_names()

      df <- team_df %>%
        dplyr::bind_cols(df)

      df <- df %>%
        dplyr::mutate_at(
          c(
            "team_id"
          ),
          as.integer
        ) %>%
        make_baseballr_data("ESPN MLB Team Season Stats from ESPN.com", Sys.time())
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no team season stats data available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(df)
}

#' @title
#' **Get ESPN MLB player stats data**
#' @author Saiem Gilani
#' @param athlete_id Athlete ID
#' @param year Year
#' @param season_type (character, default: regular): Season type - regular or postseason
#' @param total (boolean, default: FALSE): Totals
#' @return Returns a tibble with the player stats data
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |athlete_id |integer |Unique ESPN athlete identifier. |
#'    |athlete_uid |character |Athlete uid. |
#'    |athlete_guid |character |Athlete guid. |
#'    |athlete_type |character |Athlete type. |
#'    |sdr |integer |Sdr. |
#'    |first_name |character |First name. |
#'    |last_name |character |Last name. |
#'    |full_name |character |Full name. |
#'    |display_name |character |Display name. |
#'    |nickname |character |Team nickname. |
#'    |short_name |character |Short display name. |
#'    |weight |numeric |Weight. |
#'    |display_weight |character |Display weight. |
#'    |height |numeric |Height. |
#'    |display_height |character |Display height. |
#'    |age |integer |Age. |
#'    |date_of_birth |character |Date of birth. |
#'    |debut_year |integer |Debut year. |
#'    |slug |character |Slug. |
#'    |headshot_href |character |Headshot href. |
#'    |headshot_alt |character |Headshot alt. |
#'    |jersey |character |Jersey. |
#'    |position_id |integer |Position id. |
#'    |position_name |character |Position name. |
#'    |position_display_name |character |Position display name. |
#'    |position_abbreviation |character |Position abbreviation. |
#'    |position_leaf |logical |Position leaf. |
#'    |positions_x_ref |character |Positions x ref. |
#'    |positions_id |character |Positions id. |
#'    |positions_name |character |Positions name. |
#'    |positions_display_name |character |Positions display name. |
#'    |positions_abbreviation |character |Positions abbreviation. |
#'    |positions_leaf |logical |Positions leaf. |
#'    |positions_x_ref_1 |character |Positions x ref 1. |
#'    |positions_x_ref_2 |character |Positions x ref 2. |
#'    |positions_x_ref_3 |character |Positions x ref 3. |
#'    |positions_id_1 |character |Positions id 1. |
#'    |positions_name_1 |character |Positions name 1. |
#'    |positions_display_name_1 |character |Positions display name 1. |
#'    |positions_abbreviation_1 |character |Positions abbreviation 1. |
#'    |positions_leaf_1 |logical |Positions leaf 1. |
#'    |positions_x_ref_4 |character |Positions x ref 4. |
#'    |positions_x_ref_5 |character |Positions x ref 5. |
#'    |positions_x_ref_6 |character |Positions x ref 6. |
#'    |positions_id_2 |character |Positions id 2. |
#'    |positions_name_2 |character |Positions name 2. |
#'    |positions_display_name_2 |character |Positions display name 2. |
#'    |positions_abbreviation_2 |character |Positions abbreviation 2. |
#'    |positions_leaf_2 |logical |Positions leaf 2. |
#'    |positions_x_ref_7 |character |Positions x ref 7. |
#'    |positions_x_ref_8 |character |Positions x ref 8. |
#'    |positions_x_ref_9 |character |Positions x ref 9. |
#'    |positions_id_3 |character |Positions id 3. |
#'    |positions_name_3 |character |Positions name 3. |
#'    |positions_display_name_3 |character |Positions display name 3. |
#'    |positions_abbreviation_3 |character |Positions abbreviation 3. |
#'    |positions_leaf_3 |logical |Positions leaf 3. |
#'    |positions_x_ref_10 |character |Positions x ref 10. |
#'    |positions_x_ref_11 |character |Positions x ref 11. |
#'    |linked |logical |Linked. |
#'    ...and 216 further ESPN stat columns (full batting / pitching / fielding stat set).
#'
#' @export
#' @keywords MLB Player Stats
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#' try(espn_mlb_player_stats(athlete_id = 33192, year = 2022))
#' }
espn_mlb_player_stats <- function(
    athlete_id,
    year,
    season_type = "regular",
    total = FALSE) {
  .args <- mget(setdiff(names(formals()), "..."))
  if (!(tolower(season_type) %in% c("regular", "postseason"))) {
    # Check if season_type is appropriate, if not regular
    cli::cli_abort("Enter valid season_type: regular or postseason")
  }
  s_type <- ifelse(season_type == "postseason", 3, 2)

  base_url <- "https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/seasons/"

  totals <- ifelse(total == TRUE, 0, "")
  full_url <- paste0(
    base_url,
    year,
    "/types/",
    s_type,
    "/athletes/",
    athlete_id,
    "/statistics/",
    totals
  )
  athlete_url <- paste0(
    base_url,
    year,
    "/athletes/",
    athlete_id
  )
  df <- data.frame()
  tryCatch(
    expr = {
      # Create the GET request and set response as res
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)
      # Create the GET request and set response as res
      athlete_res <- .retry_request(athlete_url)

      # Check the result
      check_status(athlete_res)

      athlete_df <- athlete_res %>%
        .resp_text() %>%
        jsonlite::fromJSON(
          simplifyDataFrame = FALSE,
          simplifyVector = FALSE,
          simplifyMatrix = FALSE
        )

      team_url <- athlete_df[["team"]][["$ref"]]

      # Create the GET request and set response as res
      team_res <- .retry_request(team_url)

      # Check the result
      check_status(team_res)

      team_df <- team_res %>%
        .resp_text() %>%
        jsonlite::fromJSON(
          simplifyDataFrame = FALSE,
          simplifyVector = FALSE,
          simplifyMatrix = FALSE
        )

      team_df[["links"]] <- NULL
      team_df[["injuries"]] <- NULL
      team_df[["record"]] <- NULL
      team_df[["athletes"]] <- NULL
      team_df[["venue"]] <- NULL
      team_df[["groups"]] <- NULL
      team_df[["ranks"]] <- NULL
      team_df[["statistics"]] <- NULL
      team_df[["leaders"]] <- NULL
      team_df[["links"]] <- NULL
      team_df[["notes"]] <- NULL
      team_df[["franchise"]] <- NULL
      team_df[["againstTheSpreadRecords"]] <- NULL
      team_df[["oddsRecords"]] <- NULL
      team_df[["college"]] <- NULL
      team_df[["transactions"]] <- NULL
      team_df[["leaders"]] <- NULL
      team_df[["depthCharts"]] <- NULL
      team_df[["awards"]] <- NULL
      team_df[["events"]] <- NULL

      team_df <- team_df %>%
        purrr::map_if(is.list, as.data.frame) %>%
        as.data.frame() %>%
        dplyr::select(
          -dplyr::any_of(
            c(
              "logos.width",
              "logos.height",
              "logos.alt",
              "logos.rel..full.",
              "logos.rel..default.",
              "logos.rel..scoreboard.",
              "logos.rel..scoreboard..1",
              "logos.rel..scoreboard.2",
              "logos.lastUpdated",
              "logos.width.1",
              "logos.height.1",
              "logos.alt.1",
              "logos.rel..full..1",
              "logos.rel..dark.",
              "logos.rel..dark..1",
              "logos.lastUpdated.1",
              "logos.width.2",
              "logos.height.2",
              "logos.alt.2",
              "logos.rel..full..2",
              "logos.rel..scoreboard.",
              "logos.lastUpdated.2",
              "logos.width.3",
              "logos.height.3",
              "logos.alt.3",
              "logos.rel..full..3",
              "logos.lastUpdated.3",
              "X.ref",
              "X.ref.1",
              "X.ref.2"
            )
          )
        ) %>%
        janitor::clean_names()
      colnames(team_df)[1:13] <- paste0("team_", colnames(team_df)[1:13])

      team_df <- team_df %>%
        dplyr::rename(
          "logo_href" = "logos_href",
          "logo_dark_href" = "logos_href_1"
        )

      athlete_df[["links"]] <- NULL
      athlete_df[["injuries"]] <- NULL
      athlete_df[["birthPlace"]] <- NULL

      athlete_df <- athlete_df %>%
        purrr::map_if(is.list, as.data.frame) %>%
        tibble::tibble(data = .data$.)
      athlete_df <- athlete_df$data %>%
        as.data.frame() %>%
        dplyr::select(
          -dplyr::any_of(c(
            "X.ref",
            "X.ref.1",
            "X.ref.2",
            "X.ref.3",
            "X.ref.4",
            "X.ref.5",
            "X.ref.6",
            "X.ref.7",
            "X.ref.8",
            "position.X.ref",
            "position.X.ref.1",
            "contract.x.ref",
            "contract.x.ref.1",
            "contract.x.ref.2",
            "draft.x.ref",
            "draft.x.ref.1"
          ))
        ) %>%
        janitor::clean_names() %>%
        dplyr::rename(
          "athlete_id" = "id",
          "athlete_uid" = "uid",
          "athlete_guid" = "guid",
          "athlete_type" = "type"
        )

      # Get the content and return result as data.frame
      df <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON() %>%
        purrr::pluck("splits") %>%
        purrr::pluck("categories") %>%
        tidyr::unnest("stats", names_sep = "_")
      df <- df %>%
        dplyr::mutate(
          stats_category_name = paste0(.data$name, "_", .data$stats_name)
        ) %>%
        dplyr::select("stats_category_name", "stats_value") %>%
        tidyr::pivot_wider(
          names_from = "stats_category_name",
          values_from = "stats_value",
          values_fn = dplyr::first
        ) %>%
        janitor::clean_names()
      df <- athlete_df %>%
        dplyr::bind_cols(df) %>%
        dplyr::bind_cols(team_df)
      df <- df %>%
        dplyr::mutate_at(
          c(
            "athlete_id",
            "team_id",
            "position_id",
            "status_id",
            "sdr"
          ),
          as.integer
        ) %>%
        make_baseballr_data(
          "ESPN MLB Player Season Stats from ESPN.com",
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no player season stats data available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(df)
}

#'  **Parse ESPN MLB PBP, helper function**
#' @param resp Response object from the ESPN MLB game summary endpoint
#' @return Returns a tibble
#' @importFrom lubridate with_tz ymd_hm
#' @keywords internal
#' @export
helper_espn_mlb_pbp <- function(resp) {
  game_json <- resp %>%
    jsonlite::fromJSON()

  pbp_source <- game_json[["header"]][["competitions"]][["playByPlaySource"]]

  plays <- game_json %>%
    purrr::pluck("plays") %>%
    dplyr::as_tibble()

  if (pbp_source != "none" && nrow(plays) > 10) {
    homeAway1 <- jsonlite::fromJSON(resp)[["header"]][["competitions"]][[
      "competitors"
    ]][[1]][["homeAway"]][1]

    gameId <- as.integer(game_json[["header"]][["id"]])
    season <- game_json[["header"]][["season"]][["year"]]
    season_type <- game_json[["header"]][["season"]][["type"]]
    game_date_time <- substr(
      game_json[["header"]][["competitions"]][["date"]],
      1,
      nchar(game_json[["header"]][["competitions"]][["date"]]) - 1
    ) %>%
      lubridate::ymd_hm() %>%
      lubridate::with_tz(tzone = "America/New_York")

    game_date <- as.Date(substr(game_date_time, 0, 10))

    id_vars <- data.frame()
    if (homeAway1 == "home") {
      homeTeamId <- as.integer(
        game_json[["header"]][["competitions"]][["competitors"]][[1]][[
          "team"
        ]][["id"]] %>%
          purrr::pluck(1, .default = NA_integer_)
      )
      homeTeamMascot <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["name"]] %>%
        purrr::pluck(1, .default = NA_character_)
      homeTeamName <- game_json[["header"]][["competitions"]][["competitors"]][[
        1
      ]][["team"]][["location"]] %>%
        purrr::pluck(1, .default = NA_character_)
      homeTeamAbbrev <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["abbreviation"]] %>%
        purrr::pluck(1, .default = NA_character_)
      homeTeamLogo <- game_json[["header"]][["competitions"]][["competitors"]][[
        1
      ]][["team"]][["logos"]][[1]][["href"]] %>%
        purrr::pluck(1, .default = NA_character_)
      homeTeamLogoDark <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["logos"]][[1]][["href"]] %>%
        purrr::pluck(2, .default = NA_character_)
      homeTeamFullName <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["displayName"]] %>%
        purrr::pluck(1, .default = NA_character_)
      homeTeamColor <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["color"]] %>%
        purrr::pluck(1, .default = NA_character_)
      homeTeamAlternateColor <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["alternateColor"]] %>%
        purrr::pluck(1, .default = NA_character_)
      homeTeamScore <- as.integer(
        game_json[["header"]][["competitions"]][["competitors"]][[1]][[
          "score"
        ]] %>%
          purrr::pluck(1, .default = NA_character_)
      )
      homeTeamWinner <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["winner"]] %>%
        purrr::pluck(1, .default = NA_character_)
      homeTeamRecord <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["record"]][[1]][["summary"]] %>%
        purrr::pluck(1, .default = NA_character_)
      awayTeamId <- as.integer(
        game_json[["header"]][["competitions"]][["competitors"]][[1]][[
          "team"
        ]][["id"]] %>%
          purrr::pluck(2, .default = NA_integer_)
      )
      awayTeamMascot <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["name"]] %>%
        purrr::pluck(2, .default = NA_character_)
      awayTeamName <- game_json[["header"]][["competitions"]][["competitors"]][[
        1
      ]][["team"]][["location"]] %>%
        purrr::pluck(2, .default = NA_character_)
      awayTeamAbbrev <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["abbreviation"]] %>%
        purrr::pluck(2, .default = NA_character_)
      awayTeamLogo <- game_json[["header"]][["competitions"]][["competitors"]][[
        1
      ]][["team"]][["logos"]][[2]][["href"]] %>%
        purrr::pluck(1, .default = NA_character_)
      awayTeamLogoDark <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["logos"]][[2]][["href"]] %>%
        purrr::pluck(2, .default = NA_character_)
      awayTeamFullName <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["displayName"]] %>%
        purrr::pluck(2, .default = NA_character_)
      awayTeamColor <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["color"]] %>%
        purrr::pluck(2, .default = NA_character_)
      awayTeamAlternateColor <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["alternateColor"]] %>%
        purrr::pluck(2, .default = NA_character_)
      awayTeamScore <- as.integer(
        game_json[["header"]][["competitions"]][["competitors"]][[1]][[
          "score"
        ]] %>%
          purrr::pluck(2, .default = NA_integer_)
      )
      awayTeamWinner <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["winner"]] %>%
        purrr::pluck(2, .default = NA_character_)
      awayTeamRecord <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["record"]][[1]][["summary"]] %>%
        purrr::pluck(2, .default = NA_character_)
      id_vars <- data.frame(
        homeTeamId,
        homeTeamMascot,
        homeTeamName,
        homeTeamAbbrev,
        homeTeamLogo,
        homeTeamLogoDark,
        homeTeamFullName,
        homeTeamColor,
        homeTeamAlternateColor,
        homeTeamScore,
        homeTeamWinner,
        homeTeamRecord,
        awayTeamId,
        awayTeamMascot,
        awayTeamName,
        awayTeamAbbrev,
        awayTeamLogo,
        awayTeamLogoDark,
        awayTeamFullName,
        awayTeamColor,
        awayTeamAlternateColor,
        awayTeamScore,
        awayTeamWinner,
        awayTeamRecord
      )
    } else {
      awayTeamId <- as.integer(
        game_json[["header"]][["competitions"]][["competitors"]][[1]][[
          "team"
        ]][["id"]] %>%
          purrr::pluck(1, .default = NA_integer_)
      )
      awayTeamMascot <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["name"]] %>%
        purrr::pluck(1, .default = NA_character_)
      awayTeamName <- game_json[["header"]][["competitions"]][["competitors"]][[
        1
      ]][["team"]][["location"]] %>%
        purrr::pluck(1, .default = NA_character_)
      awayTeamAbbrev <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["abbreviation"]] %>%
        purrr::pluck(1, .default = NA_character_)
      awayTeamLogo <- game_json[["header"]][["competitions"]][["competitors"]][[
        1
      ]][["team"]][["logos"]][[1]][["href"]] %>%
        purrr::pluck(1, .default = NA_character_)
      awayTeamLogoDark <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["logos"]][[1]][["href"]] %>%
        purrr::pluck(2, .default = NA_character_)
      awayTeamFullName <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["displayName"]] %>%
        purrr::pluck(1, .default = NA_character_)
      awayTeamColor <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["color"]] %>%
        purrr::pluck(1, .default = NA_character_)
      awayTeamAlternateColor <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["alternateColor"]] %>%
        purrr::pluck(1, .default = NA_character_)
      awayTeamScore <- as.integer(
        game_json[["header"]][["competitions"]][["competitors"]][[1]][[
          "score"
        ]] %>%
          purrr::pluck(1, .default = NA_integer_)
      )
      awayTeamWinner <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["winner"]] %>%
        purrr::pluck(1, .default = NA_character_)
      awayTeamRecord <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["record"]][[1]][["summary"]] %>%
        purrr::pluck(1, .default = NA_character_)
      homeTeamId <- as.integer(
        game_json[["header"]][["competitions"]][["competitors"]][[1]][[
          "team"
        ]][["id"]] %>%
          purrr::pluck(2, .default = NA_integer_)
      )
      homeTeamMascot <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["name"]] %>%
        purrr::pluck(2, .default = NA_character_)
      homeTeamName <- game_json[["header"]][["competitions"]][["competitors"]][[
        1
      ]][["team"]][["location"]] %>%
        purrr::pluck(2, .default = NA_character_)
      homeTeamAbbrev <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["abbreviation"]] %>%
        purrr::pluck(2, .default = NA_character_)
      homeTeamLogo <- game_json[["header"]][["competitions"]][["competitors"]][[
        1
      ]][["team"]][["logos"]][[2]][["href"]] %>%
        purrr::pluck(2, .default = NA_character_)
      homeTeamLogoDark <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["logos"]][[2]][["href"]] %>%
        purrr::pluck(2, .default = NA_character_)
      homeTeamFullName <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["displayName"]] %>%
        purrr::pluck(2, .default = NA_character_)
      homeTeamColor <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["color"]] %>%
        purrr::pluck(2, .default = NA_character_)
      homeTeamAlternateColor <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["team"]][["alternateColor"]] %>%
        purrr::pluck(2, .default = NA_character_)
      homeTeamScore <- as.integer(
        game_json[["header"]][["competitions"]][["competitors"]][[1]][[
          "score"
        ]] %>%
          purrr::pluck(2, .default = NA_integer_)
      )
      homeTeamWinner <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["winner"]] %>%
        purrr::pluck(2, .default = NA_character_)
      homeTeamRecord <- game_json[["header"]][["competitions"]][[
        "competitors"
      ]][[1]][["record"]][[1]][["summary"]] %>%
        purrr::pluck(2, .default = NA_character_)
      id_vars <- data.frame(
        homeTeamId,
        homeTeamMascot,
        homeTeamName,
        homeTeamAbbrev,
        homeTeamLogo,
        homeTeamLogoDark,
        homeTeamFullName,
        homeTeamColor,
        homeTeamAlternateColor,
        homeTeamScore,
        homeTeamWinner,
        homeTeamRecord,
        awayTeamId,
        awayTeamMascot,
        awayTeamName,
        awayTeamAbbrev,
        awayTeamLogo,
        awayTeamLogoDark,
        awayTeamFullName,
        awayTeamColor,
        awayTeamAlternateColor,
        awayTeamScore,
        awayTeamWinner,
        awayTeamRecord
      )
    }

    game_json <- game_json %>%
      jsonlite::toJSON() %>%
      jsonlite::fromJSON(flatten = TRUE)

    plays <- game_json %>%
      purrr::pluck("plays")

    # (No court-coordinate transform: ESPN baseball play-by-play carries
    # pitch and batted-ball locations as pitchCoordinate / hitCoordinate,
    # which are flattened to pitch_coordinate_x/y and hit_coordinate_x/y
    # below -- there is no single top-level shot coordinate to transform.)

    ## Written this way for compliance with data repository processing
    if ("participants" %in% names(plays)) {
      plays <- plays %>%
        tidyr::unnest_wider("participants")
      suppressWarnings(
        aths <- plays %>%
          dplyr::group_by(.data$id) %>%
          dplyr::select(
            "id",
            "athlete.id"
          ) %>%
          tidyr::unnest_wider("athlete.id", names_sep = "_")
      )
      names(aths) <- c(
        "play.id",
        "athlete.id.1",
        "athlete.id.2",
        "athlete.id.3"
      )
      plays <- plays %>%
        dplyr::bind_cols(aths) %>%
        janitor::clean_names() %>%
        dplyr::mutate(dplyr::across(
          dplyr::any_of(c(
            "athlete_id_1",
            "athlete_id_2",
            "athlete_id_3"
          )),
          ~ as.integer(.x)
        ))
    }
    ## Written this way for compliance with data repository processing
    if (!("homeTeamName" %in% names(plays))) {
      plays <- plays %>%
        dplyr::bind_cols(id_vars)
    }

    plays <- plays %>%
      dplyr::select(-dplyr::any_of(c("athlete.id", "athlete_id"))) %>%
      janitor::clean_names() %>%
      # Drop any unnamed/auto-repaired column (tibble turns an empty JSON field
      # name into "...N", which clean_names() renames to "xN").
      dplyr::select(-dplyr::matches("^x[0-9]+$")) %>%
      dplyr::mutate(
        game_id = gameId,
        season = season,
        season_type = season_type,
        game_date = game_date,
        game_date_time = game_date_time
      ) %>%
      dplyr::rename(dplyr::any_of(c(
        "athlete_id_1" = "participants_0_athlete_id",
        "athlete_id_2" = "participants_1_athlete_id",
        "athlete_id_3" = "participants_2_athlete_id"
      )))

    plays <- plays %>%
      dplyr::mutate(dplyr::across(
        dplyr::any_of(c(
          "athlete_id_1",
          "athlete_id_2",
          "athlete_id_3",
          "type_id",
          "team_id"
        )),
        ~ as.integer(.x)
      ))

    plays_df <- plays %>%
      make_baseballr_data(
        "ESPN MLB Play-by-Play Information from ESPN.com",
        Sys.time()
      )

    return(plays_df)
  }
}
