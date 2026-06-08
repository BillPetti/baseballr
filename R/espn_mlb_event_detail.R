# espn_mlb_event_detail.R
# Public MLB shims for ESPN event-detail endpoints.
# These are thin wrappers over the internal helpers in
# espn_baseball_event_helpers.R.

# ---------------------------------------------------------------------------
# espn_mlb_game_odds
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Odds**
#' @name espn_mlb_game_odds
NULL
#' @title
#' **Get ESPN MLB Event Odds**
#' @rdname espn_mlb_game_odds
#' @author Saiem Gilani
#' @param event_id ESPN event/game identifier (character or numeric).
#' @param ... Additional arguments; currently unused but retained for
#'   forward compatibility. Proxy configuration should use
#'   `options(baseballr.proxy = ...)` -- see `?baseballr` for details.
#' @return A tibble with one row per odds provider.
#'
#'    |col_name             |types     |description                            |
#'    |:--------------------|:---------|:--------------------------------------|
#'    |event_id             |character |Unique event / game identifier (ESPN). |
#'    |provider_id          |character |Unique identifier for provider.        |
#'    |provider_name        |character |Provider name.                         |
#'    |details              |character |Details.                               |
#'    |over_under           |numeric   |Over under.                            |
#'    |spread               |numeric   |Spread.                                |
#'    |home_money_line      |integer   |                                       |
#'    |away_money_line      |integer   |                                       |
#'    |home_team_odds_open  |numeric   |                                       |
#'    |home_team_odds_close |numeric   |                                       |
#'    |away_team_odds_open  |numeric   |                                       |
#'    |away_team_odds_close |numeric   |                                       |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble any_of bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_odds(event_id = "401283399")
#' }
espn_mlb_game_odds <- function(event_id, ...) {
  .espn_baseball_event_odds(
    league   = "mlb",
    event_id = event_id,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_game_probabilities
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Win Probabilities**
#' @name espn_mlb_game_probabilities
NULL
#' @title
#' **Get ESPN MLB Event Win Probabilities**
#' @rdname espn_mlb_game_probabilities
#' @author Saiem Gilani
#' @param event_id ESPN event/game identifier (character or numeric).
#' @param limit integer. Maximum number of probability rows to return.
#'   Defaults to `200`. Pagination is handled internally.
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per play-level win-probability entry.
#'
#'    |col_name              |types     |description                                              |
#'    |:---------------------|:---------|:--------------------------------------------------------|
#'    |event_id              |character |Unique event / game identifier (ESPN).                   |
#'    |sequence_number       |character |Sequence number of the play within the game.             |
#'    |play_id               |character |Unique play identifier within a game.                    |
#'    |period                |integer   |Inning number.                                           |
#'    |clock                 |character |Game clock value (typically empty for baseball).         |
#'    |home_win_percentage   |numeric   |Home win percentage (0-1 decimal).                       |
#'    |away_win_percentage   |numeric   |Away win percentage (0-1 decimal).                       |
#'    |tie_percentage        |numeric   |Tie percentage (0-1 decimal).                            |
#'    |secs_to_end_of_period |numeric   |                                                         |
#'    |secs_to_end_of_game   |numeric   |                                                         |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble any_of bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_probabilities(event_id = "401283399", limit = 50)
#' }
espn_mlb_game_probabilities <- function(event_id, limit = 200, ...) {
  .espn_baseball_event_probabilities(
    league   = "mlb",
    event_id = event_id,
    limit    = limit,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_game_officials
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Officials**
#' @name espn_mlb_game_officials
NULL
#' @title
#' **Get ESPN MLB Event Officials**
#' @rdname espn_mlb_game_officials
#' @author Saiem Gilani
#' @param event_id ESPN event/game identifier (character or numeric).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per official assigned to the game.
#'
#'    |col_name      |types     |description                                            |
#'    |:-------------|:---------|:------------------------------------------------------|
#'    |event_id      |character |Unique event / game identifier (ESPN).                 |
#'    |official_id   |character |Unique official / referee identifier.                  |
#'    |full_name     |character |Player's full name.                                    |
#'    |display_name  |character |Display name.                                          |
#'    |position_id   |character |Unique position identifier.                            |
#'    |position_name |character |Listed roster position (e.g. 'Pitcher', 'Shortstop', 'Catcher'). |
#'    |position_type |character |                                                       |
#'    |order         |integer   |Display order within the result set.                   |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble any_of bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_officials(event_id = "401283399")
#' }
espn_mlb_game_officials <- function(event_id, ...) {
  .espn_baseball_event_officials(
    league   = "mlb",
    event_id = event_id,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_game_broadcasts
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Broadcasts**
#' @name espn_mlb_game_broadcasts
NULL
#' @title
#' **Get ESPN MLB Event Broadcasts**
#' @rdname espn_mlb_game_broadcasts
#' @author Saiem Gilani
#' @param event_id ESPN event/game identifier (character or numeric).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per broadcast outlet for the game.
#'
#'    |col_name        |types     |description                            |
#'    |:---------------|:---------|:--------------------------------------|
#'    |event_id        |character |Unique event / game identifier (ESPN). |
#'    |broadcast_id    |character |                                       |
#'    |type_id         |character |Type identifier (numeric).             |
#'    |type_short_name |character |                                       |
#'    |type_long_name  |character |                                       |
#'    |market_id       |character |                                       |
#'    |market_type     |character |                                       |
#'    |names           |character |                                       |
#'    |lang            |character |                                       |
#'    |region          |character |Region label.                          |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble any_of bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_broadcasts(event_id = "401283399")
#' }
espn_mlb_game_broadcasts <- function(event_id, ...) {
  .espn_baseball_event_broadcasts(
    league   = "mlb",
    event_id = event_id,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_game_situation
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Situation (Live)**
#' @name espn_mlb_game_situation
NULL
#' @title
#' **Get ESPN MLB Event Situation (Live)**
#' @rdname espn_mlb_game_situation
#' @author Saiem Gilani
#' @description
#' Returns the live game situation for one MLB event: timeouts remaining,
#' team fouls, fouls to give, bonus state, and a `$ref` to the last play.
#' During a live game this reflects current state; after the game ends
#' the values are frozen.
#'
#' @param event_id ESPN event identifier.
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble with timeouts + fouls for both teams.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |home_timeouts_current |integer |Home timeouts current. |
#'    |home_timeouts_remaining |integer |Home timeouts remaining. |
#'    |away_timeouts_current |integer |Away timeouts current. |
#'    |away_timeouts_remaining |integer |Away timeouts remaining. |
#'    |home_team_fouls |integer |Home team fouls. |
#'    |home_team_fouls_current |integer |Home team fouls current. |
#'    |home_fouls_to_give |integer |Home fouls to give. |
#'    |home_bonus_state |character |Home bonus state. |
#'    |away_team_fouls |integer |Away team fouls. |
#'    |away_team_fouls_current |integer |Away team fouls current. |
#'    |away_fouls_to_give |integer |Away fouls to give. |
#'    |away_bonus_state |character |Away bonus state. |
#'    |last_play_ref |character |Last play ref. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_situation(event_id = 401283399)
#' }
espn_mlb_game_situation <- function(event_id, ...) {
  .espn_baseball_event_situation(league = "mlb", event_id = event_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_predictor
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Predictor (Pre-game)**
#' @name espn_mlb_game_predictor
#' @title
#' **Get ESPN MLB Event Predictor (Pre-game)**
#' @rdname espn_mlb_game_predictor
#' @author Saiem Gilani
#' @description
#' Returns pre-game predictor statistics for one MLB event in long
#' format: one row per (team × statistic). Typical stats include
#' matchup quality, predicted score, win probability, and team
#' strength metrics. Returns empty for events without predictor data
#' (often the case for already-played games).
#'
#' @param event_id ESPN event identifier.
#' @param ... Additional arguments; currently unused.
#' @return A long tibble with rows for both home and away teams.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |name |character |Name. |
#'    |short_name |character |Short display name. |
#'    |last_modified |character |Last modified. |
#'    |side |character |Side. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |stat_name |character |Statistic name. |
#'    |stat_display |character |Stat display. |
#'    |description |character |Description. |
#'    |value |numeric |Numeric value. |
#'    |display_value |character |Human-readable value. |
#'    |team_ref |character |Team ref. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_predictor(event_id = 401283399)
#' }
espn_mlb_game_predictor <- function(event_id, ...) {
  .espn_baseball_event_predictor(league = "mlb", event_id = event_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_powerindex
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Power Index Index**
#' @name espn_mlb_game_powerindex
#' @title
#' **Get ESPN MLB Event Power Index Index**
#' @rdname espn_mlb_game_powerindex
#' @author Saiem Gilani
#' @description
#' Returns the per-team power-index `$ref` URLs for one MLB event.
#' Coverage is sparse — many events return zero items.
#'
#' @param event_id ESPN event identifier.
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per team-game power-index entry.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |ref |character |Ref. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_powerindex(event_id = 401283399)
#' }
espn_mlb_game_powerindex <- function(event_id, ...) {
  .espn_baseball_event_powerindex(league = "mlb", event_id = event_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_propbets
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Prop Bets (Long Format)**
#' @name espn_mlb_game_propbets
#' @title
#' **Get ESPN MLB Event Prop Bets (Long Format)**
#' @rdname espn_mlb_game_propbets
#' @author Saiem Gilani
#' @description
#' Returns prop-bet markets for one MLB event + provider in long format:
#' one row per (athlete × prop type). Each row has american / decimal /
#' fraction odds plus the current target (e.g. over/under line). Hits the
#' core-v2 `competitions/{id}/odds/{provider_id}/propBets` endpoint and
#' auto-paginates.
#'
#' @param event_id ESPN event identifier.
#' @param provider_id Sportsbook provider id (e.g. 58 = ESPN BET,
#'   100 = Caesars). Look up via [espn_mlb_game_odds()].
#' @param ... Additional arguments; currently unused.
#' @return A long tibble with one row per (athlete × prop type).
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |provider_id |character |Provider id. |
#'    |athlete_id |character |Unique ESPN athlete identifier. |
#'    |prop_type_id |character |Prop type id. |
#'    |prop_type_name |character |Prop type name. |
#'    |american |character |American. |
#'    |decimal |numeric |Decimal. |
#'    |fraction |character |Fraction. |
#'    |total |numeric |Total. |
#'    |current_target |numeric |Current target. |
#'    |last_updated |character |Last updated. |
#'    |athlete_ref |character |Athlete ref. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_propbets(event_id = 401283399, provider_id = 58)
#' }
espn_mlb_game_propbets <- function(event_id, provider_id, ...) {
  .espn_baseball_event_propbets(league = "mlb", event_id = event_id,
                                    provider_id = provider_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_team_linescores
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Competitor Linescores (Per-Inning)**
#' @name espn_mlb_game_team_linescores
NULL
#' @title
#' **Get ESPN MLB Event Competitor Linescores (Per-Inning)**
#' @rdname espn_mlb_game_team_linescores
#' @author Saiem Gilani
#' @description
#' Returns the per-inning run breakdown for one team in one MLB event.
#' One row per inning (regulation innings plus any extra innings).
#'
#' @param event_id ESPN event identifier.
#' @param team_id ESPN team identifier (the competitor whose linescore
#'   to fetch).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per inning.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |period |integer |Inning number. |
#'    |value |numeric |Numeric value. |
#'    |display_value |character |Human-readable value. |
#'    |source |character |Source. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_team_linescores(event_id = 401283399, team_id = 29)
#' }
espn_mlb_game_team_linescores <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_linescores(league = "mlb",
                                                  event_id = event_id,
                                                  team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_team_leaders
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Competitor Leaders (Top Performers)**
#' @name espn_mlb_game_team_leaders
NULL
#' @title
#' **Get ESPN MLB Event Competitor Leaders (Top Performers)**
#' @rdname espn_mlb_game_team_leaders
#' @author Saiem Gilani
#' @description
#' Returns the per-team statistical leaders for one MLB event in long
#' format: one row per (category x athlete rank). Categories typically
#' include batting average, home runs, RBIs, and MLB rating.
#'
#' @param event_id ESPN event identifier.
#' @param team_id ESPN team identifier.
#' @param ... Additional arguments; currently unused.
#' @return A long tibble with one row per (category x rank).
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |category_name |character |Statistic category name. |
#'    |category_display |character |Category display. |
#'    |category_abbrev |character |Category abbrev. |
#'    |rank |integer |Rank within the result set. |
#'    |athlete_id |character |Unique ESPN athlete identifier. |
#'    |display_value |character |Human-readable value. |
#'    |value |numeric |Numeric value. |
#'    |athlete_ref |character |Athlete ref. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_team_leaders(event_id = 401283399, team_id = 29)
#' }
espn_mlb_game_team_leaders <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_leaders(league = "mlb",
                                              event_id = event_id,
                                              team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_team_roster
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Competitor Roster (Game-Day)**
#' @name espn_mlb_game_team_roster
NULL
#' @title
#' **Get ESPN MLB Event Competitor Roster (Game-Day)**
#' @rdname espn_mlb_game_team_roster
#' @author Saiem Gilani
#' @description
#' Returns the game-day roster index for one team in one MLB event.
#' Each row carries the athlete id and the core-v2 `$ref` URL — use
#' the ref to dereference athlete-game splits or biographical data.
#'
#' @param event_id ESPN event identifier.
#' @param team_id ESPN team identifier.
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per active athlete.
#'
#'    Note: ESPN's core-v2 per-game competitor roster is not populated for
#'    MLB and typically returns an empty tibble; use [espn_mlb_game_rosters()]
#'    (site summary) for game-day rosters. When populated:
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |athlete_id |character |Unique ESPN athlete identifier. |
#'    |athlete_display_name |character |Player display name. |
#'    |position |character |Position abbreviation. |
#'    |jersey |character |Jersey number. |
#'    |starter |logical |TRUE if the player started the game. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_team_roster(event_id = 401283399, team_id = 29)
#' }
espn_mlb_game_team_roster <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_roster(league = "mlb",
                                             event_id = event_id,
                                             team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_team_statistics
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Competitor Team Statistics (Long Format)**
#' @name espn_mlb_game_team_statistics
NULL
#' @title
#' **Get ESPN MLB Event Competitor Team Statistics (Long Format)**
#' @rdname espn_mlb_game_team_statistics
#' @author Saiem Gilani
#' @description
#' Returns full team-game statistics for one team in one MLB event in
#' long format: one row per (category x stat). Covers offensive,
#' defensive, and general categories with both raw values and display
#' strings.
#'
#' @param event_id ESPN event identifier.
#' @param team_id ESPN team identifier.
#' @param ... Additional arguments; currently unused.
#' @return A long tibble with one row per (category x stat).
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |category_name |character |Statistic category name. |
#'    |category_display |character |Category display. |
#'    |stat_name |character |Statistic name. |
#'    |stat_abbrev |character |Stat abbrev. |
#'    |stat_display |character |Stat display. |
#'    |value |numeric |Numeric value. |
#'    |display_value |character |Human-readable value. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_team_statistics(event_id = 401283399, team_id = 29)
#' }
espn_mlb_game_team_statistics <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_statistics(league = "mlb",
                                                 event_id = event_id,
                                                 team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_team_records
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Competitor Records (At-Game Breakdown)**
#' @name espn_mlb_game_team_records
NULL
#' @title
#' **Get ESPN MLB Event Competitor Records (At-Game Breakdown)**
#' @rdname espn_mlb_game_team_records
#' @author Saiem Gilani
#' @description
#' Returns team records as of the given MLB event: overall, home,
#' away, conference, and division breakdowns where available. One row
#' per record type.
#'
#' @param event_id ESPN event identifier.
#' @param team_id ESPN team identifier.
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per record type.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |record_id |character |Record id. |
#'    |name |character |Name. |
#'    |abbreviation |character |Short abbreviation. |
#'    |display_name |character |Display name. |
#'    |short_display_name |character |Short display name. |
#'    |type |character |Type. |
#'    |summary |character |Record summary (e.g. 'W-L'). |
#'    |value |numeric |Numeric value. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_team_records(event_id = 401283399, team_id = 29)
#' }
espn_mlb_game_team_records <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_records(league = "mlb",
                                              event_id = event_id,
                                              team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_player_box
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Player Box Score (Long Format)**
#' @name espn_mlb_game_player_box
NULL
#' @title
#' **Get ESPN MLB Event Player Box Score (Long Format)**
#' @rdname espn_mlb_game_player_box
#' @author Saiem Gilani
#' @description
#' Returns the long-format per-game box score for a single athlete in one
#' MLB event. One row per (category x stat). Same shape as
#' [espn_mlb_game_team_statistics()] but scoped to a single
#' athlete-in-event instead of the full team. `stat_type` defaults to 0
#' (regular-season aggregate as ESPN tags it for finished events).
#'
#' @param event_id ESPN event identifier.
#' @param team_id ESPN team identifier (the competitor the athlete played for).
#' @param athlete_id ESPN athlete identifier.
#' @param stat_type Integer stat-type segment. Defaults to 0 (the only type
#'   commonly populated for finished events).
#' @param ... Additional arguments; currently unused.
#' @return A long tibble with one row per (category x stat).
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |athlete_id |character |Unique ESPN athlete identifier. |
#'    |stat_type |integer |Stat type. |
#'    |category_name |character |Category name. |
#'    |category_display |character |Category display. |
#'    |stat_name |character |Stat name. |
#'    |stat_abbrev |character |Stat abbrev. |
#'    |stat_display |character |Stat display. |
#'    |value |numeric |Numeric value. |
#'    |display_value |character |Human-readable value. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_player_box(event_id = 401283399, team_id = 29,
#'                                athlete_id = 33192)
#' }
espn_mlb_game_player_box <- function(event_id, team_id, athlete_id,
                                        stat_type = 0L, ...) {
  .espn_baseball_event_player_box(league = "mlb",
                                       event_id = event_id,
                                       team_id = team_id,
                                       athlete_id = athlete_id,
                                       stat_type = stat_type, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_team_roster_entry
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Competitor Roster Entry (Per-Athlete Game-Day Row)**
#' @name espn_mlb_game_team_roster_entry
NULL
#' @title
#' **Get ESPN MLB Event Competitor Roster Entry (Per-Athlete Game-Day Row)**
#' @rdname espn_mlb_game_team_roster_entry
#' @author Saiem Gilani
#' @description
#' Returns a single-row tibble describing one athlete's game-day roster
#' entry for one MLB event. Carries the **starter** flag, **didNotPlay**
#' flag with reason, ejection flag, and the substitution slot if the
#' athlete came in for another player. Pair with
#' [espn_mlb_game_team_roster()] to enumerate the roster.
#'
#' @param event_id ESPN event identifier.
#' @param team_id ESPN team identifier.
#' @param athlete_id ESPN athlete identifier.
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |athlete_id |character |Unique ESPN athlete identifier. |
#'    |player_id |character |Player id. |
#'    |period |integer |Inning number. |
#'    |active |logical |Active. |
#'    |starter |logical |TRUE if a starter. |
#'    |did_not_play |logical |Did not play. |
#'    |reason |character |Reason. |
#'    |ejected |logical |Ejected. |
#'    |for_player_id |character |For player id. |
#'    |jersey |character |Jersey number. |
#'    |display_name |character |Display name. |
#'    |athlete_ref |character |Athlete ref. |
#'    |position_ref |character |Position ref. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_team_roster_entry(event_id = 401283399,
#'                                             team_id = 13,
#'                                             athlete_id = 33192)
#' }
espn_mlb_game_team_roster_entry <- function(event_id, team_id,
                                                     athlete_id, ...) {
  .espn_baseball_event_competitor_roster_entry(league = "mlb",
                                                   event_id = event_id,
                                                   team_id = team_id,
                                                   athlete_id = athlete_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_play
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Play Detail (Single Play)**
#' @name espn_mlb_game_play
NULL
#' @title
#' **Get ESPN MLB Event Play Detail (Single Play)**
#' @rdname espn_mlb_game_play
#' @author Saiem Gilani
#' @description
#' Returns the rich detail block for a single MLB play: sequence, period,
#' clock, text, scoring/shooting flags, current home/away score, team
#' `$ref`, and shot coordinates if applicable. Complements the bulk
#' [espn_mlb_pbp()] output by exposing the canonical core-v2 play record.
#'
#' @param event_id ESPN event identifier.
#' @param play_id ESPN play identifier (visible in `pbp()` output as
#'   `play_id` or extractable from `plays[].$ref`).
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |play_id |character |Play id. |
#'    |sequence_number |character |Sequence number. |
#'    |type_id |character |Type id. |
#'    |type_text |character |Type text. |
#'    |text |character |Text description. |
#'    |short_text |character |Short text. |
#'    |period |integer |Inning number. |
#'    |clock |character |Clock. |
#'    |scoring_play |logical |Scoring play. |
#'    |score_value |numeric |Score value. |
#'    |away_score |integer |Away team run total. |
#'    |home_score |integer |Home team run total. |
#'    |shooting_play |logical |Shooting play. |
#'    |coordinate_x |numeric |Coordinate x. |
#'    |coordinate_y |numeric |Coordinate y. |
#'    |team_ref |character |Team ref. |
#'    |wallclock |character |Wallclock. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_play(event_id = 401283399, play_id = 4012833994)
#' }
espn_mlb_game_play <- function(event_id, play_id, ...) {
  .espn_baseball_event_play(league = "mlb",
                                event_id = event_id,
                                play_id = play_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_play_personnel
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Play Personnel (On-Court Lineup at Play)**
#' @name espn_mlb_game_play_personnel
NULL
#' @title
#' **Get ESPN MLB Event Play Personnel (On-Court Lineup at Play)**
#' @rdname espn_mlb_game_play_personnel
#' @author Saiem Gilani
#' @description
#' Returns the players on court at a specific MLB play in long format
#' (one row per athlete entry across both competitors). Foundation for
#' lineup analysis. ESPN coverage is sparse — many plays return zero
#' rows; the wrapper returns a typed empty tibble in that case.
#'
#' @param event_id ESPN event identifier.
#' @param play_id ESPN play identifier.
#' @param ... Additional arguments; currently unused.
#' @return A long tibble with one row per on-court athlete.
#'
#'    Note: ESPN baseball plays do not carry a personnel sub-resource, so this
#'    typically returns an empty tibble (it is meaningful for sports such as
#'    football). When populated:
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |play_id |character |Unique play identifier within the game. |
#'    |athlete_id |character |Athlete involved in the play. |
#'    |participant_type |character |Role of the athlete in the play. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_play_personnel(event_id = 401283399, play_id = 4012833994)
#' }
espn_mlb_game_play_personnel <- function(event_id, play_id, ...) {
  .espn_baseball_event_play_personnel(league = "mlb",
                                          event_id = event_id,
                                          play_id = play_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_team_score
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Competitor Score (Single Row)**
#' @name espn_mlb_game_team_score
NULL
#' @title
#' **Get ESPN MLB Event Competitor Score (Single Row)**
#' @rdname espn_mlb_game_team_score
#' @author Saiem Gilani
#' @description
#' Returns a one-row tibble with one team's final score for one MLB event:
#' numeric `value`, display string, `winner` flag, and source metadata.
#' Quick-lookup wrapper — use [espn_mlb_game_team_linescores()] for
#' per-period detail.
#'
#' @param event_id ESPN event identifier.
#' @param team_id ESPN team identifier.
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |value |numeric |Numeric value. |
#'    |display_value |character |Human-readable value. |
#'    |winner |logical |Winner. |
#'    |source_id |character |Source id. |
#'    |source_description |character |Source description. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_team_score(event_id = 401283399, team_id = 29)
#' }
espn_mlb_game_team_score <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_score(league = "mlb",
                                            event_id = event_id,
                                            team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_game_official_detail
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Event Official Detail (Single Official)**
#' @name espn_mlb_game_official_detail
NULL
#' @title
#' **Get ESPN MLB Event Official Detail (Single Official)**
#' @rdname espn_mlb_game_official_detail
#' @author Saiem Gilani
#' @description
#' Returns a single-row tibble for one referee assigned to one MLB event,
#' with their name, position (Referee / Crew Chief / Umpire), and crew
#' order. Pair with [espn_mlb_game_officials()] to enumerate the crew.
#'
#' @param event_id ESPN event identifier.
#' @param order Crew order index (1 = first official). Pair with the `order` column from event_officials().
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |official_id |character |Official id. |
#'    |first_name |character |First name. |
#'    |last_name |character |Last name. |
#'    |full_name |character |Full name. |
#'    |display_name |character |Display name. |
#'    |position_id |character |Unique position identifier. |
#'    |position_name |character |Position name. |
#'    |order |integer |Order. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_game_official_detail(event_id = 401283399, order = 1)
#' }
espn_mlb_game_official_detail <- function(event_id, order, ...) {
  .espn_baseball_event_official_detail(league = "mlb",
                                           event_id = event_id,
                                           order = order, ...)
}

