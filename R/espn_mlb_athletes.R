# espn_mlb_athletes.R
# Public MLB shims for ESPN athlete endpoints.
# These are thin wrappers over the internal helpers in
# espn_baseball_athlete_helpers.R.

# ---------------------------------------------------------------------------
# espn_mlb_player_info
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Info**
#' @name espn_mlb_player_info
NULL
#' @title
#' **Get ESPN MLB Athlete Info**
#' @rdname espn_mlb_player_info
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Additional arguments; currently unused but retained for
#'   forward compatibility. Proxy configuration should use
#'   `options(baseballr.proxy = ...)` -- see `?baseballr` for details.
#' @return A named list of data frames: `Bio`, `Team`, `Position`,
#'   `Status`, `College`, `Draft`.
#'
#'    **Bio**
#'
#'    |col_name      |types     |description                       |
#'    |:-------------|:---------|:---------------------------------|
#'    |id            |character |Id.                               |
#'    |full_name     |character |Player's full name.               |
#'    |display_name  |character |Display name.                     |
#'    |jersey        |character |Jersey number worn by the player. |
#'    |age           |character |Player age (in years).            |
#'    |date_of_birth |character |Date of birth (YYYY-MM-DD).       |
#'    |headshot_href |character |Headshot image URL.               |
#'
#'    **Team**
#'
#'    |col_name     |types     |description         |
#'    |:------------|:---------|:-------------------|
#'    |id           |character |Id.                 |
#'    |abbreviation |character |Short abbreviation. |
#'    |display_name |character |Display name.       |
#'
#'    **Position**
#'
#'    |col_name     |types     |description         |
#'    |:------------|:---------|:-------------------|
#'    |id           |character |Id.                 |
#'    |name         |character |Display name.       |
#'    |abbreviation |character |Short abbreviation. |
#'
#'    **Status**
#'
#'    |col_name |types     |description             |
#'    |:--------|:---------|:-----------------------|
#'    |id       |character |Id.                     |
#'    |name     |character |Display name.           |
#'    |type     |character |Record type / category. |
#'
#'    **College**
#'
#'    |col_name |types     |description   |
#'    |:--------|:---------|:-------------|
#'    |id       |character |Id.           |
#'    |name     |character |Display name. |
#'    |mascot   |character |Team mascot.  |
#'
#'    **Draft**
#'
#'    |col_name  |types     |description                 |
#'    |:---------|:---------|:---------------------------|
#'    |year      |character |4-digit year.               |
#'    |round     |character |Tournament / playoff round. |
#'    |selection |character |                            |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble select any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_player_info(athlete_id = "1966")
#' }
espn_mlb_player_info <- function(athlete_id, ...) {
  .espn_baseball_athlete_info(
    league     = "mlb",
    athlete_id = athlete_id,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_player_overview
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Overview**
#' @name espn_mlb_player_overview
NULL
#' @title
#' **Get ESPN MLB Athlete Overview**
#' @rdname espn_mlb_player_overview
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A named list of data frames: `Statistics`, `NextGame`,
#'   `Last5Games`, `Headlines`, `FantasyOutlook`.
#'
#'    **Statistics**
#'
#'    |col_name |types     |description |
#'    |:--------|:---------|:-----------|
#'    |(varies) |character |            |
#'
#'    **NextGame**
#'
#'    |col_name   |types     |description                |
#'    |:----------|:---------|:--------------------------|
#'    |id         |character |Id.                        |
#'    |date       |character |Date in YYYY-MM-DD format. |
#'    |name       |character |Display name.              |
#'    |short_name |character |Short display name.        |
#'
#'    **Last5Games**
#'
#'    |col_name |types     |description |
#'    |:--------|:---------|:-----------|
#'    |(varies) |character |            |
#'
#'    **Headlines**
#'
#'    |col_name    |types     |description                       |
#'    |:-----------|:---------|:---------------------------------|
#'    |headline    |character |News headline.                    |
#'    |description |character |Long-form description text.       |
#'    |published   |character |Publication timestamp (ISO 8601). |
#'
#'    **FantasyOutlook**
#'
#'    |col_name |types     |description |
#'    |:--------|:---------|:-----------|
#'    |(varies) |character |            |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble select any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_player_overview(athlete_id = "1966", season = 2024)
#' }
espn_mlb_player_overview <- function(athlete_id,
                                       season = most_recent_mlb_season(),
                                       ...) {
  .espn_baseball_athlete_overview(
    league     = "mlb",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_player_stats_v3
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Stats**
#' @name espn_mlb_player_stats_v3
NULL
#' @title
#' **Get ESPN MLB Athlete Stats**
#' @rdname espn_mlb_player_stats_v3
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A named list of per-category tibbles. Default category names are
#'   `General`, `Offensive`, `Defensive`, `Rebounding`, `Shooting`, `Misc`.
#'   Actual names are driven by the ESPN response; additional categories may
#'   appear. Each tibble has columns depending on the category returned by
#'   ESPN.
#'
#'
#'    Note: ESPN's web-v3 athlete-statistics feed is sparsely populated for
#'    MLB and frequently returns an empty tibble; use [espn_mlb_player_stats()]
#'    or [espn_mlb_player_splits()] for season statistics. When populated:
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |athlete_id |character |Unique ESPN athlete identifier. |
#'    |season |integer |Season (4-digit year). |
#'    |category |character |Statistic category (e.g. 'batting', 'pitching'). |
#'    |stat_name |character |Statistic name. |
#'    |abbreviation |character |Statistic abbreviation. |
#'    |value |numeric |Numeric statistic value. |
#'    |display_value |character |Human-readable statistic value. |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble select any_of bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_player_stats_v3(athlete_id = "1966", season = 2024)
#' }
espn_mlb_player_stats_v3 <- function(athlete_id,
                                    season = most_recent_mlb_season(),
                                    ...) {
  .espn_baseball_athlete_stats(
    league     = "mlb",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_player_gamelog
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Gamelog**
#' @name espn_mlb_player_gamelog
NULL
#' @title
#' **Get ESPN MLB Athlete Gamelog**
#' @rdname espn_mlb_player_gamelog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A single tibble with one row per game. Column names reflect the
#'   stat labels returned by ESPN and will vary by season and player.
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |id |character |Identifier. |
#'    |at_vs |character |At vs. |
#'    |game_date |character |Game date (YYYY-MM-DD). |
#'    |score |character |Score. |
#'    |home_team_id |character |Unique ESPN team identifier. |
#'    |away_team_id |character |Unique ESPN team identifier. |
#'    |home_team_score |character |Home team score. |
#'    |away_team_score |character |Away team score. |
#'    |game_result |character |Game result (W/L). |
#'    |league_name |character |League name. |
#'    |league_abbreviation |character |League abbreviation. |
#'    |league_short_name |character |League short name. |
#'    |event_note |character |Event note. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |team_uid |character |ESPN universal team identifier (UID). |
#'    |team_abbreviation |character |Short team abbreviation (e.g. 'NYY'). |
#'    |team_logo |character |Team logo. |
#'    |team_is_all_star |character |Team is all star. |
#'    |opponent_id |character |Opponent team identifier. |
#'    |opponent_uid |character |ESPN UID. |
#'    |opponent_display_name |character |Display name. |
#'    |opponent_abbreviation |character |Opponent abbreviation. |
#'    |opponent_logo |character |Opponent logo. |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |week |character |Week. |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_player_gamelog(athlete_id = "1966", season = 2024)
#' }
espn_mlb_player_gamelog <- function(athlete_id,
                                      season = most_recent_mlb_season(),
                                      ...) {
  .espn_baseball_athlete_gamelog(
    league     = "mlb",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_player_splits
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Splits**
#' @name espn_mlb_player_splits
NULL
#' @title
#' **Get ESPN MLB Athlete Splits**
#' @rdname espn_mlb_player_splits
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A single long-format tibble. When data are present, columns include
#'   at minimum `category` and `split_name`, plus per-stat columns driven by
#'   ESPN labels.
#'
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |name |character |Name. |
#'    |display_name |character |Display name. |
#'    |splits |list |Splits. |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows select any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_player_splits(athlete_id = "1966", season = 2024)
#' }
espn_mlb_player_splits <- function(athlete_id,
                                     season = most_recent_mlb_season(),
                                     ...) {
  .espn_baseball_athlete_splits(
    league     = "mlb",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_player_eventlog
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Eventlog**
#' @name espn_mlb_player_eventlog
NULL
#' @title
#' **Get ESPN MLB Athlete Eventlog**
#' @rdname espn_mlb_player_eventlog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A single tibble. Per-event `statistics.$ref` URLs from the ESPN
#'   core-v2 API are returned as the character column `statistics_ref` and
#'   are NOT resolved. Similarly, `event_ref`, `competition_ref`, and
#'   `team_ref` are returned as character columns.
#'
#'    |col_name        |types     |description                              |
#'    |:---------------|:---------|:----------------------------------------|
#'    |event_ref       |character |Reference link to the originating event. |
#'    |competition_ref |character |                                         |
#'    |team_ref        |character |                                         |
#'    |statistics_ref  |character |                                         |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_player_eventlog(athlete_id = "1966", season = 2024)
#' }
espn_mlb_player_eventlog <- function(athlete_id,
                                       season = most_recent_mlb_season(),
                                       ...) {
  .espn_baseball_athlete_eventlog(
    league     = "mlb",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_player_awards
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Awards**
#' @name espn_mlb_player_awards
NULL
#' @title
#' **Get ESPN MLB Athlete Awards**
#' @rdname espn_mlb_player_awards
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Additional arguments; currently unused.
#' @return A single tibble. This endpoint is sparse; many athletes have no
#'   award data, in which case an empty tibble with canonical columns is
#'   returned.
#'
#'    |col_name    |types     |description                                           |
#'    |:-----------|:---------|:-----------------------------------------------------|
#'    |season      |character |Season identifier (4-digit year or 'YYYY-YY' string). |
#'    |award_id    |character |                                                      |
#'    |name        |character |Display name.                                         |
#'    |description |character |Long-form description text.                           |
#'    |date        |character |Date in YYYY-MM-DD format.                            |
#'    |type        |character |Record type / category.                               |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble select any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_player_awards(athlete_id = "1966")
#' }
espn_mlb_player_awards <- function(athlete_id, ...) {
  .espn_baseball_athlete_awards(
    league     = "mlb",
    athlete_id = athlete_id,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_player_statisticslog
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Statisticslog**
#' @name espn_mlb_player_statisticslog
NULL
#' @title
#' **Get ESPN MLB Athlete Statisticslog**
#' @rdname espn_mlb_player_statisticslog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A single tibble. When resolved, each row corresponds to one
#'   statistical entry in the core-v2 statistics log, with `event_ref` and
#'   `statistics_ref` character columns pointing to resolvable ESPN endpoints.
#'
#'    |col_name       |types     |description                              |
#'    |:--------------|:---------|:----------------------------------------|
#'    |event_ref      |character |Reference link to the originating event. |
#'    |statistics_ref |character |                                         |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_player_statisticslog(athlete_id = "1966", season = 2024)
#' }
espn_mlb_player_statisticslog <- function(athlete_id,
                                            season = most_recent_mlb_season(),
                                            ...) {
  .espn_baseball_athlete_statisticslog(
    league     = "mlb",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}
