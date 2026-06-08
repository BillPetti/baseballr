# espn_mlb_athlete_career.R
# Public MLB shims for athlete career-level core-v2 endpoints.

# ---------------------------------------------------------------------------
# espn_mlb_player_seasons
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Career Seasons**
#' @name espn_mlb_player_seasons
NULL
#' @title
#' **Get ESPN MLB Athlete Career Seasons**
#' @rdname espn_mlb_player_seasons
#' @author Saiem Gilani
#' @description
#' Returns the list of seasons an MLB athlete appeared in. Useful for
#' bounding follow-up calls to per-season endpoints.
#'
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per career season.
#'
#'    |col_name   |types     |description                              |
#'    |:----------|:---------|:----------------------------------------|
#'    |league     |character |League slug.                             |
#'    |athlete_id |character |ESPN athlete id.                         |
#'    |season     |integer   |Season year.                             |
#'    |ref        |character |`$ref` URL to the season detail.         |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   # Aaron Judge (33192): 23 career seasons
#'   espn_mlb_player_seasons(athlete_id = 33192)
#' }
espn_mlb_player_seasons <- function(athlete_id, ...) {
  .espn_baseball_athlete_seasons(league = "mlb",
                                     athlete_id = athlete_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_player_career_stats
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Career Stats (Long Format)**
#' @name espn_mlb_player_career_stats
#' @title
#' **Get ESPN MLB Athlete Career Stats (Long Format)**
#' @rdname espn_mlb_player_career_stats
#' @author Saiem Gilani
#' @description
#' Returns career stats for an MLB athlete in long format. Default
#' `stat_type = 0L` fetches the standard "All Splits" / regular-season
#' view. Pass a vector like `c(0L, 1L, 2L)` to attempt multiple types
#' and bind them via a `stat_type_id` column; variants that 404 for that
#' athlete are silently skipped. Stat types: 0 = regular season (default
#' endpoint), 1 = postseason, 2 = career aggregate. Coverage of types 1
#' and 2 is sparse — many athletes only have type 0 populated.
#'
#' @param athlete_id ESPN athlete identifier.
#' @param stat_type Integer or integer vector of stat-type codes.
#'   Default `0L` fetches the standard "All Splits" / regular-season view.
#'   Pass a vector like `c(0L, 1L, 2L)` to bind multiple types via a
#'   `stat_type_id` column; non-existent variants are silently skipped.
#' @param ... Additional arguments; currently unused.
#' @return A long tibble (one row per stat_type × split × category × stat).
#'
#'    |col_name         |types     |description                                |
#'    |:----------------|:---------|:------------------------------------------|
#'    |league           |character |League slug.                               |
#'    |athlete_id       |character |ESPN athlete id.                           |
#'    |stat_type_id     |character |Stat-type code (0 = reg, 1 = post, 2 = career). |
#'    |split_id         |character |Split id.                                  |
#'    |split_name       |character |Split name (typically "All Splits").       |
#'    |split_type       |character |Split type code.                           |
#'    |category_name    |character |Category key (e.g. "defensive").           |
#'    |category_display |character |Category display name.                     |
#'    |category_short   |character |Category short display.                    |
#'    |category_abbrev  |character |Category abbreviation.                     |
#'    |stat_name        |character |Stat key.                                  |
#'    |stat_abbrev      |character |Stat abbreviation.                         |
#'    |stat_display     |character |Stat display name.                         |
#'    |stat_short       |character |Stat short display.                        |
#'    |description      |character |Stat description.                          |
#'    |value            |numeric   |Stat value.                                |
#'    |display_value    |character |Display-formatted value.                   |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   # Aaron Judge — regular + postseason combined
#'   espn_mlb_player_career_stats(athlete_id = 33192)
#'   # Just career aggregate
#'   espn_mlb_player_career_stats(athlete_id = 33192, stat_type = 2L)
#' }
espn_mlb_player_career_stats <- function(athlete_id,
                                           stat_type = 0L,
                                           ...) {
  .espn_baseball_athlete_career_stats(league = "mlb",
                                          athlete_id = athlete_id,
                                          stat_type = stat_type, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_draft_pick
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Draft Pick Detail**
#' @name espn_mlb_draft_pick
#' @title
#' **Get ESPN MLB Draft Pick Detail**
#' @rdname espn_mlb_draft_pick
#' @author Saiem Gilani
#' @description
#' Returns a single MLB draft pick. Defaults to the most recent MLB
#' season's #1 overall pick. For a full draft, use [espn_mlb_draft()].
#'
#' @param season Season year. Defaults to most recent MLB season.
#' @param round Draft round (default 1).
#' @param pick Pick number within the round (default 1).
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name    |types     |description                                |
#'    |:-----------|:---------|:------------------------------------------|
#'    |league      |character |League slug.                               |
#'    |season      |integer   |Season year of the draft.                  |
#'    |round       |integer   |Round number.                              |
#'    |pick        |integer   |Pick within the round.                     |
#'    |overall     |integer   |Overall pick number.                       |
#'    |traded      |logical   |Whether the pick was traded.               |
#'    |trade_note  |character |Trade note (if any).                       |
#'    |status      |character |Pick status name.                          |
#'    |athlete_id  |character |Drafted athlete's ESPN id.                 |
#'    |team_id     |character |Drafting team's ESPN id.                   |
#'    |athlete_ref |character |`$ref` to athlete.                         |
#'    |team_ref    |character |`$ref` to team.                            |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_draft_pick(season = 2024, round = 1, pick = 1)
#' }
espn_mlb_draft_pick <- function(season = most_recent_mlb_season(),
                                 round = 1L, pick = 1L, ...) {
  .espn_baseball_draft_pick(league = "mlb", season = season,
                                round = round, pick = pick, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_player_eventlog_v2 (core-v2 per-season event log)
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Per-Season Event Log (core-v2)**
#' @name espn_mlb_player_eventlog_v2
NULL
#' @title
#' **Get ESPN MLB Athlete Per-Season Event Log (core-v2)**
#' @rdname espn_mlb_player_eventlog_v2
#' @author Saiem Gilani
#' @description
#' Returns one row per (event x team) for an MLB athlete's appearances
#' in a given season. Distinct from [espn_mlb_player_eventlog()] which
#' wraps the web-common-v3 gamelog endpoint returning stats per game;
#' this core-v2 variant returns refs + `played` flag and is era-correct.
#'
#' @param athlete_id ESPN athlete identifier.
#' @param season Season year. Defaults to most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per event appearance. See package source
#'   for column schema.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |athlete_id |character |Unique ESPN athlete identifier. |
#'    |season |integer |Season (4-digit year). |
#'    |event_id |character |Unique ESPN event/game identifier. |
#'    |team_id |character |Unique ESPN team identifier. |
#'    |played |logical |Played. |
#'    |event_ref |character |Event ref. |
#'    |competition_ref |character |Competition ref. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_player_eventlog_v2(athlete_id = 33192, season = 2025)
#' }
espn_mlb_player_eventlog_v2 <- function(athlete_id,
                                          season = most_recent_mlb_season(),
                                          ...) {
  .espn_baseball_athlete_eventlog_v2(league = "mlb",
                                      athlete_id = athlete_id,
                                      season = season, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_draft_rounds
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Draft Rounds Summary**
#' @name espn_mlb_draft_rounds
#' @title
#' **Get ESPN MLB Draft Rounds Summary**
#' @rdname espn_mlb_draft_rounds
#' @author Saiem Gilani
#' @description
#' Returns one row per round of the MLB draft (typically 2 rounds:
#' 1st with 30 picks, 2nd with ~28 picks).
#'
#' @param season Season year. Defaults to most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per round.
#'
#'    Note: ESPN does not currently populate the MLB draft feed, so this
#'    typically returns an empty tibble. When populated, the columns are:
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |season |integer |Draft season (4-digit year). |
#'    |round |integer |Draft round number. |
#'    |display_name |character |Round display name. |
#'    |number_of_picks |integer |Number of picks in the round. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_draft_rounds(season = 2024)
#' }
espn_mlb_draft_rounds <- function(season = most_recent_mlb_season(), ...) {
  .espn_baseball_draft_rounds(league = "mlb", season = season, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_draft_athletes
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Draft Athletes Index**
#' @name espn_mlb_draft_athletes
#' @title
#' **Get ESPN MLB Draft Athletes Index**
#' @rdname espn_mlb_draft_athletes
#' @author Saiem Gilani
#' @description
#' Returns one row per drafted athlete in a given MLB draft year.
#'
#' @param season Season year. Defaults to most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A tibble of athlete IDs + `$ref` URLs.
#'
#'    Note: ESPN does not currently populate the MLB draft-prospect feed, so
#'    this typically returns an empty tibble. When populated, the columns are:
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |season |integer |Draft season (4-digit year). |
#'    |round |integer |Draft round number. |
#'    |pick |integer |Pick number within the round. |
#'    |overall |integer |Overall pick number. |
#'    |athlete_id |character |Unique ESPN athlete identifier. |
#'    |athlete_full_name |character |Drafted player full name. |
#'    |position |character |Player position abbreviation. |
#'    |team_id |character |Drafting team identifier. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_draft_athletes(season = 2024)
#' }
espn_mlb_draft_athletes <- function(season = most_recent_mlb_season(), ...) {
  .espn_baseball_draft_athletes(league = "mlb", season = season, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_draft_status
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Draft Status**
#' @name espn_mlb_draft_status
#' @title
#' **Get ESPN MLB Draft Status**
#' @rdname espn_mlb_draft_status
#' @author Saiem Gilani
#' @description
#' Returns the current status of one MLB draft year (round, state,
#' description). Live during the draft; static afterward.
#'
#' @param season Season year. Defaults to most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |league |character |League. |
#'    |season |integer |Season (4-digit year). |
#'    |round |integer |Draft round. |
#'    |type_id |character |Type id. |
#'    |type_name |character |Type name. |
#'    |type_state |character |Type state. |
#'    |description |character |Description. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_draft_status(season = 2024)
#' }
espn_mlb_draft_status <- function(season = most_recent_mlb_season(), ...) {
  .espn_baseball_draft_status(league = "mlb", season = season, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_season_draft
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Season Draft (Top-Level Metadata)**
#' @name espn_mlb_season_draft
NULL
#' @title
#' **Get ESPN MLB Season Draft (Top-Level Metadata)**
#' @rdname espn_mlb_season_draft
#' @author Saiem Gilani
#' @description
#' Returns a single-row tibble with top-level draft-year metadata: year,
#' number of rounds, display name, plus `$ref`s for the deeper sub-resources
#' (status, athletes, rounds) already wrapped by [espn_mlb_draft_status()],
#' [espn_mlb_draft_athletes()], and [espn_mlb_draft_rounds()].
#'
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    Note: ESPN does not currently populate the MLB draft feed, so this
#'    typically returns an empty/metadata-only tibble. When populated:
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |season |integer |Draft season (4-digit year). |
#'    |display_name |character |Draft display name. |
#'    |status |character |Draft status (e.g. 'pre', 'complete'). |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_season_draft(season = 2024)
#' }
espn_mlb_season_draft <- function(season = most_recent_mlb_season(), ...) {
  .espn_baseball_season_draft(league = "mlb", season = season, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_draft_athlete_detail
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Draft Athlete Detail (Single Drafted Player)**
#' @name espn_mlb_draft_athlete_detail
NULL
#' @title
#' **Get ESPN MLB Draft Athlete Detail (Single Drafted Player)**
#' @rdname espn_mlb_draft_athlete_detail
#' @author Saiem Gilani
#' @description
#' Returns rich single-row detail for one drafted athlete in one MLB
#' draft year: name, height, weight, position, pick (overall/round/team),
#' and a `$ref` to the athlete's core-v2 profile. Use
#' [espn_mlb_draft_athletes()] to enumerate draftees for a year.
#'
#' @param season Draft year (numeric). Defaults to the most recent MLB season.
#' @param athlete_id ESPN draftee identifier.
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    Note: ESPN does not currently populate the MLB draft feed, so this
#'    typically returns an empty tibble. When populated, the columns are:
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |season |integer |Draft season (4-digit year). |
#'    |athlete_id |character |Unique ESPN athlete identifier. |
#'    |athlete_full_name |character |Drafted player full name. |
#'    |position |character |Player position abbreviation. |
#'    |round |integer |Draft round number. |
#'    |pick |integer |Pick number within the round. |
#'    |overall |integer |Overall pick number. |
#'    |team_id |character |Drafting team identifier. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_draft_athlete_detail(season = 2024, athlete_id = 33192)
#' }
espn_mlb_draft_athlete_detail <- function(season = most_recent_mlb_season(),
                                            athlete_id, ...) {
  .espn_baseball_draft_athlete_detail(league = "mlb",
                                          season = season,
                                          athlete_id = athlete_id, ...)
}
