#' **Get ESPN MLB Injuries**
#' @name espn_mlb_injuries
NULL
#' @title
#' **Get ESPN MLB Injuries**
#' @rdname espn_mlb_injuries
#' @author Saiem Gilani
#' @param season Numeric or character season year (e.g. `2025`). The ESPN
#'   injury endpoint does not filter by season server-side; the value is
#'   attached as a constant column on the returned tibble for downstream joins.
#'   Defaults to `most_recent_mlb_season()`.
#' @param ... Currently unused; reserved for future argument threading.
#' @return Returns a tibble of league-wide MLB injury records.
#'   Returns an empty tibble (zero rows) when no injuries are reported.
#'
#'    **Injuries**
#'
#'    |col_name      |types     |description                                           |
#'    |:-------------|:---------|:-----------------------------------------------------|
#'    |team_id       |character |Unique team identifier.                               |
#'    |athlete_id    |character |Unique athlete identifier (ESPN).                     |
#'    |athlete_name  |character |Athlete display name (ESPN).                          |
#'    |position      |character |Listed roster position (G, F, C, etc.).               |
#'    |status        |character |Status label.                                         |
#'    |date          |character |Date in YYYY-MM-DD format.                            |
#'    |type          |character |Record type / category.                               |
#'    |side          |character |Side label (e.g. 'home', 'away', or 'overUnder').     |
#'    |returns_at    |character |                                                      |
#'    |short_comment |character |                                                      |
#'    |long_comment  |character |Long-form play / event comment.                       |
#'    |season        |integer   |Season identifier (4-digit year or 'YYYY-YY' string). |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_injuries()
#' }
espn_mlb_injuries <- function(season = most_recent_mlb_season(), ...) {
  .espn_baseball_league_injuries(
    league = "mlb",
    season = season,
    ...
  )
}


#' **Get ESPN MLB Team Injuries**
#' @name espn_mlb_team_injuries
NULL
#' @title
#' **Get ESPN MLB Team Injuries**
#' @rdname espn_mlb_team_injuries
#' @author Saiem Gilani
#' @description
#' Injury reports for one MLB team from ESPN's core API
#' (`sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/teams/{team_id}/injuries`).
#' The list links to each injury, and each injury to its athlete, so the
#' function makes one request per injury and one per injured player, which
#' takes a few seconds for a team with many injuries. A lookup that fails
#' leaves that row's fields `NA` and prints the cause.
#' @param team_id ESPN team identifier (character or numeric; passed as-is).
#'   Use `espn_mlb_teams()` to look up team IDs. Example: `"13"` (Texas
#'   Rangers).
#' @param ... Currently unused; reserved for future argument threading.
#' @return Returns a tibble of injury records for the specified MLB team, one
#'   row per injury ESPN lists for it (current and recent reports). Returns an
#'   empty tibble with the same columns when the team has no reported
#'   injuries. All columns are character.
#'
#'    **Injuries**
#'
#'    |col_name      |types     |description                                                        |
#'    |:-------------|:---------|:------------------------------------------------------------------|
#'    |team_id       |character |Unique team identifier (ESPN).                                     |
#'    |athlete_id    |character |Unique athlete identifier (ESPN).                                  |
#'    |athlete_name  |character |Athlete display name (ESPN).                                       |
#'    |position      |character |Position abbreviation (e.g. 'SP', 'RP', 'CF').                     |
#'    |status        |character |Injury status (e.g. 'Out', 'Day-To-Day').                          |
#'    |date          |character |Report timestamp, ISO 8601 UTC (e.g. '2026-11-03T23:00Z').         |
#'    |type          |character |Injured body part or injury type (e.g. 'Elbow').                   |
#'    |side          |character |Side of the body ('Left', 'Right', 'Not Specified'), or NA.        |
#'    |returns_at    |character |Expected return date, YYYY-MM-DD, or NA.                           |
#'    |short_comment |character |Short injury note.                                                 |
#'    |long_comment  |character |Long-form injury note.                                             |
#'    |injury_id     |character |Unique injury report identifier (ESPN).                            |
#'    |detail        |character |Injury detail (e.g. 'Surgery', 'Inflammation', 'Strain'), or NA.   |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_team_injuries(team_id = "13")
#' }
espn_mlb_team_injuries <- function(team_id, ...) {
  .espn_baseball_team_injuries(
    league  = "mlb",
    team_id = team_id,
    ...
  )
}
