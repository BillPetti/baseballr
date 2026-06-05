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
#' @param team_id ESPN team identifier (character or numeric; passed as-is).
#'   Use `espn_mlb_teams()` to look up team IDs. Example: `"17"` (Las Vegas
#'   Aces).
#' @param ... Currently unused; reserved for future argument threading.
#' @return Returns a tibble of injury records for the specified MLB team.
#'   Returns an empty tibble (zero rows) when the team has no reported
#'   injuries.
#'
#'    **Injuries**
#'
#'    |col_name      |types     |description                                       |
#'    |:-------------|:---------|:-------------------------------------------------|
#'    |team_id       |character |Unique team identifier.                           |
#'    |athlete_id    |character |Unique athlete identifier (ESPN).                 |
#'    |athlete_name  |character |Athlete display name (ESPN).                      |
#'    |position      |character |Listed roster position (G, F, C, etc.).           |
#'    |status        |character |Status label.                                     |
#'    |date          |character |Date in YYYY-MM-DD format.                        |
#'    |type          |character |Record type / category.                           |
#'    |side          |character |Side label (e.g. 'home', 'away', or 'overUnder'). |
#'    |returns_at    |character |                                                  |
#'    |short_comment |character |                                                  |
#'    |long_comment  |character |Long-form play / event comment.                   |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows
#' @importFrom janitor clean_names
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
