# espn_mlb_groups_v2.R
# Public MLB shims for ESPN per-season group / conference endpoints.
# (Distinct from the existing site-v2 espn_mlb_groups, which targets a
# different endpoint family — this batch wraps the core-v2 groups.)

# ---------------------------------------------------------------------------
# espn_mlb_season_groups
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Season Groups Index**
#' @name espn_mlb_season_groups
NULL
#' @title
#' **Get ESPN MLB Season Groups Index**
#' @rdname espn_mlb_season_groups
#' @author Saiem Gilani
#' @description
#' Returns the list of group IDs (conferences / divisions) for one
#' (MLB season x season-type) via core-v2
#' `/seasons/{season}/types/{season_type}/groups`.
#'
#' @param season Season year. Defaults to most recent MLB season.
#' @param season_type Season-type id (2 = regular (default)).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per group.
#'
#'    |col_name    |types     |description                              |
#'    |:-----------|:---------|:----------------------------------------|
#'    |league      |character |League slug.                             |
#'    |season      |integer   |Season year.                             |
#'    |season_type |integer   |Season-type id.                          |
#'    |group_id    |character |ESPN group id.                           |
#'    |ref         |character |`$ref` URL for the group detail.         |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_season_groups(season = 2025)
#' }
espn_mlb_season_groups <- function(season = most_recent_mlb_season(),
                                    season_type = c(2L, 3L), ...) {
  .espn_baseball_season_groups(league = "mlb", season = season,
                                   season_type = season_type, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_season_group
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Season Group Detail**
#' @name espn_mlb_season_group
#' @title
#' **Get ESPN MLB Season Group Detail**
#' @rdname espn_mlb_season_group
#' @author Saiem Gilani
#' @description
#' Returns metadata for one group (conference or division) in one
#' (MLB season x season-type), plus `$ref` URLs to its parent group,
#' children groups, member teams, and standings.
#'
#' @param group_id ESPN group identifier.
#' @param season Season year. Defaults to most recent MLB season.
#' @param season_type Season-type id (2 = regular (default)).
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name       |types     |description                                |
#'    |:--------------|:---------|:------------------------------------------|
#'    |league         |character |League slug.                               |
#'    |season         |integer   |Season year.                               |
#'    |season_type    |integer   |Season-type id.                            |
#'    |group_id       |character |ESPN group id.                             |
#'    |uid            |character |ESPN UID string.                           |
#'    |name           |character |Full name (e.g. "Eastern Conference").     |
#'    |abbreviation   |character |Short code (e.g. "EAST").                  |
#'    |short_name     |character |Short name.                                |
#'    |midsize_name   |character |Mid-size display name.                     |
#'    |is_conference  |logical   |Whether this group is a conference.        |
#'    |slug           |character |URL slug.                                  |
#'    |parent_ref     |character |`$ref` to parent group (if any).           |
#'    |children_ref   |character |`$ref` to child-groups endpoint.           |
#'    |teams_ref      |character |`$ref` to teams-in-group endpoint.         |
#'    |standings_ref  |character |`$ref` to standings endpoint.              |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_season_group(group_id = 5, season = 2025)
#' }
espn_mlb_season_group <- function(group_id,
                                   season = most_recent_mlb_season(),
                                   season_type = 2L, ...) {
  .espn_baseball_season_group(league = "mlb", season = season,
                                  season_type = season_type,
                                  group_id = group_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_season_group_children
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Season Group Children Index**
#' @name espn_mlb_season_group_children
#' @title
#' **Get ESPN MLB Season Group Children Index**
#' @rdname espn_mlb_season_group_children
#' @author Saiem Gilani
#' @description
#' Returns the list of child groups (e.g. divisions within a conference)
#' for one (MLB season x season-type x parent-group).
#'
#' @param group_id Parent group identifier.
#' @param season Season year. Defaults to most recent MLB season.
#' @param season_type Season-type id (2 = regular (default)).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per child group.
#'
#'    |col_name        |types     |description                            |
#'    |:---------------|:---------|:--------------------------------------|
#'    |league          |character |League slug.                           |
#'    |season          |integer   |Season year.                           |
#'    |season_type     |integer   |Season-type id.                        |
#'    |parent_group_id |character |Parent group id (queried).             |
#'    |child_group_id  |character |Child group id.                        |
#'    |ref             |character |`$ref` to child group detail.          |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_season_group_children(group_id = 5, season = 2025)
#' }
espn_mlb_season_group_children <- function(group_id,
                                            season = most_recent_mlb_season(),
                                            season_type = 2L, ...) {
  .espn_baseball_season_group_children(league = "mlb", season = season,
                                           season_type = season_type,
                                           group_id = group_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_season_group_teams
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Season Group Teams Index**
#' @name espn_mlb_season_group_teams
#' @title
#' **Get ESPN MLB Season Group Teams Index**
#' @rdname espn_mlb_season_group_teams
#' @author Saiem Gilani
#' @description
#' Returns the list of team IDs that belong to one group (conference or
#' division) for one (MLB season x season-type).
#'
#' @param group_id ESPN group identifier.
#' @param season Season year. Defaults to most recent MLB season.
#' @param season_type Season-type id (2 = regular (default)).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per team in the group.
#'
#'    |col_name    |types     |description                              |
#'    |:-----------|:---------|:----------------------------------------|
#'    |league      |character |League slug.                             |
#'    |season      |integer   |Season year.                             |
#'    |season_type |integer   |Season-type id.                          |
#'    |group_id    |character |ESPN group id.                           |
#'    |team_id     |character |ESPN team id.                            |
#'    |ref         |character |`$ref` URL to the team-in-season entry.  |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_season_group_teams(group_id = 5, season = 2025)
#' }
espn_mlb_season_group_teams <- function(group_id,
                                         season = most_recent_mlb_season(),
                                         season_type = 2L, ...) {
  .espn_baseball_season_group_teams(league = "mlb", season = season,
                                        season_type = season_type,
                                        group_id = group_id, ...)
}
