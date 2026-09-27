# Loaders for the season-by-season conference / division reference tables
# published by sportsdataverse/sdv-reference-data to the sportsdataverse-data
# release tags mlb_groups and ncaa_baseball_groups. Schema: that repo's
# CONTRACT.md. The tags carry parquet + csv; the csv is read with the
# contract's column classes (ids stay character, "150" not 150; an empty field
# is a null) so no parquet dependency is needed.

# Column classes of the four {league}_groups tables, per CONTRACT.md.
.groups_col_classes <- list(
  groups = c(
    league = "character", group_id = "character", level = "character",
    first_season = "integer", last_season = "integer", notes = "character"
  ),
  group_seasons = c(
    league = "character", group_id = "character", season = "integer",
    level = "character", name = "character", short_name = "character",
    abbreviation = "character", parent_group_id = "character",
    n_teams = "integer"
  ),
  group_aliases = c(
    league = "character", group_id = "character", source = "character",
    source_id = "character", name_kind = "character", value = "character",
    valid_from = "integer", valid_to = "integer"
  ),
  team_group_seasons = c(
    league = "character", season = "integer", team_id = "character",
    team_id_source = "character", team_name = "character",
    subdivision_id = "character", conference_id = "character",
    division_id = "character", source = "character",
    sources_agree = "logical", notes = "character"
  )
)

# Internal worker: validates seasons (per-season tables only), builds the
# release URLs, reads each csv with the contract column classes, optionally
# writes into a DB, and tags the result baseballr_data. A file that fails to
# download warns and contributes a zero-row frame carrying the contract schema.
#' @keywords internal
#' @noRd
.groups_release_loader <- function(league, table, description,
                                   seasons = NULL, min_season = NULL,
                                   max_season = NULL,
                                   dbConnection = NULL, tablename = NULL, ...) {
  in_db <- !is.null(dbConnection) && !is.null(tablename)
  cols <- .groups_col_classes[[table]]

  # seasons = TRUE reads the release's all-seasons file
  file_stem <- paste0(league, "_", table)
  if (!is.null(min_season) && !isTRUE(seasons)) {
    stopifnot(is.numeric(seasons),
              all(seasons >= min_season),
              all(seasons <= max_season),
              all(seasons == trunc(seasons)))
    file_stem <- paste0(file_stem, "_", seasons)
  }
  urls <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/releases/download/",
    league, "_groups/", file_stem, ".csv"
  )

  read_one <- function(url) {
    out <- data.table::as.data.table(lapply(cols, vector, length = 0))
    tryCatch(
      expr = {
        out <- csv_from_url(url, colClasses = cols, na.strings = "",
                            encoding = "UTF-8", showProgress = FALSE)
      },
      error = function(e) {
        cli::cli_warn("Failed to read {.url {url}}: {conditionMessage(e)}")
      }
    )
    out
  }

  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = urls)
  out <- lapply(urls, progressively(read_one, p))
  out <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  if (in_db) {
    DBI::dbWriteTable(dbConnection, tablename, out, append = TRUE, ...)
    return(invisible(NULL))
  }
  out |>
    make_baseballr_data(description, Sys.time())
}

#' @title
#' **Load MLB groups (leagues and divisions) from the SportsDataverse data repo**
#' @rdname load_mlb_groups
#' @description One row per MLB group lineage (the league, the American and
#'   National Leagues, and their divisions), keyed by the SportsDataverse
#'   group id. Published to the `mlb_groups` release tag on the
#'   [sportsdataverse-data releases](https://github.com/sportsdataverse/sportsdataverse-data/releases)
#'   by [sdv-reference-data](https://github.com/sportsdataverse/sdv-reference-data).
#'   Season keys are the single calendar year.
#' @param ... Additional arguments passed to an underlying function that
#'   writes the data into a database.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A `baseballr_data` tibble:
#'
#'    |col_name     |types     |description                                                                 |
#'    |:------------|:---------|:---------------------------------------------------------------------------|
#'    |league       |character |League key.                                                                 |
#'    |group_id     |character |SportsDataverse group id, `{league}:{slug}`; one id per lineage across renames. |
#'    |level        |character |Group level: `league`, `subdivision`, `conference` or `division`.          |
#'    |first_season |integer   |First season with at least one member.                                      |
#'    |last_season  |integer   |Last season with at least one member.                                       |
#'    |notes        |character |Lineage decisions and source caveats.                                       |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_mlb_groups())
#' }
load_mlb_groups <- function(..., dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("mlb", "groups",
    "MLB groups from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename, ...)
}

#' @title
#' **Load MLB group names and parents by season from the SportsDataverse data repo**
#' @rdname load_mlb_group_seasons
#' @description One row per MLB group per season it existed, with the name,
#'   abbreviation and parent group **as of that season** (not today's).
#'   Published to the `mlb_groups` release tag on the sportsdataverse-data
#'   releases. Season keys are the single calendar year.
#' @inheritParams load_mlb_groups
#' @return A `baseballr_data` tibble:
#'
#'    |col_name        |types     |description                                                                  |
#'    |:---------------|:---------|:----------------------------------------------------------------------------|
#'    |league          |character |League key.                                                                  |
#'    |group_id        |character |SportsDataverse group id, `{league}:{slug}`.                                 |
#'    |season          |integer   |Season.                                                                      |
#'    |level           |character |Group level: `league`, `subdivision`, `conference` or `division`.           |
#'    |name            |character |Group name as of that season.                                                |
#'    |short_name      |character |Short name as of that season.                                                |
#'    |abbreviation    |character |Abbreviation as of that season.                                              |
#'    |parent_group_id |character |Parent group id as of that season (division, conference, subdivision, league). |
#'    |n_teams         |integer   |Member teams that season.                                                    |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_mlb_group_seasons())
#' }
load_mlb_group_seasons <- function(..., dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("mlb", "group_seasons",
    "MLB group seasons from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename, ...)
}

#' @title
#' **Load MLB group aliases from the SportsDataverse data repo**
#' @rdname load_mlb_group_aliases
#' @description Every name and id a source uses for an MLB group, with the
#'   seasons it is valid for -- the crosswalk from ESPN / MLB Stats API
#'   league and division ids and names to SportsDataverse group ids.
#'   Published to the `mlb_groups` release tag on the sportsdataverse-data
#'   releases.
#' @inheritParams load_mlb_groups
#' @return A `baseballr_data` tibble:
#'
#'    |col_name   |types     |description                                                                     |
#'    |:----------|:---------|:-------------------------------------------------------------------------------|
#'    |league     |character |League key.                                                                     |
#'    |group_id   |character |SportsDataverse group id, `{league}:{slug}`.                                    |
#'    |source     |character |Source that uses the alias (e.g. `espn`, `mlb`, `ncaa`, `sdv`).                 |
#'    |source_id  |character |The source's own id for the group, when it has one.                             |
#'    |name_kind  |character |Alias kind: `name`, `short_name`, `abbreviation`, `slug` or `code`.            |
#'    |value      |character |The alias.                                                                      |
#'    |valid_from |integer   |First season the alias is valid (inclusive); `NA` = unbounded.                  |
#'    |valid_to   |integer   |Last season the alias is valid (inclusive); `NA` = unbounded.                   |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_mlb_group_aliases())
#' }
load_mlb_group_aliases <- function(..., dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("mlb", "group_aliases",
    "MLB group aliases from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename, ...)
}

#' @title
#' **Load MLB team league and division memberships by season from the SportsDataverse data repo**
#' @rdname load_mlb_team_group_seasons
#' @description One row per MLB team per season, with the league
#'   (`conference_id`) and division it played in that season, e.g. the Houston
#'   Astros in `mlb:nl-central` for 2012 and `mlb:al-west` from 2013.
#'   Published to the `mlb_groups` release tag on the sportsdataverse-data
#'   releases, one file per season.
#' @param seasons A vector of 4-digit seasons, or `TRUE` for every published
#'   season. (Min: 1901)
#' @inheritParams load_mlb_groups
#' @return A `baseballr_data` tibble:
#'
#'    |col_name       |types     |description                                                                          |
#'    |:--------------|:---------|:------------------------------------------------------------------------------------|
#'    |league         |character |League key.                                                                          |
#'    |season         |integer   |Season.                                                                              |
#'    |team_id        |character |ESPN team id where ESPN covers the team, otherwise the league's own id.              |
#'    |team_id_source |character |Id system of `team_id` (e.g. `espn`, `mlb`, `ncaa_org`).                             |
#'    |team_name      |character |Team name as of that season.                                                         |
#'    |subdivision_id |character |SportsDataverse subdivision group id; `NA` where the level does not apply.            |
#'    |conference_id  |character |SportsDataverse conference group id (for MLB, the American or National League).      |
#'    |division_id    |character |SportsDataverse division group id; `NA` where the level does not apply.              |
#'    |source         |character |Source the membership came from.                                                     |
#'    |sources_agree  |logical   |Whether a second source agrees; `NA` when only one source covers the season.         |
#'    |notes          |character |Notes, e.g. the league's own team id.                                                |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_mlb_team_group_seasons(seasons = 2013))
#' }
load_mlb_team_group_seasons <- function(seasons = most_recent_mlb_season(), ...,
                                        dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("mlb", "team_group_seasons",
    "MLB team group seasons from the SportsDataverse data repo",
    seasons = seasons, min_season = 1901,
    max_season = most_recent_mlb_season(),
    dbConnection = dbConnection, tablename = tablename, ...)
}

#' @title
#' **Load NCAA baseball groups (divisions and conferences) from the SportsDataverse data repo**
#' @rdname load_ncaa_baseball_groups
#' @description One row per NCAA baseball group lineage (divisions I-III and
#'   their conferences), keyed by the SportsDataverse group id. Published to
#'   the `ncaa_baseball_groups` release tag on the
#'   [sportsdataverse-data releases](https://github.com/sportsdataverse/sportsdataverse-data/releases)
#'   by [sdv-reference-data](https://github.com/sportsdataverse/sdv-reference-data).
#'   Season keys are the spring year (2026 = the spring 2026 season).
#' @inheritParams load_mlb_groups
#' @inherit load_mlb_groups return
#' @export
#' @examples
#' \donttest{
#'   try(load_ncaa_baseball_groups())
#' }
load_ncaa_baseball_groups <- function(..., dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("ncaa_baseball", "groups",
    "NCAA baseball groups from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename, ...)
}

#' @title
#' **Load NCAA baseball group names and parents by season from the SportsDataverse data repo**
#' @rdname load_ncaa_baseball_group_seasons
#' @description One row per NCAA baseball group per season it existed, with
#'   the name, abbreviation and parent group **as of that season** (not
#'   today's). Published to the `ncaa_baseball_groups` release tag on the
#'   sportsdataverse-data releases. Season keys are the spring year.
#' @inheritParams load_mlb_groups
#' @inherit load_mlb_group_seasons return
#' @export
#' @examples
#' \donttest{
#'   try(load_ncaa_baseball_group_seasons())
#' }
load_ncaa_baseball_group_seasons <- function(..., dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("ncaa_baseball", "group_seasons",
    "NCAA baseball group seasons from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename, ...)
}

#' @title
#' **Load NCAA baseball group aliases from the SportsDataverse data repo**
#' @rdname load_ncaa_baseball_group_aliases
#' @description Every name and id a source uses for an NCAA baseball group,
#'   with the seasons it is valid for -- the crosswalk from stats.ncaa.org
#'   conference ids and names to SportsDataverse group ids. Published to the
#'   `ncaa_baseball_groups` release tag on the sportsdataverse-data releases.
#' @inheritParams load_mlb_groups
#' @inherit load_mlb_group_aliases return
#' @export
#' @examples
#' \donttest{
#'   try(load_ncaa_baseball_group_aliases())
#' }
load_ncaa_baseball_group_aliases <- function(..., dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("ncaa_baseball", "group_aliases",
    "NCAA baseball group aliases from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename, ...)
}

#' @title
#' **Load NCAA baseball team division and conference memberships by season from the SportsDataverse data repo**
#' @rdname load_ncaa_baseball_team_group_seasons
#' @description One row per NCAA baseball team per season, with the division
#'   (`subdivision_id`) and conference it played in that season. `team_id` is
#'   the stats.ncaa.org org id. Published to the `ncaa_baseball_groups`
#'   release tag on the sportsdataverse-data releases, one file per season.
#' @param seasons A vector of 4-digit seasons (the spring year), or `TRUE`
#'   for every published season. (Min: 2010)
#' @inheritParams load_mlb_groups
#' @inherit load_mlb_team_group_seasons return
#' @export
#' @examples
#' \donttest{
#'   try(load_ncaa_baseball_team_group_seasons(seasons = 2025))
#' }
load_ncaa_baseball_team_group_seasons <- function(seasons = most_recent_ncaa_baseball_season(), ...,
                                                  dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("ncaa_baseball", "team_group_seasons",
    "NCAA baseball team group seasons from the SportsDataverse data repo",
    seasons = seasons, min_season = 2010,
    max_season = most_recent_ncaa_baseball_season(),
    dbConnection = dbConnection, tablename = tablename, ...)
}
