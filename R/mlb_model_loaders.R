# Loaders for the pre-computed MLB model datasets published on the
# sportsdataverse-data GitHub releases (tags mlb_hitting_models,
# mlb_pitching_models, mlb_fielding_models, mlb_game_state). One loader per
# dataset stem, mirroring sportsdataverse-py's load_mlb_* surface; all share
# .mlb_model_release_loader().

# Internal worker: validates seasons, builds release URLs for
# {stem}_{season}.rds under the given tag, downloads (optional progressr),
# optionally writes into a DB, and tags the result baseballr_data.
#' @keywords internal
#' @noRd
.mlb_model_release_loader <- function(seasons, release_tag, file_prefix,
                                      min_season, description,
                                      dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)

  in_db <- !is.null(dbConnection) && !is.null(tablename)

  if (isTRUE(seasons)) seasons <- min_season:most_recent_mlb_season()

  stopifnot(is.numeric(seasons),
            all(seasons >= min_season),
            all(seasons <= most_recent_mlb_season()))

  urls <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/releases/download/",
    release_tag, "/", file_prefix, "_", seasons, ".rds"
  )

  p <- NULL
  if (requireNamespace("progressr", quietly = TRUE)) {
    p <- progressr::progressor(along = seasons)
  }
  out <- lapply(urls, function(u) {
    res <- rds_from_url(u)
    if (!is.null(p)) p()
    res
  })
  out <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  if (in_db) {
    DBI::dbWriteTable(dbConnection, tablename, out, append = TRUE)
    return(invisible(NULL))
  }
  out |>
    make_baseballr_data(description, Sys.time())
}

#' @name mlb_model_loaders
#' @title
#' **Load pre-computed MLB model datasets from the SportsDataverse data repo**
#' @description Season-level loaders for the modeled MLB datasets published on
#'   the [sportsdataverse-data releases](https://github.com/sportsdataverse/sportsdataverse-data/releases),
#'   mirroring sportsdataverse-py's `load_mlb_*` loader surface:
#'
#' * `load_mlb_expected_stats()` / `load_mlb_expected_hr()` /
#'   `load_mlb_batter_projection()` -- hitting models (xBA/xwOBA-style
#'   expected stats, expected home runs, and blended batter projections).
#' * `load_mlb_stuff_plus()` / `load_mlb_command_plus()` / `load_mlb_xera()`
#'   -- pitching models (Stuff+, Command+, and expected ERA).
#' * `load_mlb_oaa()` / `load_mlb_catcher_framing()` -- fielding models
#'   (outs above average and catcher framing runs).
#' * `load_mlb_re24_matrix()` / `load_mlb_we_table()` / `load_mlb_wpa()` --
#'   game-state references (run-expectancy matrix, win-expectancy table, and
#'   per-play win probability added).
#'
#' Coverage starts in 2015 (2016 for `load_mlb_batter_projection()`) and runs
#' through the most recent season.
#' @param seasons A vector of 4-digit seasons, or `TRUE` for every published
#'   season.
#' @param ... Additional arguments passed to the underlying database write.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A `baseballr_data` tibble (one row per player-season, or per
#'   game-state cell for the game-state references).
#' @examples
#' \donttest{
#'   try(load_mlb_expected_stats(2024))
#' }
NULL

#' @rdname mlb_model_loaders
#' @export
load_mlb_expected_stats <- function(seasons = most_recent_mlb_season(), ...,
                                    dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_hitting_models", "mlb_expected_stats", 2015,
    "MLB expected stats data from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @rdname mlb_model_loaders
#' @export
load_mlb_expected_hr <- function(seasons = most_recent_mlb_season(), ...,
                                 dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_hitting_models", "mlb_expected_hr", 2015,
    "MLB expected home runs data from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @rdname mlb_model_loaders
#' @export
load_mlb_batter_projection <- function(seasons = most_recent_mlb_season(), ...,
                                       dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_hitting_models", "mlb_batter_projection", 2016,
    "MLB batter projection data from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @rdname mlb_model_loaders
#' @export
load_mlb_stuff_plus <- function(seasons = most_recent_mlb_season(), ...,
                                dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_pitching_models", "mlb_stuff_plus", 2015,
    "MLB Stuff+ data from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @rdname mlb_model_loaders
#' @export
load_mlb_command_plus <- function(seasons = most_recent_mlb_season(), ...,
                                  dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_pitching_models", "mlb_command_plus", 2015,
    "MLB Command+ data from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @rdname mlb_model_loaders
#' @export
load_mlb_xera <- function(seasons = most_recent_mlb_season(), ...,
                          dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_pitching_models", "mlb_xera", 2015,
    "MLB expected ERA data from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @rdname mlb_model_loaders
#' @export
load_mlb_oaa <- function(seasons = most_recent_mlb_season(), ...,
                         dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_fielding_models", "mlb_oaa", 2015,
    "MLB outs above average data from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @rdname mlb_model_loaders
#' @export
load_mlb_catcher_framing <- function(seasons = most_recent_mlb_season(), ...,
                                     dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_fielding_models", "mlb_catcher_framing", 2015,
    "MLB catcher framing data from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @rdname mlb_model_loaders
#' @export
load_mlb_re24_matrix <- function(seasons = most_recent_mlb_season(), ...,
                                 dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_game_state", "mlb_re24_matrix", 2015,
    "MLB run expectancy matrix from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @rdname mlb_model_loaders
#' @export
load_mlb_we_table <- function(seasons = most_recent_mlb_season(), ...,
                              dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_game_state", "mlb_we_table", 2015,
    "MLB win expectancy table from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @rdname mlb_model_loaders
#' @export
load_mlb_wpa <- function(seasons = most_recent_mlb_season(), ...,
                         dbConnection = NULL, tablename = NULL) {
  .mlb_model_release_loader(seasons, "mlb_game_state", "mlb_wpa", 2015,
    "MLB win probability added data from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}
