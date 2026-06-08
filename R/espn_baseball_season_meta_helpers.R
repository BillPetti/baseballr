# espn_baseball_season_meta_helpers.R
# Internal helpers for the ESPN MLB season-meta wrappers:
#   - season types (regular, postseason, ...)
#   - season-type leaders (PPG, RPG, ...)
#   - season rankings (AP / Coaches polls, mostly college)
# None of these are exported.

# ---------------------------------------------------------------------------
# .espn_baseball_season_types (index)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball season-types index
#' @noRd
.espn_baseball_season_types <- function(league, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Types Index"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/types?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      items <- raw[["items"]] %||% list()
      refs <- if (length(items) == 0L) character(0) else
        vapply(items, function(x) x[["$ref"]] %||% NA_character_,
               character(1))
      ids <- if (length(refs) == 0L) character(0) else
        sub(".*/types/([0-9]+).*", "\\1", refs)
      result <- data.frame(
        league      = rep(league, length(refs)),
        season      = rep(as.integer(season), length(refs)),
        season_type = suppressWarnings(as.integer(ids)),
        ref         = refs,
        stringsAsFactors = FALSE
      ) %>% dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Season Types Index"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} season types for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} season types",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_season_type (detail)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single season-type detail
#' @noRd
.espn_baseball_season_type <- function(league, season, season_type, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season, season_type = season_type)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Type Detail"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/types/", season_type,
    "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      row <- list(
        league        = league,
        season        = as.integer(season),
        season_type   = as.integer(raw[["id"]] %||% season_type),
        type          = as.integer(raw[["type"]] %||% season_type),
        name          = raw[["name"]] %||% NA_character_,
        abbreviation  = raw[["abbreviation"]] %||% NA_character_,
        year          = as.integer(raw[["year"]] %||% season),
        start_date    = raw[["startDate"]] %||% NA_character_,
        end_date      = raw[["endDate"]] %||% NA_character_,
        has_groups    = isTRUE(raw[["hasGroups"]]),
        has_standings = isTRUE(raw[["hasStandings"]]),
        has_legs      = isTRUE(raw[["hasLegs"]]),
        slug          = raw[["slug"]] %||% NA_character_,
        groups_ref    = if (is.list(raw[["groups"]])) raw[["groups"]][["$ref"]] %||% NA_character_ else NA_character_,
        weeks_ref     = if (is.list(raw[["weeks"]])) raw[["weeks"]][["$ref"]] %||% NA_character_ else NA_character_,
        leaders_ref   = if (is.list(raw[["leaders"]])) raw[["leaders"]][["$ref"]] %||% NA_character_ else NA_character_
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Season Type Detail"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} season type {season_type} for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} season type {season_type}",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_season_leaders (long format)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball season-type leaders (long format)
#'
#' Returns one row per (season_type x category x leader). Sample with
#' the default `season_type = c(2L, 3L)`: 14 categories x 25 leaders
#' x 2 season types = ~700 rows.
#'
#' @noRd
.espn_baseball_season_leaders <- function(league, season,
                                             season_type = c(2L, 3L), ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season, season_type = season_type)

  fetch_one <- function(st) {
    url <- paste0(
      "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
      league, "/seasons/", season, "/types/", st,
      "/leaders?lang=en&region=us"
    )
    res <- tryCatch(.retry_request(url), error = function(e) NULL)
    if (is.null(res) || httr2::resp_status(res) != 200L) return(list())
    raw <- res %>% .resp_text() %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
    categories <- raw[["categories"]] %||% list()
    rows <- list()
    for (cat in categories) {
      cat_name <- cat[["name"]] %||% NA_character_
      cat_display <- cat[["displayName"]] %||% NA_character_
      cat_short   <- cat[["shortDisplayName"]] %||% NA_character_
      cat_abbrev  <- cat[["abbreviation"]] %||% NA_character_
      leaders <- cat[["leaders"]] %||% list()
      for (i in seq_along(leaders)) {
        l <- leaders[[i]]
        a <- l[["athlete"]]
        t <- l[["team"]]
        aref <- if (is.list(a)) a[["$ref"]] %||% NA_character_ else NA_character_
        tref <- if (is.list(t)) t[["$ref"]] %||% NA_character_ else NA_character_
        aid <- if (!is.na(aref)) sub(".*/athletes/([0-9]+).*", "\\1", aref) else NA_character_
        tid <- if (!is.na(tref)) sub(".*/teams/([0-9]+).*", "\\1", tref) else NA_character_
        rows[[length(rows) + 1L]] <- list(
          league             = league,
          season             = as.integer(season),
          season_type        = as.integer(st),
          category_name      = cat_name,
          category_display   = cat_display,
          category_short     = cat_short,
          category_abbrev    = cat_abbrev,
          rank               = i,
          athlete_id         = aid,
          team_id            = tid,
          display_value      = as.character(l[["displayValue"]] %||% NA),
          value              = suppressWarnings(as.numeric(l[["value"]] %||% NA)),
          rel                = paste(unlist(l[["rel"]] %||% list()), collapse = ","),
          athlete_ref        = aref,
          team_ref           = tref
        )
      }
    }
    rows
  }

  result <- NULL
  tryCatch(
    expr = {
      all_rows <- list()
      for (st in season_type) {
        rs <- fetch_one(as.integer(st))
        if (length(rs) > 0L) all_rows <- c(all_rows, rs)
        if (length(season_type) > 1L) Sys.sleep(0.3)
      }
      if (length(all_rows) == 0L) {
        result <- data.frame(
          league = character(0), season = integer(0),
          season_type = integer(0), category_name = character(0),
          category_display = character(0), category_short = character(0),
          category_abbrev = character(0), rank = integer(0),
          athlete_id = character(0), team_id = character(0),
          display_value = character(0), value = numeric(0),
          rel = character(0), athlete_ref = character(0),
          team_ref = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season-Type Leaders"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(all_rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season-Type Leaders"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} season leaders for season={season}, season_type={season_type}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} season leaders",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_season_rankings (index)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball season-rankings index
#' @noRd
.espn_baseball_season_rankings <- function(league, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Rankings Index"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/rankings?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      items <- raw[["items"]] %||% list()
      refs <- if (length(items) == 0L) character(0) else
        vapply(items, function(x) x[["$ref"]] %||% NA_character_,
               character(1))
      ids <- if (length(refs) == 0L) character(0) else
        sub(".*/rankings/([0-9]+).*", "\\1", refs)
      result <- data.frame(
        league     = rep(league, length(refs)),
        season     = rep(as.integer(season), length(refs)),
        ranking_id = ids,
        ref        = refs,
        stringsAsFactors = FALSE
      ) %>% dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Season Rankings Index"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} season rankings for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} season rankings",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_season_ranking (detail with ranks)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single ranking-source snapshots index
#'
#' Returns one row per weekly snapshot for a ranking source (e.g. AP Top
#' 25). Each row's `ref` URL resolves to the actual ranked teams for that
#' week via `.espn_baseball_week_ranking()` (forthcoming).
#'
#' @noRd
.espn_baseball_season_ranking <- function(league, season, ranking_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season, ranking_id = ranking_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Ranking Snapshots Index"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/rankings/", ranking_id,
    "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      snapshots <- raw[["rankings"]] %||% list()
      refs <- if (length(snapshots) == 0L) character(0) else
        vapply(snapshots, function(x) x[["$ref"]] %||% NA_character_,
               character(1))
      # Parse season_type + week from each ref URL
      season_types <- if (length(refs) == 0L) integer(0) else
        suppressWarnings(as.integer(
          sub(".*/types/([0-9]+)/weeks/.*", "\\1", refs)
        ))
      weeks <- if (length(refs) == 0L) integer(0) else
        suppressWarnings(as.integer(
          sub(".*/weeks/([0-9]+)/.*", "\\1", refs)
        ))
      result <- data.frame(
        league      = rep(league, length(refs)),
        season      = rep(as.integer(season), length(refs)),
        ranking_id  = rep(as.character(ranking_id), length(refs)),
        name        = rep(raw[["name"]] %||% NA_character_, length(refs)),
        short_name  = rep(raw[["shortName"]] %||% NA_character_, length(refs)),
        type        = rep(raw[["type"]] %||% NA_character_, length(refs)),
        season_type = season_types,
        week        = weeks,
        ref         = refs,
        stringsAsFactors = FALSE
      ) %>% dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league),
                 " Season Ranking Snapshots Index"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} ranking {ranking_id} for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} ranking {ranking_id}",
      args = .args),
    finally = {}
  )
  result
}
