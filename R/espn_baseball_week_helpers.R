# espn_baseball_week_helpers.R
# Internal helpers for the ESPN MLB week / week-ranking wrappers.

# ---------------------------------------------------------------------------
# .espn_baseball_season_weeks (index)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball season-weeks index
#'
#' `season_type` accepts a scalar or vector; default `c(2L, 3L)` fetches
#' regular season + postseason weeks and binds them.
#'
#' @noRd
.espn_baseball_season_weeks <- function(league, season,
                                           season_type = c(2L, 3L), ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season, season_type = season_type)

  fetch_one <- function(st) {
    url <- paste0(
      "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
      league, "/seasons/", season, "/types/", st,
      "/weeks?limit=200&lang=en&region=us"
    )
    res <- tryCatch(.retry_request(url), error = function(e) NULL)
    if (is.null(res) || httr2::resp_status(res) != 200L) return(NULL)
    raw <- res %>% .resp_text() %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
    items <- raw[["items"]] %||% list()
    refs <- if (length(items) == 0L) character(0) else
      vapply(items, function(x) x[["$ref"]] %||% NA_character_,
             character(1))
    weeks <- if (length(refs) == 0L) integer(0) else
      suppressWarnings(as.integer(
        sub(".*/weeks/([0-9]+).*", "\\1", refs)
      ))
    data.frame(
      league      = rep(league, length(refs)),
      season      = rep(as.integer(season), length(refs)),
      season_type = rep(as.integer(st), length(refs)),
      week        = weeks,
      ref         = refs,
      stringsAsFactors = FALSE
    )
  }

  result <- NULL
  tryCatch(
    expr = {
      parts <- list()
      for (st in season_type) {
        df <- fetch_one(as.integer(st))
        if (!is.null(df) && nrow(df) > 0L) parts[[length(parts) + 1L]] <- df
        if (length(season_type) > 1L) Sys.sleep(0.3)
      }
      if (length(parts) == 0L) {
        result <- data.frame(
          league = character(0), season = integer(0),
          season_type = integer(0), week = integer(0),
          ref = character(0), stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season Weeks Index"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, parts) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season Weeks Index"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} weeks for season={season}, season_type={season_type}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} weeks",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_season_week (detail)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single-week metadata
#' @noRd
.espn_baseball_season_week <- function(league, season,
                                          season_type = 2L,
                                          week, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season,
                season_type = season_type, week = week)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Week Detail"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/types/", season_type,
    "/weeks/", week, "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      row <- list(
        league       = league,
        season       = as.integer(season),
        season_type  = as.integer(season_type),
        week         = as.integer(raw[["number"]] %||% week),
        text         = raw[["text"]] %||% NA_character_,
        start_date   = raw[["startDate"]] %||% NA_character_,
        end_date     = raw[["endDate"]] %||% NA_character_,
        rankings_ref = if (is.list(raw[["rankings"]])) raw[["rankings"]][["$ref"]] %||% NA_character_ else NA_character_
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Season Week Detail"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} week {week} for season={season}, season_type={season_type}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} week {week}",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_week_rankings (per-week ranking sources index)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball per-week rankings index
#' @noRd
.espn_baseball_week_rankings <- function(league, season,
                                            season_type = 2L,
                                            week, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season,
                season_type = season_type, week = week)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Week Rankings Index"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/types/", season_type,
    "/weeks/", week, "/rankings?lang=en&region=us"
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
        league      = rep(league, length(refs)),
        season      = rep(as.integer(season), length(refs)),
        season_type = rep(as.integer(season_type), length(refs)),
        week        = rep(as.integer(week), length(refs)),
        ranking_id  = ids,
        ref         = refs,
        stringsAsFactors = FALSE
      ) %>% dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Week Rankings Index"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} week-{week} rankings for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} week-{week} rankings",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_week_ranking (per-week ranked teams)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single ranking snapshot for one week
#'
#' Returns the long-format ranked teams for one (season x season-type x
#' week x ranking-source). Typically 25 rows (AP Top 25 / Coaches Poll).
#'
#' @noRd
.espn_baseball_week_ranking <- function(league, season,
                                           season_type = 2L,
                                           week, ranking_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season,
                season_type = season_type, week = week,
                ranking_id = ranking_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Week Ranking Detail"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/types/", season_type,
    "/weeks/", week, "/rankings/", ranking_id,
    "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      ranks <- raw[["ranks"]] %||% list()
      rows <- list()
      for (r in ranks) {
        tref <- if (is.list(r[["team"]]))
          r[["team"]][["$ref"]] %||% NA_character_ else NA_character_
        tid <- if (!is.na(tref))
          sub(".*/teams/([0-9]+).*", "\\1", tref) else NA_character_
        rec_summary <- if (is.list(r[["record"]]))
          r[["record"]][["summary"]] %||% NA_character_ else NA_character_
        rows[[length(rows) + 1L]] <- list(
          league             = league,
          season             = as.integer(season),
          season_type        = as.integer(season_type),
          week               = as.integer(week),
          ranking_id         = as.character(ranking_id),
          name               = raw[["name"]] %||% NA_character_,
          short_name         = raw[["shortName"]] %||% NA_character_,
          type               = raw[["type"]] %||% NA_character_,
          headline           = raw[["headline"]] %||% NA_character_,
          date               = raw[["date"]] %||% NA_character_,
          current            = suppressWarnings(as.integer(r[["current"]] %||% NA)),
          previous           = suppressWarnings(as.integer(r[["previous"]] %||% NA)),
          points             = suppressWarnings(as.numeric(r[["points"]] %||% NA)),
          first_place_votes  = suppressWarnings(as.integer(r[["firstPlaceVotes"]] %||% NA)),
          trend              = r[["trend"]] %||% NA_character_,
          record_summary     = rec_summary,
          team_id            = tid,
          team_ref           = tref,
          last_updated       = r[["lastUpdated"]] %||% NA_character_
        )
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), season = integer(0),
          season_type = integer(0), week = integer(0),
          ranking_id = character(0), name = character(0),
          short_name = character(0), type = character(0),
          headline = character(0), date = character(0),
          current = integer(0), previous = integer(0),
          points = numeric(0), first_place_votes = integer(0),
          trend = character(0), record_summary = character(0),
          team_id = character(0), team_ref = character(0),
          last_updated = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Week Ranking Detail"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Week Ranking Detail"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} week-{week} ranking {ranking_id} for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} week-{week} ranking {ranking_id}",
      args = .args),
    finally = {}
  )
  result
}
