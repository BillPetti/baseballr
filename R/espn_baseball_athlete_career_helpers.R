# espn_baseball_athlete_career_helpers.R
# Internal helpers for career-level athlete endpoints:
#   - athletes/{id}/seasons
#   - athletes/{id}/statistics(/{type})

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_seasons (career season list)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete-seasons index
#' @noRd
.espn_baseball_athlete_seasons <- function(league, athlete_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Athlete Seasons"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/athletes/", athlete_id,
    "/seasons?limit=200&lang=en&region=us"
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
      seasons <- if (length(refs) == 0L) integer(0) else
        suppressWarnings(as.integer(
          sub(".*/seasons/([0-9]+).*", "\\1", refs)
        ))
      result <- data.frame(
        league     = rep(league, length(refs)),
        athlete_id = rep(as.character(athlete_id), length(refs)),
        season     = seasons,
        ref        = refs,
        stringsAsFactors = FALSE
      ) %>% dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Seasons"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} athlete seasons for athlete_id={athlete_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} athlete seasons",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_career_stats (long format)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete career statistics (long format)
#'
#' Returns one row per (stat_type x category x stat) for an athlete's
#' career stats. By default fetches both regular-season (type 0) and
#' postseason (type 1) and binds them, exposed via a `stat_type_id`
#' column. Pass a single integer (0 / 1 / 2) to fetch one type only.
#'
#' Stat type codes: 0 = regular season (default endpoint), 1 = postseason,
#' 2 = career aggregate.
#'
#' @noRd
.espn_baseball_athlete_career_stats <- function(league, athlete_id,
                                                   stat_type = 0L,
                                                   ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id,
                stat_type = stat_type)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Athlete Career Stats"))

  fetch_one <- function(type_id) {
    type_segment <- if (is.null(type_id) || is.na(type_id)) "" else
      paste0("/", as.integer(type_id))
    url <- paste0(
      "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
      league, "/athletes/", athlete_id, "/statistics", type_segment,
      "?lang=en&region=us"
    )
    res <- tryCatch(.retry_request(url), error = function(e) NULL)
    if (is.null(res)) return(list())
    if (httr2::resp_status(res) != 200) return(list())
    raw <- res %>% .resp_text() %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
    splits <- raw[["splits"]]
    if (!is.list(splits)) return(list())
    categories <- splits[["categories"]] %||% list()
    split_id   <- splits[["id"]] %||% NA_character_
    split_name <- splits[["name"]] %||% NA_character_
    split_type <- splits[["type"]] %||% NA_character_
    rows <- list()
    for (cat in categories) {
      cat_name <- cat[["name"]] %||% NA_character_
      cat_disp <- cat[["displayName"]] %||% NA_character_
      cat_short <- cat[["shortDisplayName"]] %||% NA_character_
      cat_abbr <- cat[["abbreviation"]] %||% NA_character_
      stats <- cat[["stats"]] %||% list()
      for (s in stats) {
        rows[[length(rows) + 1L]] <- list(
          league         = league,
          athlete_id     = as.character(athlete_id),
          stat_type_id   = if (is.null(type_id) || is.na(type_id)) NA_character_ else as.character(type_id),
          split_id       = as.character(split_id),
          split_name     = split_name,
          split_type     = split_type,
          category_name  = cat_name,
          category_display = cat_disp,
          category_short = cat_short,
          category_abbrev = cat_abbr,
          stat_name      = s[["name"]] %||% NA_character_,
          stat_abbrev    = s[["abbreviation"]] %||% NA_character_,
          stat_display   = s[["displayName"]] %||% NA_character_,
          stat_short     = s[["shortDisplayName"]] %||% NA_character_,
          description    = s[["description"]] %||% NA_character_,
          value          = suppressWarnings(as.numeric(s[["value"]] %||% NA)),
          display_value  = as.character(s[["displayValue"]] %||% NA)
        )
      }
    }
    rows
  }

  tryCatch(
    expr = {
      all_rows <- list()
      types_to_fetch <- if (length(stat_type) == 0L) list(NULL) else as.list(stat_type)
      for (t in types_to_fetch) {
        rs <- fetch_one(t)
        if (length(rs) > 0L) all_rows <- c(all_rows, rs)
        if (length(types_to_fetch) > 1L) Sys.sleep(0.3)
      }
      if (length(all_rows) == 0L) {
        result <- data.frame(
          league = character(0), athlete_id = character(0),
          stat_type_id = character(0), split_id = character(0),
          split_name = character(0), split_type = character(0),
          category_name = character(0), category_display = character(0),
          category_short = character(0), category_abbrev = character(0),
          stat_name = character(0), stat_abbrev = character(0),
          stat_display = character(0), stat_short = character(0),
          description = character(0), value = numeric(0),
          display_value = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Athlete Career Stats"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(all_rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Athlete Career Stats"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} athlete career stats for athlete_id={athlete_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} athlete career stats",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_eventlog_v2 (per-season per-game event log)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball per-season athlete event log
#'
#' Fetches `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/seasons/{season}/athletes/{athlete_id}/eventlog`
#' and returns one row per (event x team) appearance. Distinct from the
#' web-common-v3 `athlete_eventlog` wrapper: that one returns gamelog
#' rows with stats; this core-v2 endpoint returns event refs and the
#' `played` flag (whether the athlete was active for that game).
#'
#' @noRd
.espn_baseball_athlete_eventlog_v2 <- function(league, athlete_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Athlete Event Log"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/athletes/", athlete_id,
    "/eventlog?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      events <- raw[["events"]]
      items <- if (is.list(events)) events[["items"]] %||% list() else list()
      rows <- list()
      for (it in items) {
        eref <- if (is.list(it[["event"]]))
          it[["event"]][["$ref"]] %||% NA_character_ else NA_character_
        cref <- if (is.list(it[["competition"]]))
          it[["competition"]][["$ref"]] %||% NA_character_ else NA_character_
        eid <- if (!is.na(eref))
          sub(".*/events/([0-9]+).*", "\\1", eref) else NA_character_
        rows[[length(rows) + 1L]] <- list(
          league          = league,
          athlete_id      = as.character(athlete_id),
          season          = as.integer(season),
          event_id        = eid,
          team_id         = as.character(it[["teamId"]] %||% NA),
          played          = isTRUE(it[["played"]]),
          event_ref       = eref,
          competition_ref = cref
        )
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), athlete_id = character(0),
          season = integer(0), event_id = character(0),
          team_id = character(0), played = logical(0),
          event_ref = character(0), competition_ref = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Athlete Event Log"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Athlete Event Log"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} athlete event log for athlete_id={athlete_id}, season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} athlete event log",
      args = .args),
    finally = {}
  )
  result
}
