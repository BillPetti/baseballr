# espn_baseball_team_deep_helpers.R
# Internal helpers for the deeper per-team / per-coach core-v2 endpoints:
#   - team odds-records      (sparse; MLB)
#   - team depth chart       (MLB only)
#   - team season-roster     (all 4 leagues, paginated $refs)
#   - coach in season        (single-coach-in-season detail)

# ---------------------------------------------------------------------------
# .espn_baseball_team_odds_records (long format)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball team odds-records (long format)
#'
#' Returns one row per (category x stat) for a team's odds-related records
#' in a (season x season-type). ESPN's `odds-records` is sparse — many
#' (team x season-type) combinations return 404, so the wrapper returns
#' an empty tibble in that case.
#'
#' @noRd
.espn_baseball_team_odds_records <- function(league, team_id,
                                                season,
                                                season_type = 0L, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, team_id = team_id, season = season,
                season_type = season_type)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Team Odds-Records"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/types/", season_type,
    "/teams/", team_id, "/odds-records?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      items <- raw[["items"]] %||% list()
      rows <- list()
      for (it in items) {
        cat_abbr <- it[["abbreviation"]] %||% NA_character_
        cat_disp <- it[["displayName"]] %||% NA_character_
        cat_short <- it[["shortDisplayName"]] %||% NA_character_
        cat_type <- it[["type"]] %||% NA_character_
        stats <- it[["stats"]] %||% list()
        for (s in stats) {
          rows[[length(rows) + 1L]] <- list(
            league          = league,
            team_id         = as.character(team_id),
            season          = as.integer(season),
            season_type     = as.integer(season_type),
            category_type   = cat_type,
            category_abbrev = cat_abbr,
            category_short  = cat_short,
            category_display = cat_disp,
            stat_type       = s[["type"]] %||% NA_character_,
            stat_abbrev     = s[["abbreviation"]] %||% NA_character_,
            stat_display    = s[["displayName"]] %||% NA_character_,
            value           = suppressWarnings(as.numeric(s[["value"]] %||% NA)),
            display_value   = as.character(s[["displayValue"]] %||% NA)
          )
        }
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), team_id = character(0),
          season = integer(0), season_type = integer(0),
          category_type = character(0), category_abbrev = character(0),
          category_short = character(0), category_display = character(0),
          stat_type = character(0), stat_abbrev = character(0),
          stat_display = character(0), value = numeric(0),
          display_value = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Odds-Records"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Odds-Records"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} team odds-records for team_id={team_id}, season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} team odds-records",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_team_depthchart (long format)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball team depth chart (long format)
#'
#' Returns one row per (position x rank x athlete). MLB-only at ESPN.
#' Schema: `items[1].positions[5].athletes[N]` where each position is
#' PG / SG / SF / PF / C, and each `athletes[i]` has `{athlete $ref, rank}`.
#'
#' @noRd
.espn_baseball_team_depthchart <- function(league, team_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, team_id = team_id, season = season)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Team Depth Chart"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/teams/", team_id,
    "/depthcharts?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      items <- raw[["items"]] %||% list()
      rows <- list()
      for (dc in items) {
        dc_id <- as.character(dc[["id"]] %||% NA)
        dc_name <- dc[["name"]] %||% NA_character_
        positions <- dc[["positions"]] %||% list()
        for (pos_key in names(positions)) {
          p <- positions[[pos_key]]
          pos_ref <- if (is.list(p[["position"]]))
            p[["position"]][["$ref"]] %||% NA_character_ else NA_character_
          athletes <- p[["athletes"]] %||% list()
          for (a in athletes) {
            aref <- if (is.list(a[["athlete"]]))
              a[["athlete"]][["$ref"]] %||% NA_character_ else NA_character_
            aid <- if (!is.na(aref))
              sub(".*/athletes/([0-9]+).*", "\\1", aref) else NA_character_
            rows[[length(rows) + 1L]] <- list(
              league         = league,
              team_id        = as.character(team_id),
              season         = as.integer(season),
              depthchart_id  = dc_id,
              depthchart_name = dc_name,
              position       = pos_key,
              rank           = suppressWarnings(as.integer(a[["rank"]] %||% NA)),
              athlete_id     = aid,
              athlete_ref    = aref,
              position_ref   = pos_ref
            )
          }
        }
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), team_id = character(0),
          season = integer(0), depthchart_id = character(0),
          depthchart_name = character(0), position = character(0),
          rank = integer(0), athlete_id = character(0),
          athlete_ref = character(0), position_ref = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Depth Chart"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Depth Chart"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} team depth chart for team_id={team_id}, season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} team depth chart",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_team_season_roster (per-season roster, paginated)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball per-season team roster (core-v2)
#'
#' Distinct from the site-v2-backed `espn_*_team_roster()` — this is the
#' core-v2 endpoint `/seasons/{y}/teams/{id}/athletes`, which returns the
#' authoritative per-season athlete list as `$ref` URLs. Useful for
#' era-correct rosters across all 4 leagues.
#'
#' @noRd
.espn_baseball_team_season_roster <- function(league, team_id,
                                                 season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, team_id = team_id, season = season)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Team Season Roster"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/teams/", team_id,
    "/athletes?limit=200&lang=en&region=us"
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
        sub(".*/athletes/([0-9]+).*", "\\1", refs)
      result <- data.frame(
        league     = rep(league, length(refs)),
        team_id    = rep(as.character(team_id), length(refs)),
        season     = rep(as.integer(season), length(refs)),
        athlete_id = ids,
        ref        = refs,
        stringsAsFactors = FALSE
      ) %>% dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Team Season Roster"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} team season roster for team_id={team_id}, season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} team season roster",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_coach_season (single coach in one season)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball coach-in-season detail
#'
#' Single coach for one specific season. Returns name, birth info, plus
#' `$ref`s to college, person, team, and records. ESPN's coverage is
#' sparse: many `(coach_id x season)` combinations return 404.
#'
#' @noRd
.espn_baseball_coach_season <- function(league, coach_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, coach_id = coach_id, season = season)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Coach-in-Season Detail"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/coaches/", coach_id,
    "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      bp <- raw[["birthPlace"]]
      birth_city  <- if (is.list(bp)) bp[["city"]]  %||% NA_character_ else NA_character_
      birth_state <- if (is.list(bp)) bp[["state"]] %||% NA_character_ else NA_character_
      ref_key <- function(k) {
        v <- raw[[k]]
        if (is.list(v)) v[["$ref"]] %||% NA_character_ else NA_character_
      }
      row <- list(
        league        = league,
        season        = as.integer(season),
        coach_id      = as.character(raw[["id"]] %||% coach_id),
        uid           = raw[["uid"]] %||% NA_character_,
        first_name    = raw[["firstName"]] %||% NA_character_,
        last_name     = raw[["lastName"]] %||% NA_character_,
        date_of_birth = raw[["dateOfBirth"]] %||% NA_character_,
        birth_city    = birth_city,
        birth_state   = birth_state,
        n_records     = length(raw[["records"]] %||% list()),
        person_ref    = ref_key("person"),
        college_ref   = ref_key("college"),
        team_ref      = ref_key("team")
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Coach-in-Season Detail"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} coach-in-season detail for coach_id={coach_id}, season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} coach-in-season detail",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_team_season_statistics
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball team-season statistics (long format with rank)
#'
#' Wraps `seasons/{y}/types/{t}/teams/{tid}/statistics`. One row per
#' (category x stat) for the team's season-type aggregate stats. Each
#' row also carries the team's league-rank for that stat where ESPN
#' provides it.
#'
#' @noRd
.espn_baseball_team_season_statistics <- function(league, team_id, season,
                                                      season_type = 2L, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, team_id = team_id, season = season,
                season_type = season_type)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Team Season Statistics"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season,
    "/types/", as.integer(season_type),
    "/teams/", team_id, "/statistics?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      splits <- raw[["splits"]]
      cats <- if (is.list(splits)) splits[["categories"]] %||% list() else list()
      rows <- list()
      for (cat in cats) {
        cat_name <- cat[["name"]] %||% NA_character_
        cat_disp <- cat[["displayName"]] %||% NA_character_
        stats <- cat[["stats"]] %||% list()
        for (s in stats) {
          rows[[length(rows) + 1L]] <- list(
            league             = league,
            season             = as.integer(season),
            season_type        = as.integer(season_type),
            team_id            = as.character(team_id),
            category_name      = cat_name,
            category_display   = cat_disp,
            stat_name          = s[["name"]] %||% NA_character_,
            stat_abbrev        = s[["abbreviation"]] %||% NA_character_,
            stat_display       = s[["displayName"]] %||% NA_character_,
            value              = suppressWarnings(as.numeric(s[["value"]] %||% NA)),
            display_value      = as.character(s[["displayValue"]] %||% NA_character_),
            rank               = suppressWarnings(as.integer(s[["rank"]] %||% NA)),
            rank_display_value = as.character(s[["rankDisplayValue"]] %||% NA_character_)
          )
        }
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), season = integer(0),
          season_type = integer(0), team_id = character(0),
          category_name = character(0), category_display = character(0),
          stat_name = character(0), stat_abbrev = character(0),
          stat_display = character(0), value = numeric(0),
          display_value = character(0), rank = integer(0),
          rank_display_value = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Season Statistics"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Season Statistics"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} team season statistics for team_id={team_id}, season={season}, season_type={season_type}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} team season statistics",
      args = .args),
    finally = {}
  )
  result
}
