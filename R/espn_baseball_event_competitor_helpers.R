# espn_baseball_event_competitor_helpers.R
# Internal helpers shared by the per-event competitor wrappers
# (linescores, leaders, roster, statistics, records, roster entry,
# score, per-game player box). None of these are exported.

# ---------------------------------------------------------------------------
# .espn_baseball_event_competitor_linescores
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball competitor linescores (per-inning)
#' @noRd
.espn_baseball_event_competitor_linescores <- function(league, event_id,
                                                          team_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id, team_id = team_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Competitor Linescores"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/events/", event_id, "/competitions/", event_id,
    "/competitors/", team_id, "/linescores?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      items <- raw[["items"]] %||% list()
      rows <- list()
      for (it in items) {
        rows[[length(rows) + 1L]] <- list(
          league        = league,
          event_id      = as.character(event_id),
          team_id       = as.character(team_id),
          period        = as.integer(it[["period"]] %||% NA),
          value         = suppressWarnings(as.numeric(it[["value"]] %||% NA)),
          display_value = as.character(it[["displayValue"]] %||% NA),
          source        = as.character(it[["source"]] %||% NA)
        )
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), event_id = character(0),
          team_id = character(0), period = integer(0),
          value = numeric(0), display_value = character(0),
          source = character(0), stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Competitor Linescores"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Competitor Linescores"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} competitor linescores for event_id={event_id}, team_id={team_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} competitor linescores",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_event_competitor_leaders
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball competitor leaders (per-game top performers)
#' @noRd
.espn_baseball_event_competitor_leaders <- function(league, event_id,
                                                       team_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id, team_id = team_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Competitor Leaders"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/events/", event_id, "/competitions/", event_id,
    "/competitors/", team_id, "/leaders?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      cats <- raw[["categories"]] %||% list()
      rows <- list()
      for (cat in cats) {
        cat_name <- cat[["name"]] %||% NA_character_
        cat_disp <- cat[["displayName"]] %||% NA_character_
        cat_abbr <- cat[["abbreviation"]] %||% NA_character_
        leaders <- cat[["leaders"]] %||% list()
        for (i in seq_along(leaders)) {
          l <- leaders[[i]]
          a <- l[["athlete"]]
          aref <- if (is.list(a)) a[["$ref"]] %||% NA_character_ else NA_character_
          aid <- if (!is.na(aref)) sub(".*/athletes/([0-9]+).*", "\\1", aref) else NA_character_
          rows[[length(rows) + 1L]] <- list(
            league           = league,
            event_id         = as.character(event_id),
            team_id          = as.character(team_id),
            category_name    = cat_name,
            category_display = cat_disp,
            category_abbrev  = cat_abbr,
            rank             = i,
            athlete_id       = aid,
            display_value    = as.character(l[["displayValue"]] %||% NA),
            value            = suppressWarnings(as.numeric(l[["value"]] %||% NA)),
            athlete_ref      = aref
          )
        }
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), event_id = character(0),
          team_id = character(0), category_name = character(0),
          category_display = character(0), category_abbrev = character(0),
          rank = integer(0), athlete_id = character(0),
          display_value = character(0), value = numeric(0),
          athlete_ref = character(0), stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Competitor Leaders"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Competitor Leaders"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} competitor leaders for event_id={event_id}, team_id={team_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} competitor leaders",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_event_competitor_roster (paginated $refs)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball competitor game-day roster index
#' @noRd
.espn_baseball_event_competitor_roster <- function(league, event_id,
                                                      team_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id, team_id = team_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Competitor Roster"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/events/", event_id, "/competitions/", event_id,
    "/competitors/", team_id, "/roster?limit=100&lang=en&region=us"
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
      aids <- if (length(refs) == 0L) character(0) else
        sub(".*/roster/([0-9]+).*", "\\1", refs)
      result <- data.frame(
        league     = rep(league, length(refs)),
        event_id   = rep(as.character(event_id), length(refs)),
        team_id    = rep(as.character(team_id), length(refs)),
        athlete_id = aids,
        ref        = refs,
        stringsAsFactors = FALSE
      ) %>% dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Competitor Roster"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} competitor roster for event_id={event_id}, team_id={team_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} competitor roster",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_event_competitor_statistics (long-format team box)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball competitor team statistics (long format)
#' @noRd
.espn_baseball_event_competitor_statistics <- function(league, event_id,
                                                          team_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id, team_id = team_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Competitor Statistics"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/events/", event_id, "/competitions/", event_id,
    "/competitors/", team_id, "/statistics?lang=en&region=us"
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
            league           = league,
            event_id         = as.character(event_id),
            team_id          = as.character(team_id),
            category_name    = cat_name,
            category_display = cat_disp,
            stat_name        = s[["name"]] %||% NA_character_,
            stat_abbrev      = s[["abbreviation"]] %||% NA_character_,
            stat_display     = s[["displayName"]] %||% NA_character_,
            value            = suppressWarnings(as.numeric(s[["value"]] %||% NA)),
            display_value    = as.character(s[["displayValue"]] %||% NA)
          )
        }
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), event_id = character(0),
          team_id = character(0), category_name = character(0),
          category_display = character(0), stat_name = character(0),
          stat_abbrev = character(0), stat_display = character(0),
          value = numeric(0), display_value = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Competitor Statistics"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Competitor Statistics"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} competitor statistics for event_id={event_id}, team_id={team_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} competitor statistics",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_event_competitor_records (per-game record types)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball competitor records (at-game record breakdown)
#' @noRd
.espn_baseball_event_competitor_records <- function(league, event_id,
                                                       team_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id, team_id = team_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Competitor Records"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/events/", event_id, "/competitions/", event_id,
    "/competitors/", team_id, "/records?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      items <- raw[["items"]] %||% list()
      rows <- list()
      for (it in items) {
        rows[[length(rows) + 1L]] <- list(
          league             = league,
          event_id           = as.character(event_id),
          team_id            = as.character(team_id),
          record_id          = as.character(it[["id"]] %||% NA),
          name               = it[["name"]] %||% NA_character_,
          abbreviation       = it[["abbreviation"]] %||% NA_character_,
          display_name       = it[["displayName"]] %||% NA_character_,
          short_display_name = it[["shortDisplayName"]] %||% NA_character_,
          type               = it[["type"]] %||% NA_character_,
          summary            = it[["summary"]] %||% NA_character_,
          value              = suppressWarnings(as.numeric(it[["value"]] %||% NA))
        )
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), event_id = character(0),
          team_id = character(0), record_id = character(0),
          name = character(0), abbreviation = character(0),
          display_name = character(0), short_display_name = character(0),
          type = character(0), summary = character(0),
          value = numeric(0), stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Competitor Records"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Competitor Records"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} competitor records for event_id={event_id}, team_id={team_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} competitor records",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_event_player_box
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball per-game player box score (long format)
#'
#' Wraps `events/{eid}/competitions/{cid}/competitors/{tid}/roster/{aid}/statistics/{type}`.
#' One row per (category x stat) — same shape as `event_competitor_statistics`
#' but scoped to a single athlete-in-event instead of the full team.
#'
#' @noRd
.espn_baseball_event_player_box <- function(league, event_id, team_id,
                                                athlete_id, stat_type = 0L,
                                                ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id, team_id = team_id,
                athlete_id = athlete_id, stat_type = stat_type)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Event Player Box"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/events/", event_id, "/competitions/", event_id,
    "/competitors/", team_id, "/roster/", athlete_id,
    "/statistics/", as.integer(stat_type),
    "?lang=en&region=us"
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
            league           = league,
            event_id         = as.character(event_id),
            team_id          = as.character(team_id),
            athlete_id       = as.character(athlete_id),
            stat_type        = as.integer(stat_type),
            category_name    = cat_name,
            category_display = cat_disp,
            stat_name        = s[["name"]] %||% NA_character_,
            stat_abbrev      = s[["abbreviation"]] %||% NA_character_,
            stat_display     = s[["displayName"]] %||% NA_character_,
            value            = suppressWarnings(as.numeric(s[["value"]] %||% NA)),
            display_value    = as.character(s[["displayValue"]] %||% NA)
          )
        }
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), event_id = character(0),
          team_id = character(0), athlete_id = character(0),
          stat_type = integer(0), category_name = character(0),
          category_display = character(0), stat_name = character(0),
          stat_abbrev = character(0), stat_display = character(0),
          value = numeric(0), display_value = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Event Player Box"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Event Player Box"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} event player box for event_id={event_id}, team_id={team_id}, athlete_id={athlete_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} event player box",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_event_competitor_roster_entry
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single roster entry for an athlete-in-event
#'
#' Wraps `events/{eid}/competitions/{cid}/competitors/{tid}/roster/{aid}`.
#' Single-row tibble with starter flag, DNP reason, ejection flag, period
#' of entry, and athlete + position $refs.
#'
#' @noRd
.espn_baseball_event_competitor_roster_entry <- function(league, event_id,
                                                             team_id, athlete_id,
                                                             ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id, team_id = team_id,
                athlete_id = athlete_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Event Competitor Roster Entry"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/events/", event_id, "/competitions/", event_id,
    "/competitors/", team_id, "/roster/", athlete_id,
    "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      athlete_ref <- if (is.list(raw[["athlete"]]))
        raw[["athlete"]][["$ref"]] %||% NA_character_ else NA_character_
      position_ref <- if (is.list(raw[["position"]]))
        raw[["position"]][["$ref"]] %||% NA_character_ else NA_character_

      row <- list(
        league       = league,
        event_id     = as.character(event_id),
        team_id      = as.character(team_id),
        athlete_id   = as.character(athlete_id),
        player_id    = as.character(raw[["playerId"]] %||% NA),
        period       = as.integer(raw[["period"]] %||% NA),
        active       = as.logical(raw[["active"]] %||% NA),
        starter      = as.logical(raw[["starter"]] %||% NA),
        did_not_play = as.logical(raw[["didNotPlay"]] %||% NA),
        reason       = as.character(raw[["reason"]] %||% NA_character_),
        ejected      = as.logical(raw[["ejected"]] %||% NA),
        for_player_id = as.character(raw[["forPlayerId"]] %||% NA_character_),
        jersey       = as.character(raw[["jersey"]] %||% NA_character_),
        display_name = as.character(raw[["displayName"]] %||% NA_character_),
        athlete_ref  = athlete_ref,
        position_ref = position_ref
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league),
                 " Event Competitor Roster Entry"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} event roster entry for event_id={event_id}, team_id={team_id}, athlete_id={athlete_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} event roster entry",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_event_competitor_score
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball competitor final score (single-row)
#'
#' Wraps `events/{eid}/competitions/{cid}/competitors/{tid}/score`. Returns a
#' one-row tibble with the team's final score, display value, winner flag,
#' and source.
#'
#' @noRd
.espn_baseball_event_competitor_score <- function(league, event_id, team_id,
                                                      ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id, team_id = team_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Event Competitor Score"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/events/", event_id, "/competitions/", event_id,
    "/competitors/", team_id, "/score?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      source <- raw[["source"]]
      source_id <- if (is.list(source)) as.character(source[["id"]] %||% NA) else
        if (is.character(source)) source else NA_character_
      source_desc <- if (is.list(source)) as.character(source[["description"]] %||% NA) else NA_character_
      row <- list(
        league        = league,
        event_id      = as.character(event_id),
        team_id       = as.character(team_id),
        value         = suppressWarnings(as.numeric(raw[["value"]] %||% NA)),
        display_value = as.character(raw[["displayValue"]] %||% NA_character_),
        winner        = as.logical(raw[["winner"]] %||% NA),
        source_id     = source_id,
        source_description = source_desc
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Event Competitor Score"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} competitor score for event_id={event_id}, team_id={team_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} competitor score",
      args = .args),
    finally = {}
  )
  result
}
