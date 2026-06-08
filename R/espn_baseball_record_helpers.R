# espn_baseball_record_helpers.R
# Internal helpers for the ESPN MLB team-record wrappers.

# ---------------------------------------------------------------------------
# .espn_baseball_team_record
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball per-season-type team record
#'
#' Fetches `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/seasons/{season}/types/{season_type}/teams/{team_id}/record`
#' and returns a tibble with one row per (season_type x record_type).
#' `season_type` accepts a scalar or vector; default `c(2L, 3L)` fetches
#' regular season + postseason and binds.
#'
#' @noRd
.espn_baseball_team_record <- function(league, team_id, season,
                                          season_type = c(2L, 3L), ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, team_id = team_id, season = season,
                season_type = season_type)

  fetch_one <- function(st) {
    url <- paste0(
      "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
      league, "/seasons/", season, "/types/", st,
      "/teams/", team_id, "/record?lang=en&region=us"
    )
    res <- tryCatch(.retry_request(url), error = function(e) NULL)
    if (is.null(res)) return(list())
    if (httr2::resp_status(res) != 200L) return(list())
    raw <- res %>% .resp_text() %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
    items <- raw[["items"]] %||% list()
    lapply(items, function(it) {
      list(
        league             = league,
        team_id            = as.character(team_id),
        season             = as.integer(season),
        season_type        = as.integer(st),
        record_id          = as.character(it[["id"]] %||% NA),
        name               = it[["name"]] %||% NA_character_,
        abbreviation       = it[["abbreviation"]] %||% NA_character_,
        display_name       = it[["displayName"]] %||% NA_character_,
        short_display_name = it[["shortDisplayName"]] %||% NA_character_,
        description        = it[["description"]] %||% NA_character_,
        type               = it[["type"]] %||% NA_character_,
        summary            = it[["summary"]] %||% NA_character_,
        display_value      = it[["displayValue"]] %||% NA_character_,
        value              = as.numeric(it[["value"]] %||% NA)
      )
    })
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
          league = character(0), team_id = character(0),
          season = integer(0), season_type = integer(0),
          record_id = character(0), name = character(0),
          abbreviation = character(0), display_name = character(0),
          short_display_name = character(0), description = character(0),
          type = character(0), summary = character(0),
          display_value = character(0), value = numeric(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Record"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(all_rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Record"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} team record for team_id={team_id}, season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} team record for team_id={team_id}",
      args = .args
    ),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_team_record_detail
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball team record detail (long format)
#'
#' Wraps `seasons/{y}/types/{t}/teams/{tid}/records/{record_id}`. Returns
#' one row per stat in the record's `stats[]` array. Higher `record_id`
#' values are typically per-opponent (e.g., "Chicago Bulls 1-1") while
#' low values (0-3) are overall / home / away / conference.
#'
#' @noRd
.espn_baseball_team_record_detail <- function(league, team_id, season,
                                                  record_id, season_type = 2L,
                                                  ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, team_id = team_id, season = season,
                record_id = record_id, season_type = season_type)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Team Record Detail"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season,
    "/types/", as.integer(season_type),
    "/teams/", team_id,
    "/records/", record_id, "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      rec_name <- as.character(raw[["name"]] %||% NA_character_)
      rec_abbrev <- as.character(raw[["abbreviation"]] %||% NA_character_)
      rec_display <- as.character(raw[["displayName"]] %||% NA_character_)
      rec_type <- as.character(raw[["type"]] %||% NA_character_)
      summary <- as.character(raw[["summary"]] %||% NA_character_)
      display_value <- as.character(raw[["displayValue"]] %||% NA_character_)

      stats <- raw[["stats"]] %||% list()
      rows <- list()
      for (s in stats) {
        rows[[length(rows) + 1L]] <- list(
          league            = league,
          team_id           = as.character(team_id),
          season            = as.integer(season),
          season_type       = as.integer(season_type),
          record_id         = as.character(record_id),
          record_name       = rec_name,
          record_abbrev     = rec_abbrev,
          record_display    = rec_display,
          record_type       = rec_type,
          record_summary    = summary,
          stat_name         = s[["name"]] %||% NA_character_,
          stat_abbrev       = s[["abbreviation"]] %||% NA_character_,
          stat_display      = s[["displayName"]] %||% NA_character_,
          value             = suppressWarnings(as.numeric(s[["value"]] %||% NA)),
          stat_display_value = as.character(s[["displayValue"]] %||% NA_character_)
        )
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), team_id = character(0),
          season = integer(0), season_type = integer(0),
          record_id = character(0), record_name = character(0),
          record_abbrev = character(0), record_display = character(0),
          record_type = character(0), record_summary = character(0),
          stat_name = character(0), stat_abbrev = character(0),
          stat_display = character(0), value = numeric(0),
          stat_display_value = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Record Detail"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Record Detail"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} team record detail for team_id={team_id}, season={season}, record_id={record_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} team record detail",
      args = .args),
    finally = {}
  )
  result
}
