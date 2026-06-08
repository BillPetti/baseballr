# espn_baseball_coach_helpers.R
# Internal helpers for the ESPN MLB single-coach wrappers.

# ---------------------------------------------------------------------------
# .espn_baseball_coach
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single coach detail
#'
#' Fetches `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/coaches/{coach_id}`
#' and returns a single-row tibble with name, biography, current team /
#' college refs, and a count of career-record and coach-season entries.
#'
#' @noRd
.espn_baseball_coach <- function(league, coach_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, coach_id = coach_id)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Coach Detail"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/coaches/", coach_id, "?lang=en&region=us"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

      bp <- raw[["birthPlace"]]
      birth_city  <- if (is.list(bp)) bp[["city"]]  %||% NA_character_ else NA_character_
      birth_state <- if (is.list(bp)) bp[["state"]] %||% NA_character_ else NA_character_

      coll  <- raw[["college"]]
      team  <- raw[["team"]]

      row <- list(
        coach_id        = as.character(raw[["id"]] %||% coach_id),
        uid             = raw[["uid"]] %||% NA_character_,
        first_name      = raw[["firstName"]] %||% NA_character_,
        last_name       = raw[["lastName"]] %||% NA_character_,
        date_of_birth   = raw[["dateOfBirth"]] %||% NA_character_,
        birth_city      = birth_city,
        birth_state     = birth_state,
        n_career_records = length(raw[["careerRecords"]] %||% list()),
        n_coach_seasons  = length(raw[["coachSeasons"]] %||% list()),
        college_ref     = if (is.list(coll)) coll[["$ref"]] %||% NA_character_ else NA_character_,
        team_ref        = if (is.list(team)) team[["$ref"]] %||% NA_character_ else NA_character_,
        league          = league
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Coach Detail"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} coach {coach_id}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} coach {coach_id}",
      args = .args
    ),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_coach_record
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball coach career record (long format)
#'
#' Wraps `coaches/{coach_id}/record/{record_type}`. Returns one row per
#' stat in the record's `stats[]` array (e.g., wins, losses, win pct).
#' The top-level summary fields (name, type, displayValue) are
#' denormalized into every row for context.
#'
#' record_type values commonly populated: 0 = Total, 1 = Pre Season,
#' 2 = Regular Season, 3 = Post Season.
#'
#' @noRd
.espn_baseball_coach_record <- function(league, coach_id,
                                            record_type = 0L, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, coach_id = coach_id,
                record_type = record_type)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Coach Record"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/coaches/", coach_id,
    "/record/", as.integer(record_type), "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      rec_name <- as.character(raw[["name"]] %||% NA_character_)
      rec_type <- as.character(raw[["type"]] %||% NA_character_)
      summary <- as.character(raw[["summary"]] %||% NA_character_)
      display_value <- as.character(raw[["displayValue"]] %||% NA_character_)

      stats <- raw[["stats"]] %||% list()
      rows <- list()
      for (s in stats) {
        rows[[length(rows) + 1L]] <- list(
          league            = league,
          coach_id          = as.character(coach_id),
          record_type_id    = as.integer(record_type),
          record_name       = rec_name,
          record_type       = rec_type,
          record_summary    = summary,
          record_display    = display_value,
          stat_name         = s[["name"]] %||% NA_character_,
          stat_abbrev       = s[["abbreviation"]] %||% NA_character_,
          stat_display      = s[["displayName"]] %||% NA_character_,
          value             = suppressWarnings(as.numeric(s[["value"]] %||% NA)),
          stat_display_value = as.character(s[["displayValue"]] %||% NA_character_)
        )
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), coach_id = character(0),
          record_type_id = integer(0), record_name = character(0),
          record_type = character(0), record_summary = character(0),
          record_display = character(0), stat_name = character(0),
          stat_abbrev = character(0), stat_display = character(0),
          value = numeric(0), stat_display_value = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Coach Record"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Coach Record"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} coach record for coach_id={coach_id}, record_type={record_type}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} coach record",
      args = .args),
    finally = {}
  )
  result
}
