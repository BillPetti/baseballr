# espn_baseball_event_play_helpers.R
# Internal helpers shared by the per-play event wrappers (single
# play detail, on-court personnel). Each helper accepts
# `league = "mlb"` etc. None of these are exported.

# ---------------------------------------------------------------------------
# .espn_baseball_event_play
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single play detail
#'
#' Wraps `events/{eid}/competitions/{cid}/plays/{pid}`. Single-row tibble
#' with sequence, period, clock, text, scoring flag, score, team `$ref`,
#' and shot coordinates if applicable.
#'
#' @noRd
.espn_baseball_event_play <- function(league, event_id, play_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id, play_id = play_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Event Play Detail"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/events/", event_id, "/competitions/", event_id,
    "/plays/", play_id,
    "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

      team_ref <- if (is.list(raw[["team"]]))
        raw[["team"]][["$ref"]] %||% NA_character_ else NA_character_
      type_obj <- raw[["type"]]
      type_id <- if (is.list(type_obj)) as.character(type_obj[["id"]] %||% NA) else NA_character_
      type_text <- if (is.list(type_obj)) as.character(type_obj[["text"]] %||% NA) else NA_character_
      period_n <- if (is.list(raw[["period"]]))
        as.integer(raw[["period"]][["number"]] %||% NA) else NA_integer_
      clock_disp <- if (is.list(raw[["clock"]]))
        as.character(raw[["clock"]][["displayValue"]] %||% NA) else NA_character_
      coord <- raw[["coordinate"]]
      coord_x <- if (is.list(coord)) suppressWarnings(as.numeric(coord[["x"]] %||% NA)) else NA_real_
      coord_y <- if (is.list(coord)) suppressWarnings(as.numeric(coord[["y"]] %||% NA)) else NA_real_

      row <- list(
        league             = league,
        event_id           = as.character(event_id),
        play_id            = as.character(play_id),
        sequence_number    = as.character(raw[["sequenceNumber"]] %||% NA),
        type_id            = type_id,
        type_text          = type_text,
        text               = as.character(raw[["text"]] %||% NA_character_),
        short_text         = as.character(raw[["shortText"]] %||% NA_character_),
        period             = period_n,
        clock              = clock_disp,
        scoring_play       = as.logical(raw[["scoringPlay"]] %||% NA),
        score_value        = suppressWarnings(as.numeric(raw[["scoreValue"]] %||% NA)),
        away_score         = suppressWarnings(as.integer(raw[["awayScore"]] %||% NA)),
        home_score         = suppressWarnings(as.integer(raw[["homeScore"]] %||% NA)),
        shooting_play      = as.logical(raw[["shootingPlay"]] %||% NA),
        coordinate_x       = coord_x,
        coordinate_y       = coord_y,
        team_ref           = team_ref,
        wallclock          = as.character(raw[["wallclock"]] %||% NA_character_)
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Event Play Detail"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} event play detail for event_id={event_id}, play_id={play_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} event play detail",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_event_play_personnel
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball play personnel (players on court at a play)
#'
#' Wraps `events/{eid}/competitions/{cid}/plays/{pid}/personnel`. Long-format
#' tibble: one row per (competitor x athlete entry). When ESPN has not
#' populated personnel for a play, returns a typed empty tibble.
#'
#' @noRd
.espn_baseball_event_play_personnel <- function(league, event_id, play_id,
                                                    ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id, play_id = play_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Event Play Personnel"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/events/", event_id, "/competitions/", event_id,
    "/plays/", play_id, "/personnel?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      pp <- raw[["playPersonnel"]] %||% list()
      rows <- list()
      for (block in pp) {
        comp <- block[["competitor"]]
        comp_ref <- if (is.list(comp)) comp[["$ref"]] %||% NA_character_ else NA_character_
        team_id <- if (!is.na(comp_ref))
          sub(".*/competitors/([0-9]+).*", "\\1", comp_ref) else NA_character_
        entries <- block[["entries"]] %||% list()
        if (length(entries) == 0L) next
        for (e in entries) {
          ath <- e[["athlete"]]
          ath_ref <- if (is.list(ath)) ath[["$ref"]] %||% NA_character_ else NA_character_
          aid <- if (!is.na(ath_ref))
            sub(".*/athletes/([0-9]+).*", "\\1", ath_ref) else NA_character_
          rows[[length(rows) + 1L]] <- list(
            league      = league,
            event_id    = as.character(event_id),
            play_id     = as.character(play_id),
            team_id     = team_id,
            athlete_id  = aid,
            athlete_ref = ath_ref,
            competitor_ref = comp_ref
          )
        }
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), event_id = character(0),
          play_id = character(0), team_id = character(0),
          athlete_id = character(0), athlete_ref = character(0),
          competitor_ref = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Event Play Personnel"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Event Play Personnel"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} play personnel for event_id={event_id}, play_id={play_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} play personnel",
      args = .args),
    finally = {}
  )
  result
}
