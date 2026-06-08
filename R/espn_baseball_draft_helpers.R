# espn_baseball_draft_helpers.R
# Internal helper for single-draft-pick endpoint.

# ---------------------------------------------------------------------------
# .espn_baseball_draft_pick (single pick detail)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single draft pick detail
#' @noRd
.espn_baseball_draft_pick <- function(league, season, round, pick, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season, round = round, pick = pick)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Draft Pick Detail"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/draft/rounds/", round,
    "/picks/", pick, "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      aref <- if (is.list(raw[["athlete"]]))
        raw[["athlete"]][["$ref"]] %||% NA_character_ else NA_character_
      tref <- if (is.list(raw[["team"]]))
        raw[["team"]][["$ref"]] %||% NA_character_ else NA_character_
      aid <- if (!is.na(aref))
        sub(".*/athletes/([0-9]+).*", "\\1", aref) else NA_character_
      tid <- if (!is.na(tref))
        sub(".*/teams/([0-9]+).*", "\\1", tref) else NA_character_
      status_obj <- raw[["status"]]
      status_name <- if (is.list(status_obj))
        status_obj[["name"]] %||% NA_character_ else NA_character_
      row <- list(
        league       = league,
        season       = as.integer(season),
        round        = as.integer(raw[["round"]] %||% round),
        pick         = as.integer(raw[["pick"]] %||% pick),
        overall      = as.integer(raw[["overall"]] %||% NA),
        traded       = isTRUE(raw[["traded"]]),
        trade_note   = raw[["tradeNote"]] %||% NA_character_,
        status       = status_name,
        athlete_id   = aid,
        team_id      = tid,
        athlete_ref  = aref,
        team_ref     = tref
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Draft Pick Detail"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} draft pick for season={season}, round={round}, pick={pick}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} draft pick",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_draft_rounds (per-round summary)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball draft rounds index
#'
#' Returns one row per round with metadata + pick count. The picks
#' themselves are nested but better accessed individually via
#' `.espn_baseball_draft_pick()`.
#'
#' @noRd
.espn_baseball_draft_rounds <- function(league, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Draft Rounds"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season,
    "/draft/rounds?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      items <- raw[["items"]] %||% list()
      rows <- list()
      for (it in items) {
        status_obj <- it[["status"]]
        status_type <- if (is.list(status_obj)) {
          tp <- status_obj[["type"]]
          if (is.list(tp)) tp[["name"]] %||% NA_character_
          else as.character(tp %||% NA_character_)
        } else NA_character_
        rows[[length(rows) + 1L]] <- list(
          league             = league,
          season             = as.integer(season),
          round              = as.integer(it[["number"]] %||% NA),
          display_name       = it[["displayName"]] %||% NA_character_,
          short_display_name = it[["shortDisplayName"]] %||% NA_character_,
          n_picks            = length(it[["picks"]] %||% list()),
          status             = status_type
        )
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), season = integer(0),
          round = integer(0), display_name = character(0),
          short_display_name = character(0), n_picks = integer(0),
          status = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Draft Rounds"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Draft Rounds"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} draft rounds for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} draft rounds",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_draft_athletes (paginated draftee index)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball draft-athletes index
#' @noRd
.espn_baseball_draft_athletes <- function(league, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Draft Athletes"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season,
    "/draft/athletes?limit=200&lang=en&region=us"
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
        sub(".*/draft/athletes/([0-9]+).*", "\\1", refs)
      result <- data.frame(
        league     = rep(league, length(refs)),
        season     = rep(as.integer(season), length(refs)),
        athlete_id = ids,
        ref        = refs,
        stringsAsFactors = FALSE
      ) %>% dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Draft Athletes"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} draft athletes for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} draft athletes",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_draft_status (single-row draft status snapshot)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball draft status
#' @noRd
.espn_baseball_draft_status <- function(league, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Draft Status"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season,
    "/draft/status?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      tp <- raw[["type"]]
      row <- list(
        league      = league,
        season      = as.integer(season),
        round       = as.integer(raw[["round"]] %||% NA),
        type_id     = if (is.list(tp)) as.character(tp[["id"]] %||% NA) else NA_character_,
        type_name   = if (is.list(tp)) tp[["name"]] %||% NA_character_ else NA_character_,
        type_state  = if (is.list(tp)) tp[["state"]] %||% NA_character_ else NA_character_,
        description = if (is.list(tp)) tp[["description"]] %||% NA_character_ else NA_character_
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Draft Status"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} draft status for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} draft status",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_draft_athlete_detail
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single drafted athlete record
#' @noRd
.espn_baseball_draft_athlete_detail <- function(league, season,
                                                    athlete_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season, athlete_id = athlete_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Draft Athlete Detail"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season,
    "/draft/athletes/", athlete_id, "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      pos <- raw[["position"]]
      pos_name <- if (is.list(pos)) as.character(pos[["displayName"]] %||% pos[["name"]] %||% NA) else NA_character_
      pos_abbrev <- if (is.list(pos)) as.character(pos[["abbreviation"]] %||% NA) else NA_character_

      ath <- raw[["athlete"]]
      ath_ref <- if (is.list(ath)) as.character(ath[["$ref"]] %||% NA) else NA_character_
      ath_real_id <- if (!is.na(ath_ref))
        sub(".*/athletes/([0-9]+).*", "\\1", ath_ref) else NA_character_

      pk <- raw[["pick"]]
      pick_overall <- if (is.list(pk)) suppressWarnings(as.integer(pk[["overall"]] %||% NA)) else NA_integer_
      pick_round   <- if (is.list(pk)) suppressWarnings(as.integer(pk[["round"]] %||% NA)) else NA_integer_
      pick_team_ref <- if (is.list(pk) && is.list(pk[["team"]]))
        as.character(pk[["team"]][["$ref"]] %||% NA) else NA_character_
      pick_team_id <- if (!is.na(pick_team_ref))
        sub(".*/teams/([0-9]+).*", "\\1", pick_team_ref) else NA_character_

      row <- list(
        league             = league,
        season             = as.integer(season),
        draftee_id         = as.character(raw[["id"]] %||% athlete_id),
        athlete_id         = ath_real_id,
        first_name         = as.character(raw[["firstName"]] %||% NA_character_),
        last_name          = as.character(raw[["lastName"]] %||% NA_character_),
        full_name          = as.character(raw[["fullName"]] %||% NA_character_),
        display_name       = as.character(raw[["displayName"]] %||% NA_character_),
        height             = suppressWarnings(as.numeric(raw[["height"]] %||% NA)),
        display_height     = as.character(raw[["displayHeight"]] %||% NA_character_),
        weight             = suppressWarnings(as.numeric(raw[["weight"]] %||% NA)),
        display_weight     = as.character(raw[["displayWeight"]] %||% NA_character_),
        position_name      = pos_name,
        position_abbrev    = pos_abbrev,
        pick_overall       = pick_overall,
        pick_round         = pick_round,
        pick_team_id       = pick_team_id,
        athlete_ref        = ath_ref,
        headshot           = as.character(raw[["headshot"]] %||% NA_character_)
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Draft Athlete Detail"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} draft athlete detail for season={season}, athlete_id={athlete_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} draft athlete detail",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_season_draft
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball draft year top-level metadata
#'
#' Wraps `seasons/{y}/draft`. Single-row tibble with year, numberOfRounds,
#' displayName, shortDisplayName, plus `$ref`s for the deeper sub-resources
#' (athletes, rounds, positions, status) that are already wrapped by
#' `.espn_baseball_draft_athletes()`, `.espn_baseball_draft_rounds()`,
#' and `.espn_baseball_draft_status()`.
#'
#' @noRd
.espn_baseball_season_draft <- function(league, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Draft (top-level)"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/draft?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      sub_ref <- function(k) {
        v <- raw[[k]]
        if (is.list(v)) as.character(v[["$ref"]] %||% NA_character_) else
          if (is.character(v)) v else NA_character_
      }
      row <- list(
        league             = league,
        season             = as.integer(season),
        year               = suppressWarnings(as.integer(raw[["year"]] %||% NA)),
        uid                = as.character(raw[["uid"]] %||% NA_character_),
        number_of_rounds   = suppressWarnings(as.integer(raw[["numberOfRounds"]] %||% NA)),
        display_name       = as.character(raw[["displayName"]] %||% NA_character_),
        short_display_name = as.character(raw[["shortDisplayName"]] %||% NA_character_),
        status_ref         = sub_ref("status"),
        athletes_ref       = sub_ref("athletes"),
        rounds_ref         = sub_ref("rounds")
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Season Draft (top-level)"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} season draft top-level for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} season draft top-level",
      args = .args),
    finally = {}
  )
  result
}
