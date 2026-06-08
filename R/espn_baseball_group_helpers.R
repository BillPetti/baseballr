# espn_baseball_group_helpers.R
# Internal helpers for the ESPN MLB group / conference wrappers.

# ---------------------------------------------------------------------------
# .espn_baseball_season_groups (index)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball season-groups index
#'
#' `season_type` accepts a scalar or vector; default `c(2L, 3L)` fetches
#' regular season + postseason groups and binds them.
#'
#' @noRd
.espn_baseball_season_groups <- function(league, season,
                                            season_type = c(2L, 3L), ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season, season_type = season_type)

  fetch_one <- function(st) {
    url <- paste0(
      "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
      league, "/seasons/", season, "/types/", st,
      "/groups?limit=200&lang=en&region=us"
    )
    res <- tryCatch(.retry_request(url), error = function(e) NULL)
    if (is.null(res) || httr2::resp_status(res) != 200L) return(NULL)
    raw <- res %>% .resp_text() %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
    items <- raw[["items"]] %||% list()
    refs <- if (length(items) == 0L) character(0) else
      vapply(items, function(x) x[["$ref"]] %||% NA_character_,
             character(1))
    ids <- if (length(refs) == 0L) character(0) else
      sub(".*/groups/([0-9]+).*", "\\1", refs)
    data.frame(
      league      = rep(league, length(refs)),
      season      = rep(as.integer(season), length(refs)),
      season_type = rep(as.integer(st), length(refs)),
      group_id    = ids,
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
          season_type = integer(0), group_id = character(0),
          ref = character(0), stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season Groups Index"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, parts) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season Groups Index"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} season groups for season={season}, season_type={season_type}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} season groups",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_season_group (detail)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single group detail
#' @noRd
.espn_baseball_season_group <- function(league, season,
                                           season_type = 2L,
                                           group_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season,
                season_type = season_type, group_id = group_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Group Detail"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/types/", season_type,
    "/groups/", group_id, "?lang=en&region=us"
  )
  tryCatch(
    expr = {
      res <- .retry_request(url); check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      ref_key <- function(k) {
        v <- raw[[k]]
        if (is.list(v)) v[["$ref"]] %||% NA_character_ else NA_character_
      }
      row <- list(
        league         = league,
        season         = as.integer(season),
        season_type    = as.integer(season_type),
        group_id       = as.character(raw[["id"]] %||% group_id),
        uid            = raw[["uid"]] %||% NA_character_,
        name           = raw[["name"]] %||% NA_character_,
        abbreviation   = raw[["abbreviation"]] %||% NA_character_,
        short_name     = raw[["shortName"]] %||% NA_character_,
        midsize_name   = raw[["midsizeName"]] %||% NA_character_,
        is_conference  = isTRUE(raw[["isConference"]]),
        slug           = raw[["slug"]] %||% NA_character_,
        parent_ref     = ref_key("parent"),
        children_ref   = ref_key("children"),
        teams_ref      = ref_key("teams"),
        standings_ref  = ref_key("standings")
      )
      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Season Group Detail"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} group {group_id} for season={season}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} group {group_id}",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_season_group_children
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball group children index
#' @noRd
.espn_baseball_season_group_children <- function(league, season,
                                                    season_type = 2L,
                                                    group_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season,
                season_type = season_type, group_id = group_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Group Children Index"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/types/", season_type,
    "/groups/", group_id, "/children?limit=200&lang=en&region=us"
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
        sub(".*/groups/([0-9]+).*", "\\1", refs)
      result <- data.frame(
        league          = rep(league, length(refs)),
        season          = rep(as.integer(season), length(refs)),
        season_type     = rep(as.integer(season_type), length(refs)),
        parent_group_id = rep(as.character(group_id), length(refs)),
        child_group_id  = ids,
        ref             = refs,
        stringsAsFactors = FALSE
      ) %>% dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Season Group Children Index"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} group {group_id} children",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} group {group_id} children",
      args = .args),
    finally = {}
  )
  result
}

# ---------------------------------------------------------------------------
# .espn_baseball_season_group_teams
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball group teams index
#' @noRd
.espn_baseball_season_group_teams <- function(league, season,
                                                 season_type = 2L,
                                                 group_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season,
                season_type = season_type, group_id = group_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Group Teams Index"))
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/types/", season_type,
    "/groups/", group_id, "/teams?limit=200&lang=en&region=us"
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
        sub(".*/teams/([0-9]+).*", "\\1", refs)
      result <- data.frame(
        league      = rep(league, length(refs)),
        season      = rep(as.integer(season), length(refs)),
        season_type = rep(as.integer(season_type), length(refs)),
        group_id    = rep(as.character(group_id), length(refs)),
        team_id     = ids,
        ref         = refs,
        stringsAsFactors = FALSE
      ) %>% dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Season Group Teams Index"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} group {group_id} teams",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} group {group_id} teams",
      args = .args),
    finally = {}
  )
  result
}
