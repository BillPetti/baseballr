# espn_baseball_powerindex_helpers.R
# Internal helpers for the ESPN MLB powerindex wrappers.
# Long-format output: one row per (team x stat).

# ---------------------------------------------------------------------------
# .espn_baseball_powerindex
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball season power index (long format)
#'
#' Fetches `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/seasons/{season}/powerindex`,
#' iterating through pages until all teams' power-index-and-related metrics are
#' collected. The endpoint returns all season types in a single paginated
#' response; filtering happens client-side. `season_type` accepts a scalar
#' or vector — default `c(2L, 3L)` keeps regular season + postseason rows.
#'
#' @noRd
.espn_baseball_powerindex <- function(league, season,
                                         season_type = c(2L, 3L),
                                         page_limit = 100L, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season, season_type = season_type)

  st_set <- as.integer(season_type)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Power Index"))
  rows <- list()

  page <- 1L
  page_count <- NA_integer_

  tryCatch(
    expr = {
      repeat {
        url <- paste0(
          "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
          league, "/seasons/", season,
          "/powerindex?limit=", page_limit,
          "&page=", page, "&lang=en&region=us"
        )
        res <- .retry_request(url)
        check_status(res)
        raw <- res %>% .resp_text() %>%
          jsonlite::fromJSON(simplifyVector = FALSE)
        if (is.na(page_count)) {
          page_count <- as.integer(raw[["pageCount"]] %||% 1L)
        }
        items <- raw[["items"]] %||% list()

        for (it in items) {
          item_st <- as.integer(it[["seasonType"]] %||% NA)
          if (length(st_set) > 0L && !is.na(item_st) &&
              !(item_st %in% st_set)) next
          tref <- if (is.list(it[["team"]]))
            it[["team"]][["$ref"]] %||% NA_character_ else NA_character_
          tid <- if (!is.na(tref))
            sub(".*/teams/([0-9]+).*", "\\1", tref) else NA_character_
          stats <- it[["stats"]] %||% list()
          for (s in stats) {
            rows[[length(rows) + 1L]] <- list(
              league         = league,
              season         = as.integer(it[["season"]] %||% season),
              season_type    = as.integer(it[["seasonType"]] %||% NA),
              team_id        = tid,
              stat_name      = s[["name"]] %||% NA_character_,
              abbreviation   = s[["abbreviation"]] %||% NA_character_,
              display_name   = s[["displayName"]] %||% NA_character_,
              description    = s[["description"]] %||% NA_character_,
              value          = suppressWarnings(as.numeric(s[["value"]] %||% NA)),
              display_value  = as.character(s[["displayValue"]] %||% NA),
              last_updated   = it[["lastUpdated"]] %||% NA_character_,
              team_ref       = tref
            )
          }
        }

        if (page >= page_count) break
        page <- page + 1L
        Sys.sleep(0.5)
      }

      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), season = integer(0),
          season_type = integer(0), team_id = character(0),
          stat_name = character(0), abbreviation = character(0),
          display_name = character(0), description = character(0),
          value = numeric(0), display_value = character(0),
          last_updated = character(0), team_ref = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season Power Index"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season Power Index"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} powerindex for season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} powerindex for season={season}",
      args = .args
    ),
    finally = {}
  )
  result
}
