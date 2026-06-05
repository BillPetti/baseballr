# espn_baseball_futures_helpers.R
# Internal helpers for the ESPN MLB futures wrappers.
# `seasons/{y}/futures` index returns market+books inline, so a single
# fetch yields the full long-format odds table — no separate detail call.

# ---------------------------------------------------------------------------
# .espn_baseball_futures
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball season futures (long format)
#'
#' Fetches the per-season futures-market index:
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/seasons/{season}/futures`.
#' Each item is a futures market (e.g. "MLB Championship Winner") with a
#' single provider and a list of `books` (one entry per team). Returns
#' one row per (market x team) for direct tabular use.
#'
#' @param league character.
#' @param season numeric. Season year.
#' @param ... Unused.
#' @return A `baseballr_data` tibble in long format, or `NULL` on error.
#' @noRd
.espn_baseball_futures <- function(league, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Season Futures"))

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/futures?limit=200&lang=en&region=us"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

      items <- raw[["items"]] %||% list()
      rows <- list()

      for (m in items) {
        market_id   <- m[["id"]] %||% NA_character_
        market_name <- m[["name"]] %||% NA_character_
        market_type <- m[["type"]] %||% NA_character_
        market_disp <- m[["displayName"]] %||% NA_character_

        futures <- m[["futures"]] %||% list()
        for (f in futures) {
          prov <- f[["provider"]]
          prov_id   <- if (is.list(prov)) prov[["id"]]   %||% NA_character_ else NA_character_
          prov_name <- if (is.list(prov)) prov[["name"]] %||% NA_character_ else NA_character_

          books <- f[["books"]] %||% list()
          if (length(books) == 0L) next
          for (b in books) {
            tref <- if (is.list(b[["team"]])) b[["team"]][["$ref"]] %||% NA_character_ else NA_character_
            tid  <- if (!is.na(tref)) sub(".*/teams/([0-9]+).*", "\\1", tref) else NA_character_
            value <- b[["value"]] %||% NA_character_

            rows[[length(rows) + 1L]] <- list(
              season            = as.integer(season),
              league            = league,
              market_id         = market_id,
              market_name       = market_name,
              market_type       = market_type,
              market_display    = market_disp,
              provider_id       = prov_id,
              provider_name     = prov_name,
              team_id           = tid,
              odds_value        = as.character(value),
              team_ref          = tref
            )
          }
        }
      }

      if (length(rows) == 0L) {
        result <- data.frame(
          season         = integer(0),
          league         = character(0),
          market_id      = character(0),
          market_name    = character(0),
          market_type    = character(0),
          market_display = character(0),
          provider_id    = character(0),
          provider_name  = character(0),
          team_id        = character(0),
          odds_value     = character(0),
          team_ref       = character(0),
          stringsAsFactors = FALSE
        ) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season Futures"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                         stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season Futures"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} season futures for season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} season futures for season={season}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}
