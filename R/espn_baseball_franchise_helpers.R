# espn_baseball_franchise_helpers.R
# Internal helpers for the ESPN MLB franchise wrappers.
# Each helper accepts `league = "mlb"` or `league = "mlb"`.
# None of these are exported.

# ---------------------------------------------------------------------------
# .espn_baseball_franchise
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball franchise detail
#'
#' Fetches the core-v2 franchise resource
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/franchises/{franchise_id}`
#' and returns a single-row tibble. The franchise resource is stable across
#' relocations and rebrands — for example, the Charlotte Hornets and New
#' Orleans Pelicans share a different franchise lineage than current /
#' historical teams suggest.
#'
#' @param league character. `"mlb"`.
#' @param franchise_id character or numeric. ESPN franchise identifier.
#' @param ... Unused; absorbed for forward compatibility.
#' @return A single-row `baseballr_data` tibble, or `NULL` on error.
#' @noRd
.espn_baseball_franchise <- function(league, franchise_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, franchise_id = franchise_id)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Franchise from ESPN.com"))

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/franchises/", franchise_id,
    "?lang=en&region=us"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

      scalar_keys <- c(
        "id", "uid", "slug", "location", "name", "nickname",
        "abbreviation", "displayName", "shortDisplayName",
        "color", "isActive"
      )
      row <- list()
      for (k in scalar_keys) {
        v <- raw[[k]]
        row[[k]] <- if (is.null(v)) NA else v
      }
      row[["league"]] <- league

      logos <- raw[["logos"]]
      logo_hrefs <- if (is.list(logos)) {
        vapply(logos, function(x) x[["href"]] %||% NA_character_,
               character(1))
      } else character(0)
      row[["logo"]]      <- if (length(logo_hrefs) >= 1L) logo_hrefs[1L] else NA_character_
      row[["logo_dark"]] <- if (length(logo_hrefs) >= 2L) logo_hrefs[2L] else NA_character_

      for (k in c("venue", "team")) {
        v <- raw[[k]]
        url_v <- if (is.list(v)) v[["$ref"]] %||% NA_character_ else NA_character_
        row[[paste0(k, "_ref")]] <- url_v
      }

      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league),
                 " Franchise from ESPN.com"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} franchise={franchise_id}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} franchise={franchise_id}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_franchises
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball franchises index
#'
#' Fetches the paginated franchise index
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/franchises?limit=200`
#' and returns a tibble of franchise IDs and the `$ref` URL for each — feed
#' the IDs to `.espn_baseball_franchise()` to get full details.
#'
#' @param league character.
#' @param ... Unused.
#' @return A `baseballr_data` tibble with one row per franchise, or `NULL` on error.
#' @noRd
.espn_baseball_franchises <- function(league, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Franchises Index"))

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/franchises?limit=200&lang=en&region=us"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

      items <- raw[["items"]] %||% list()
      refs <- if (length(items) == 0L) character(0) else
        vapply(items, function(x) x[["$ref"]] %||% NA_character_,
               character(1))
      ids <- if (length(refs) == 0L) character(0) else
        sub(".*/franchises/([0-9]+).*", "\\1", refs)
      result <- data.frame(
        franchise_id = ids,
        ref          = refs,
        league       = rep(league, length(refs)),
        stringsAsFactors = FALSE
      ) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Franchises Index"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} franchises index",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} franchises index",
      args = .args
    ),
    finally = {}
  )
  return(result)
}
