# espn_baseball_position_helpers.R
# Internal helpers shared by the position dictionary wrappers.
# Each helper accepts the package's allowed league slugs.
# None of these are exported.

# ---------------------------------------------------------------------------
# .espn_baseball_positions
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball positions index
#'
#' Fetches the position dictionary at
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/positions`
#' and returns one row per position with its id and canonical `$ref` URL.
#'
#' @param league character.
#' @param ... Unused; reserved for forward compatibility.
#' @return A `baseballr_data` tibble, or `NULL` on error.
#' @noRd
.espn_baseball_positions <- function(league, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Positions Index"))

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/positions?limit=200&lang=en&region=us"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

      items <- raw[["items"]] %||% list()
      refs  <- if (length(items) == 0L) character(0) else
        vapply(items, function(x) x[["$ref"]] %||% NA_character_,
               character(1))
      ids   <- if (length(refs) == 0L) character(0) else
        sub(".*/positions/([0-9]+).*", "\\1", refs)
      result <- data.frame(
        position_id = ids,
        ref         = refs,
        league      = rep(league, length(refs)),
        stringsAsFactors = FALSE
      ) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Positions Index"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} positions index",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} positions index",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_position
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball position detail
#'
#' Fetches one position from
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/positions/{position_id}`.
#'
#' @param league character.
#' @param position_id character or numeric. ESPN position id.
#' @param ... Unused.
#' @return A single-row `baseballr_data` tibble, or `NULL` on error.
#' @noRd
.espn_baseball_position <- function(league, position_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, position_id = position_id)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Position from ESPN.com"))

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/positions/", position_id,
    "?lang=en&region=us"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

      parent_ref <- if (is.list(raw[["parent"]]))
        raw[["parent"]][["$ref"]] %||% NA_character_ else NA_character_

      row <- list(
        position_id        = as.character(raw[["id"]] %||% NA_character_),
        name               = as.character(raw[["name"]] %||% NA_character_),
        display_name       = as.character(raw[["displayName"]] %||% NA_character_),
        abbreviation       = as.character(raw[["abbreviation"]] %||% NA_character_),
        leaf               = as.logical(raw[["leaf"]] %||% NA),
        parent_ref         = parent_ref,
        league             = league
      )

      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Position from ESPN.com"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} position={position_id}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} position={position_id}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}
