# espn_mlb_transactions.R
# MLB-only ESPN endpoint wrappers:
#   espn_mlb_draft(), espn_mlb_freeagents(), espn_mlb_transactions()
# These endpoints are MLB-specific, so they are implemented directly
# without shared helpers.

# ---------------------------------------------------------------------------
# espn_mlb_draft
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Draft Picks**
#' @name espn_mlb_draft
NULL
#' @title
#' **Get ESPN MLB Draft Picks**
#' @rdname espn_mlb_draft
#' @author Saiem Gilani
#' @param season Season year (numeric, e.g. 2025). Defaults to the most
#'   recent MLB season.
#' @param ... Additional arguments; currently unused but retained for
#'   forward compatibility. Proxy configuration should use
#'   `options(baseballr.proxy = ...)`.
#' @return A `baseballr_data` tibble with one row per draft pick:
#'
#'    |col_name    |types     |description                                           |
#'    |:-----------|:---------|:-----------------------------------------------------|
#'    |season      |integer   |Season identifier (4-digit year or 'YYYY-YY' string). |
#'    |round       |integer   |Tournament / playoff round.                           |
#'    |pick        |integer   |                                                      |
#'    |overall     |integer   |Overall.                                              |
#'    |traded      |logical   |                                                      |
#'    |trade_note  |character |                                                      |
#'    |status      |character |Status label.                                         |
#'    |athlete_id  |character |Unique athlete identifier (ESPN).                     |
#'    |athlete_ref |character |                                                      |
#'    |team_id     |character |Unique team identifier.                               |
#'    |team_ref    |character |                                                      |
#'
#'    Athlete and team details (name, position, college, abbreviation) are not
#'    inlined in the draft response; resolve them via `espn_mlb_player_info()`
#'    or `espn_mlb_team()` using the returned IDs.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble mutate
#' @importFrom rlang %||%
#' @import rvest
#' @export
#' @family ESPN MLB Functions
#' @details
#' Calls the ESPN core-v2 endpoint
#' `https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/seasons/{year}/draft/rounds`,
#' which returns each round of the draft with its picks inlined as `picks: [...]`.
#' For historical seasons with no ESPN draft data the function returns an empty
#' tibble rather than erroring.
#' @examples
#' \donttest{
#'   espn_mlb_draft(season = 2024)
#' }
espn_mlb_draft <- function(season = most_recent_mlb_season(), ...) {
  .args <- mget(setdiff(names(formals()), "..."))

  picks_df <- data.frame(stringsAsFactors = FALSE)

  # Parse the trailing path-segment ID out of an ESPN $ref URL, e.g.
  # ".../athletes/108565?lang=..." -> "108565".
  .ref_id <- function(ref, segment) {
    if (is.null(ref) || is.na(ref) || !nzchar(ref)) return(NA_character_)
    m <- regmatches(ref, regexec(paste0("/", segment, "/(\\d+)"), ref))[[1]]
    if (length(m) >= 2) m[[2]] else NA_character_
  }

  tryCatch(
    expr = {
      # /draft itself returns {rounds, athletes, ...} (no top-level items).
      # /draft/rounds returns a paged collection where each item is a round
      # carrying its picks inline as `picks: [...]`.
      rounds_url <- paste0(
        "https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb",
        "/seasons/", as.integer(season), "/draft/rounds"
      )

      res <- .retry_request(rounds_url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = FALSE)

      rounds <- raw[["items"]]
      if (is.null(rounds) || length(rounds) == 0) {
        picks_df <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data("ESPN MLB Draft Picks from ESPN.com", Sys.time())
        return(picks_df)
      }

      pick_rows <- list()
      for (rd in rounds) {
        round_no <- as.integer(rd[["number"]] %||% NA_integer_)
        picks    <- rd[["picks"]]
        if (is.null(picks) || length(picks) == 0) next

        for (pk in picks) {
          ath_ref  <- pk[["athlete"]][["$ref"]] %||% NA_character_
          team_ref <- pk[["team"]][["$ref"]]    %||% NA_character_

          pick_rows[[length(pick_rows) + 1L]] <- data.frame(
            season       = as.integer(season),
            round        = round_no,
            pick         = as.integer(pk[["pick"]]    %||% NA_integer_),
            overall      = as.integer(pk[["overall"]] %||% NA_integer_),
            traded       = isTRUE(pk[["traded"]]),
            trade_note   = as.character(pk[["tradeNote"]] %||% NA_character_),
            status       = as.character(pk[["status"]][["name"]] %||% NA_character_),
            athlete_id   = .ref_id(ath_ref,  "athletes"),
            athlete_ref  = as.character(ath_ref),
            team_id      = .ref_id(team_ref, "teams"),
            team_ref     = as.character(team_ref),
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(pick_rows) == 0) {
        picks_df <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data("ESPN MLB Draft Picks from ESPN.com", Sys.time())
        return(picks_df)
      }

      picks_df <- do.call(rbind, pick_rows) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data("ESPN MLB Draft Picks from ESPN.com", Sys.time())
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN MLB draft data for season ", season),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN MLB draft data for season ", season),
      args = .args
    ),
    finally = {}
  )
  return(picks_df)
}


# ---------------------------------------------------------------------------
# espn_mlb_freeagents
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Free Agents**
#' @name espn_mlb_freeagents
NULL
#' @title
#' **Get ESPN MLB Free Agents**
#' @rdname espn_mlb_freeagents
#' @author Saiem Gilani
#' @param season Season year (numeric, e.g. 2025). Defaults to the most
#'   recent MLB season.
#' @param ... Additional arguments; currently unused but retained for
#'   forward compatibility. Proxy configuration should use
#'   `options(baseballr.proxy = ...)`.
#' @return A `baseballr_data` tibble with one row per free agent:
#'
#'    |col_name            |types     |description                                           |
#'    |:-------------------|:---------|:-----------------------------------------------------|
#'    |season              |integer   |Season identifier (4-digit year or 'YYYY-YY' string). |
#'    |athlete_id          |character |Unique athlete identifier (ESPN).                     |
#'    |athlete_name        |character |Athlete display name (ESPN).                          |
#'    |position            |character |Listed roster position (G, F, C, etc.).               |
#'    |prior_team_id       |character |                                                      |
#'    |status              |character |Status label.                                         |
#'    |signed_team_id      |character |                                                      |
#'    |signed_date         |character |                                                      |
#'    |contract_value      |character |                                                      |
#'    |contract_term_years |character |                                                      |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom rlang %||%
#' @import rvest
#' @export
#' @family ESPN MLB Functions
#' @details
#' Calls the ESPN core-v2 endpoint
#' `https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/seasons/{year}/freeagents`.
#' As of 2026-05, this endpoint returns HTTP 500 for every season tested and
#' there is no documented replacement; the function consequently returns an
#' empty tibble and emits a CLI error message. It is retained so that downstream
#' code does not break if ESPN restores the endpoint, but should not be relied
#' on in the meantime.
#' @examples
#' \donttest{
#'   espn_mlb_freeagents(season = 2025)
#' }
espn_mlb_freeagents <- function(season = most_recent_mlb_season(), ...) {
  .args <- mget(setdiff(names(formals()), "..."))

  fa_df <- data.frame(stringsAsFactors = FALSE)

  tryCatch(
    expr = {
      url <- paste0(
        "https://sports.core.api.espn.com/v2/sports/baseball/leagues/mlb",
        "/seasons/", as.integer(season), "/freeagents"
      )

      res <- .retry_request(url)
      check_status(res)

      raw <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON(simplifyDataFrame = TRUE)

      items <- raw[["items"]]

      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        fa_df <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data("ESPN MLB Free Agents from ESPN.com", Sys.time())
        return(fa_df)
      }

      if (!is.data.frame(items)) {
        items <- as.data.frame(items, stringsAsFactors = FALSE)
      }

      # Extract nested athlete fields
      athlete_id   <- NA_character_
      athlete_name <- NA_character_
      if ("athlete" %in% colnames(items)) {
        ath <- items[["athlete"]]
        if (is.data.frame(ath)) {
          athlete_id   <- as.character(ath[["id"]] %||% NA_character_)
          athlete_name <- as.character(ath[["displayName"]] %||% NA_character_)
        } else if (is.list(ath)) {
          athlete_id <- vapply(ath, function(a) {
            as.character(if (is.list(a) || is.data.frame(a)) a[["id"]] %||% NA_character_ else NA_character_)
          }, character(1))
          athlete_name <- vapply(ath, function(a) {
            as.character(if (is.list(a) || is.data.frame(a)) a[["displayName"]] %||% NA_character_ else NA_character_)
          }, character(1))
        }
      }

      position_val <- NA_character_
      if ("position" %in% colnames(items)) {
        pos <- items[["position"]]
        if (is.data.frame(pos)) {
          position_val <- as.character(pos[["abbreviation"]] %||% NA_character_)
        } else if (is.list(pos)) {
          position_val <- vapply(pos, function(p) {
            as.character(if (is.list(p) || is.data.frame(p)) p[["abbreviation"]] %||% NA_character_ else NA_character_)
          }, character(1))
        } else {
          position_val <- as.character(pos)
        }
      }

      prior_team_id <- NA_character_
      if ("team" %in% colnames(items)) {
        tm <- items[["team"]]
        if (is.data.frame(tm)) {
          prior_team_id <- as.character(tm[["id"]] %||% NA_character_)
        } else if (is.list(tm)) {
          prior_team_id <- vapply(tm, function(t) {
            as.character(if (is.list(t) || is.data.frame(t)) t[["id"]] %||% NA_character_ else NA_character_)
          }, character(1))
        }
      }

      signed_team_id <- NA_character_
      if ("signedTeam" %in% colnames(items)) {
        stm <- items[["signedTeam"]]
        if (is.data.frame(stm)) {
          signed_team_id <- as.character(stm[["id"]] %||% NA_character_)
        } else if (is.list(stm)) {
          signed_team_id <- vapply(stm, function(t) {
            as.character(if (is.list(t) || is.data.frame(t)) t[["id"]] %||% NA_character_ else NA_character_)
          }, character(1))
        }
      }

      get_scalar_col <- function(df, col) {
        if (col %in% colnames(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
      }

      fa_df <- data.frame(
        season              = as.integer(season),
        athlete_id          = athlete_id,
        athlete_name        = athlete_name,
        position            = position_val,
        prior_team_id       = prior_team_id,
        status              = get_scalar_col(items, "status"),
        signed_team_id      = signed_team_id,
        signed_date         = get_scalar_col(items, "signedDate"),
        contract_value      = get_scalar_col(items, "contractValue"),
        contract_term_years = get_scalar_col(items, "contractTermYears"),
        stringsAsFactors = FALSE
      ) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data("ESPN MLB Free Agents from ESPN.com", Sys.time())
    },
    error = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN MLB free agents for season {season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN MLB free agents for season {season}",
      args = .args
    ),
    finally = {}
  )
  return(fa_df)
}


# ---------------------------------------------------------------------------
# espn_mlb_transactions
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Transactions**
#' @name espn_mlb_transactions
NULL
#' @title
#' **Get ESPN MLB Transactions**
#' @rdname espn_mlb_transactions
#' @author Saiem Gilani
#' @param season Season year (numeric, e.g. 2025). Defaults to the most
#'   recent MLB season.
#' @param limit Maximum number of transactions to return (integer). Default
#'   `100`.
#' @param ... Additional arguments; currently unused but retained for
#'   forward compatibility. Proxy configuration should use
#'   `options(baseballr.proxy = ...)`.
#' @return A `baseballr_data` tibble with one row per transaction:
#'
#'    |col_name       |types     |description                       |
#'    |:--------------|:---------|:---------------------------------|
#'    |transaction_id |character |                                  |
#'    |date           |character |Date in YYYY-MM-DD format.        |
#'    |type           |character |Record type / category.           |
#'    |description    |character |Long-form description text.       |
#'    |team_id        |character |Unique team identifier.           |
#'    |athlete_id     |character |Unique athlete identifier (ESPN). |
#'    |athlete_name   |character |Athlete display name (ESPN).      |
#'    |from_team_id   |character |                                  |
#'    |to_team_id     |character |                                  |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom rlang %||%
#' @import rvest
#' @export
#' @family ESPN MLB Functions
#' @details
#' Calls the ESPN site-v2 endpoint
#' `https://site.api.espn.com/apis/site/v2/sports/baseball/mlb/transactions?season={year}&limit={limit}`.
#' Releases have a `NULL` `to_team_id`; those are stored as `NA`. Returns an
#' empty tibble rather than erroring when no transactions are available.
#' @examples
#' \donttest{
#'   espn_mlb_transactions(season = 2025, limit = 10)
#' }
espn_mlb_transactions <- function(
    season = most_recent_mlb_season(),
    limit  = 100,
    ...) {
  .args <- mget(setdiff(names(formals()), "..."))

  tx_df <- data.frame(stringsAsFactors = FALSE)

  tryCatch(
    expr = {
      url <- paste0(
        "https://site.api.espn.com/apis/site/v2/sports/baseball/mlb",
        "/transactions?season=", as.integer(season),
        "&limit=", as.integer(limit)
      )

      res <- .retry_request(url)
      check_status(res)

      raw <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON(simplifyDataFrame = TRUE)

      items <- raw[["transactions"]]
      if (is.null(items)) items <- raw[["items"]]

      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        tx_df <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data("ESPN MLB Transactions from ESPN.com", Sys.time())
        return(tx_df)
      }

      if (!is.data.frame(items)) {
        items <- as.data.frame(items, stringsAsFactors = FALSE)
      }

      # Nested field extractors
      athlete_id   <- NA_character_
      athlete_name <- NA_character_
      if ("athlete" %in% colnames(items)) {
        ath <- items[["athlete"]]
        if (is.data.frame(ath)) {
          athlete_id   <- as.character(ath[["id"]] %||% NA_character_)
          athlete_name <- as.character(ath[["displayName"]] %||% NA_character_)
        } else if (is.list(ath)) {
          athlete_id <- vapply(ath, function(a) {
            as.character(if (is.list(a) || is.data.frame(a)) a[["id"]] %||% NA_character_ else NA_character_)
          }, character(1))
          athlete_name <- vapply(ath, function(a) {
            as.character(if (is.list(a) || is.data.frame(a)) a[["displayName"]] %||% NA_character_ else NA_character_)
          }, character(1))
        }
      }

      team_id <- NA_character_
      if ("team" %in% colnames(items)) {
        tm <- items[["team"]]
        if (is.data.frame(tm)) {
          team_id <- as.character(tm[["id"]] %||% NA_character_)
        } else if (is.list(tm)) {
          team_id <- vapply(tm, function(t) {
            as.character(if (is.list(t) || is.data.frame(t)) t[["id"]] %||% NA_character_ else NA_character_)
          }, character(1))
        }
      }

      from_team_id <- NA_character_
      if ("fromTeam" %in% colnames(items)) {
        ftm <- items[["fromTeam"]]
        if (is.data.frame(ftm)) {
          from_team_id <- as.character(ftm[["id"]] %||% NA_character_)
        } else if (is.list(ftm)) {
          from_team_id <- vapply(ftm, function(t) {
            as.character(if (is.list(t) || is.data.frame(t)) t[["id"]] %||% NA_character_ else NA_character_)
          }, character(1))
        }
      }

      to_team_id <- NA_character_
      if ("toTeam" %in% colnames(items)) {
        ttm <- items[["toTeam"]]
        if (is.data.frame(ttm)) {
          to_team_id <- as.character(ttm[["id"]] %||% NA_character_)
        } else if (is.list(ttm)) {
          to_team_id <- vapply(ttm, function(t) {
            as.character(if (is.list(t) || is.data.frame(t)) t[["id"]] %||% NA_character_ else NA_character_)
          }, character(1))
        }
      }

      get_col <- function(df, col) {
        if (col %in% colnames(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
      }

      tx_df <- data.frame(
        transaction_id = get_col(items, "id"),
        date           = get_col(items, "date"),
        type           = get_col(items, "type"),
        description    = get_col(items, "description"),
        team_id        = team_id,
        athlete_id     = athlete_id,
        athlete_name   = athlete_name,
        from_team_id   = from_team_id,
        to_team_id     = to_team_id,
        stringsAsFactors = FALSE
      ) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data("ESPN MLB Transactions from ESPN.com", Sys.time())
    },
    error = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN MLB transactions for season {season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN MLB transactions for season {season}",
      args = .args
    ),
    finally = {}
  )
  return(tx_df)
}
