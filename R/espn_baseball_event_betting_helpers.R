# espn_baseball_event_betting_helpers.R
# Internal helpers shared by the event betting-market wrappers
# (odds, prop bets). Each helper accepts `league = "mlb"` etc.
# None of these are exported.

# ---------------------------------------------------------------------------
# .espn_baseball_event_odds
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball event odds
#'
#' Fetches
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/events/{event_id}/competitions/{event_id}/odds`
#' and returns a tidy tibble (one row per provider). some leagues typically return
#' an empty tibble because ESPN does not carry NCAA baseball odds lines.
#'
#' @param league character. `"mlb"`.
#' @param event_id character or numeric. ESPN event/game identifier.
#' @param ... Unused; absorbed for forward compatibility.
#' @return A `baseballr_data` tibble or `NULL` on error.
#' @noRd
.espn_baseball_event_odds <- function(league, event_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Event Odds (event_id=", event_id, ") from ESPN.com"))

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/events/",
    event_id,
    "/competitions/",
    event_id,
    "/odds"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      items <- raw[["items"]]

      if (is.null(items) || !is.data.frame(items) || nrow(items) == 0) {
        result <- data.frame(
          event_id              = character(0),
          provider_id           = character(0),
          provider_name         = character(0),
          details               = character(0),
          over_under            = numeric(0),
          spread                = numeric(0),
          home_money_line       = integer(0),
          away_money_line       = integer(0),
          home_team_odds_open   = numeric(0),
          home_team_odds_close  = numeric(0),
          away_team_odds_open   = numeric(0),
          away_team_odds_close  = numeric(0),
          stringsAsFactors      = FALSE
        ) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Event Odds (event_id=", event_id, ") from ESPN.com"),
            Sys.time()
          )
        return(result)
      }

      n <- nrow(items)

      provider_id   <- rep(NA_character_, n)
      provider_name <- rep(NA_character_, n)

      if (!is.null(items[["provider"]]) && is.data.frame(items[["provider"]])) {
        prov <- items[["provider"]]
        provider_id   <- as.character(prov[["id"]] %||% NA_character_)
        provider_name <- as.character(prov[["name"]] %||% NA_character_)
      }

      details         <- as.character(items[["details"]] %||% NA_character_)
      over_under      <- suppressWarnings(as.numeric(items[["overUnder"]] %||% NA_real_))
      spread          <- suppressWarnings(as.numeric(items[["spread"]] %||% NA_real_))
      home_money_line <- suppressWarnings(as.integer(items[["homeTeamOdds.moneyLine"]] %||%
                           items[["homeMoneyLine"]] %||% NA_integer_))
      away_money_line <- suppressWarnings(as.integer(items[["awayTeamOdds.moneyLine"]] %||%
                           items[["awayMoneyLine"]] %||% NA_integer_))

      # Team-odds open/close -- nested under homeTeamOdds / awayTeamOdds
      home_open  <- rep(NA_real_, n)
      home_close <- rep(NA_real_, n)
      away_open  <- rep(NA_real_, n)
      away_close <- rep(NA_real_, n)

      if (!is.null(items[["homeTeamOdds"]]) && is.data.frame(items[["homeTeamOdds"]])) {
        hto <- items[["homeTeamOdds"]]
        home_open  <- suppressWarnings(as.numeric(hto[["open.value"]] %||%
                        hto[["openLine"]] %||% NA_real_))
        home_close <- suppressWarnings(as.numeric(hto[["close.value"]] %||%
                        hto[["closeLine"]] %||% NA_real_))
      }
      if (!is.null(items[["awayTeamOdds"]]) && is.data.frame(items[["awayTeamOdds"]])) {
        ato <- items[["awayTeamOdds"]]
        away_open  <- suppressWarnings(as.numeric(ato[["open.value"]] %||%
                        ato[["openLine"]] %||% NA_real_))
        away_close <- suppressWarnings(as.numeric(ato[["close.value"]] %||%
                        ato[["closeLine"]] %||% NA_real_))
      }

      odds_df <- data.frame(
        event_id             = as.character(event_id),
        provider_id          = provider_id,
        provider_name        = provider_name,
        details              = details,
        over_under           = over_under,
        spread               = spread,
        home_money_line      = home_money_line,
        away_money_line      = away_money_line,
        home_team_odds_open  = home_open,
        home_team_odds_close = home_close,
        away_team_odds_open  = away_open,
        away_team_odds_close = away_close,
        stringsAsFactors     = FALSE
      )

      result <- odds_df %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Event Odds (event_id=", event_id, ") from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN ", league, " event odds for event_id=", event_id),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN ", league, " event odds for event_id=", event_id),
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_event_propbets (long-format prop bets)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball event prop bets (long format)
#' @noRd
.espn_baseball_event_propbets <- function(league, event_id, provider_id,
                                             page_limit = 100L, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, event_id = event_id,
                provider_id = provider_id)
  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Event Prop Bets"))
  rows <- list()
  page <- 1L
  page_count <- NA_integer_
  tryCatch(
    expr = {
      repeat {
        url <- paste0(
          "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
          league, "/events/", event_id, "/competitions/", event_id,
          "/odds/", provider_id, "/propBets?limit=", page_limit,
          "&page=", page, "&lang=en&region=us"
        )
        res <- tryCatch(.retry_request(url), error = function(e) NULL)
        if (is.null(res) || httr2::resp_status(res) != 200L) break
        raw <- res %>% .resp_text() %>%
          jsonlite::fromJSON(simplifyVector = FALSE)
        if (is.na(page_count)) {
          page_count <- as.integer(raw[["pageCount"]] %||% 1L)
        }
        items <- raw[["items"]] %||% list()
        for (it in items) {
          aref <- if (is.list(it[["athlete"]]))
            it[["athlete"]][["$ref"]] %||% NA_character_ else NA_character_
          aid <- if (!is.na(aref))
            sub(".*/athletes/([0-9]+).*", "\\1", aref) else NA_character_
          tp <- it[["type"]]
          odds <- it[["odds"]]
          cur <- it[["current"]]
          rows[[length(rows) + 1L]] <- list(
            league          = league,
            event_id        = as.character(event_id),
            provider_id     = as.character(provider_id),
            athlete_id      = aid,
            prop_type_id    = if (is.list(tp)) as.character(tp[["id"]] %||% NA) else NA_character_,
            prop_type_name  = if (is.list(tp)) tp[["name"]] %||% NA_character_ else NA_character_,
            american        = if (is.list(odds)) as.character(odds[["american"]] %||% NA) else NA_character_,
            decimal         = if (is.list(odds)) suppressWarnings(as.numeric(odds[["decimal"]] %||% NA)) else NA_real_,
            fraction        = if (is.list(odds)) as.character(odds[["fraction"]] %||% NA) else NA_character_,
            total           = if (is.list(odds)) suppressWarnings(as.numeric(odds[["total"]] %||% NA)) else NA_real_,
            current_target  = if (is.list(cur)) suppressWarnings(as.numeric(cur[["target"]] %||% NA)) else NA_real_,
            last_updated    = it[["lastUpdated"]] %||% NA_character_,
            athlete_ref     = aref
          )
        }
        if (page >= page_count) break
        page <- page + 1L
        Sys.sleep(0.4)
      }
      if (length(rows) == 0L) {
        result <- data.frame(
          league = character(0), event_id = character(0),
          provider_id = character(0), athlete_id = character(0),
          prop_type_id = character(0), prop_type_name = character(0),
          american = character(0), decimal = numeric(0),
          fraction = character(0), total = numeric(0),
          current_target = numeric(0), last_updated = character(0),
          athlete_ref = character(0),
          stringsAsFactors = FALSE
        ) %>% dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Event Prop Bets"),
            Sys.time()
          )
      } else {
        result <- do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Event Prop Bets"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(e,
      hint = "Failed to retrieve ESPN {league} event prop bets for event_id={event_id}, provider_id={provider_id}",
      args = .args),
    warning = function(w) .report_api_warning(w,
      hint = "Warning retrieving ESPN {league} event prop bets",
      args = .args),
    finally = {}
  )
  result
}
