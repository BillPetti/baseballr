#' Internal helper -- ESPN baseball calendar
#'
#' Fetches the schedule calendar for an ESPN baseball league and returns a
#' tidy tibble of calendar entries. This is the single implementation shared by
#' `espn_mbb_calendar()` (league = "mlb") and
#' `espn_mlb_calendar()` (league = "mlb"). The scoreboard endpoint carries a
#' `leagues[[1]]$calendar` block that describes the season types and their
#' date-range entries (weeks / round labels).
#'
#' @param league character. One of `"mlb"`.
#' @param season integer or character. Four-digit season year (e.g. `2025`).
#' @param ... Currently unused; reserved for future pass-through arguments.
#' @return A `baseballr_data` tibble of calendar entries, or `NULL` on error.
#' @noRd
.espn_baseball_calendar <- function(league, season, ...) {
  if (!league %in% c("mlb")) {
    stop(
      paste0(
        "league must be one of 'mlb', got: ",
        league
      ),
      call. = FALSE
    )
  }

  .args <- mget(setdiff(names(formals()), "..."))

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/baseball/",
    league,
    "/scoreboard?dates=",
    as.integer(season)
  )

  calendar <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Calendar from ESPN.com"))

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)

      resp <- res %>%
        .resp_text()

      raw <- jsonlite::fromJSON(resp, flatten = FALSE)

      # Navigate to the leagues[[1]]$calendar block. With jsonlite's default
      # simplification, `raw$leagues` parses as a 1-row data.frame (not a
      # list-of-lists), so direct field access works on the data frame and
      # `calendar` lands as a list-column whose [[1]] element is the calendar
      # payload.
      leagues_block <- raw[["leagues"]]
      empty_calendar <- function() {
        data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Calendar from ESPN.com"),
            Sys.time()
          )
      }

      if (is.null(leagues_block) ||
          (is.data.frame(leagues_block) && nrow(leagues_block) == 0) ||
          (!is.data.frame(leagues_block) && length(leagues_block) == 0)) {
        calendar <- empty_calendar()
        return(calendar)
      }

      # Resolve "first league" record across both possible jsonlite shapes:
      #   - data.frame (with simplifyDataFrame = TRUE, the default)
      #   - list of lists (with simplifyDataFrame = FALSE)
      if (is.data.frame(leagues_block)) {
        first_league <- as.list(leagues_block[1, , drop = FALSE])
        # List-columns wrap their value once more; unwrap.
        unwrap1 <- function(v) if (is.list(v) && length(v) == 1) v[[1]] else v
        first_league <- lapply(first_league, unwrap1)
      } else {
        first_league <- leagues_block[[1]]
      }

      cal_block <- first_league[["calendar"]]

      if (is.null(cal_block) || length(cal_block) == 0) {
        calendar <- empty_calendar()
        return(calendar)
      }

      # ESPN scoreboard returns calendar in one of two shapes:
      #   (A) Flat list of ISO date strings, e.g. "2025-05-02T07:00Z" -- the
      #       common ESPN scoreboard shape in 2025+.
      #   (B) Legacy nested season-type objects each with $entries[] -- the
      #       older shape used by some baseball/NFL endpoints.
      # Detect which shape we got and parse accordingly.
      # Detect calendar shape robustly. With simplifyDataFrame=TRUE the
      # whole calendar may already be a character vector of date strings.
      is_flat_dates <- is.character(cal_block) ||
        (is.list(cal_block) && length(cal_block) >= 1 &&
           !is.null(cal_block[[1]]) &&
           is.character(cal_block[[1]]) &&
           length(cal_block[[1]]) == 1)

      cal_start <- first_league[["calendarStartDate"]] %||% NA_character_
      cal_end   <- first_league[["calendarEndDate"]]   %||% NA_character_
      cal_type  <- first_league[["calendarType"]]      %||% NA_character_

      if (is_flat_dates) {
        # Shape A: flat list of date strings
        date_chars <- vapply(cal_block, function(x) {
          if (is.character(x) && length(x) >= 1) as.character(x[[1]]) else NA_character_
        }, character(1))
        n <- length(date_chars)
        combined <- data.frame(
          season            = rep(as.character(season), n),
          season_type       = rep(NA_character_, n),
          season_type_label = rep(NA_character_, n),
          season_start_date = rep(cal_start, n),
          season_end_date   = rep(cal_end, n),
          calendar_type     = rep(cal_type, n),
          label             = rep(NA_character_, n),
          alternate_label   = rep(NA_character_, n),
          detail            = rep(NA_character_, n),
          value             = date_chars,
          start_date        = date_chars,
          end_date          = date_chars,
          stringsAsFactors  = FALSE
        )
      } else {
        # Shape B: nested season-type objects, each with $entries[]
        rows <- lapply(seq_along(cal_block), function(i) {
          st <- cal_block[[i]]

          season_type_label <- st[["label"]] %||% NA_character_
          season_type_value <- st[["value"]] %||% NA_character_
          season_type_startDate <- st[["startDate"]] %||% NA_character_
          season_type_endDate   <- st[["endDate"]]   %||% NA_character_

          entries <- st[["entries"]]
          if (is.null(entries) || length(entries) == 0) {
            return(data.frame(
              season            = as.character(season),
              season_type       = season_type_value,
              season_type_label = season_type_label,
              season_start_date = season_type_startDate,
              season_end_date   = season_type_endDate,
              calendar_type     = cal_type,
              label             = NA_character_,
              alternate_label   = NA_character_,
              detail            = NA_character_,
              value             = NA_character_,
              start_date        = NA_character_,
              end_date          = NA_character_,
              stringsAsFactors  = FALSE
            ))
          }

          if (is.data.frame(entries)) {
            n <- nrow(entries)
            data.frame(
              season            = rep(as.character(season), n),
              season_type       = rep(season_type_value, n),
              season_type_label = rep(season_type_label, n),
              season_start_date = rep(season_type_startDate, n),
              season_end_date   = rep(season_type_endDate, n),
              calendar_type     = rep(cal_type, n),
              label             = as.character(entries[["label"]] %||% rep(NA_character_, n)),
              alternate_label   = as.character(entries[["alternateLabel"]] %||% rep(NA_character_, n)),
              detail            = as.character(entries[["detail"]] %||% rep(NA_character_, n)),
              value             = as.character(entries[["value"]] %||% rep(NA_character_, n)),
              start_date        = as.character(entries[["startDate"]] %||% rep(NA_character_, n)),
              end_date          = as.character(entries[["endDate"]] %||% rep(NA_character_, n)),
              stringsAsFactors  = FALSE
            )
          } else {
            entry_rows <- lapply(entries, function(e) {
              data.frame(
                season            = as.character(season),
                season_type       = season_type_value,
                season_type_label = season_type_label,
                season_start_date = season_type_startDate,
                season_end_date   = season_type_endDate,
                calendar_type     = cal_type,
                label             = e[["label"]]           %||% NA_character_,
                alternate_label   = e[["alternateLabel"]]  %||% NA_character_,
                detail            = e[["detail"]]          %||% NA_character_,
                value             = e[["value"]]           %||% NA_character_,
                start_date        = e[["startDate"]]       %||% NA_character_,
                end_date          = e[["endDate"]]         %||% NA_character_,
                stringsAsFactors  = FALSE
              )
            })
            do.call(rbind, entry_rows)
          }
        })

        combined <- do.call(rbind, rows)
      }

      calendar <- combined %>%
        data.frame(stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Calendar from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0(
        "Failed to retrieve ESPN ", league, " calendar for season=", season
      ),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0(
        "Warning retrieving ESPN ", league, " calendar for season=", season
      ),
      args = .args
    ),
    finally = {}
  )
  return(calendar)
}
