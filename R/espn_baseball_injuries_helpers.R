# Internal helpers for ESPN baseball injury endpoints.
# MLB uses the standard ESPN injuries JSON shape;
# the only difference is the league slug in the URL.

#' Internal helper: fetch league-wide ESPN baseball injuries
#'
#' Fetches the `injuries` array from
#' `https://site.api.espn.com/apis/site/v2/sports/baseball/{league}/injuries`
#' and returns a flat tibble. The `season` parameter is informational only
#' (ESPN's injury endpoint does not accept a `?season=` query parameter) but is
#' attached as a constant column on the output for downstream joins.
#'
#' @param league character(1). One of `"mlb"` or
#'   `"mlb"`.
#' @param season numeric or character. The season year (e.g. `2025`). Attached
#'   as a constant column; not sent as a query parameter.
#' @param ... Currently unused. Reserved for future argument threading.
#'
#' @return A `baseballr_data` tibble (or `NULL` on error). Columns depend on the
#'   ESPN response; expected columns include `team_id`, `athlete_id`,
#'   `athlete_name`, `position`, `status`, `date`, `type`, `side`,
#'   `returns_at`, `short_comment`, `long_comment`. Returns an empty tibble
#'   when ESPN's `injuries` array is empty (can be empty).
#'
#' @noRd
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows mutate
#' @importFrom janitor clean_names
.espn_baseball_league_injuries <- function(league, season, ...) {
  stopifnot(league %in% c("mlb"))

  .args <- list(league = league, season = season)

  injuries_df <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Injury Information from ESPN.com"))

  tryCatch(
    expr = {
      url <- paste0(
        "https://site.api.espn.com/apis/site/v2/sports/baseball/",
        league,
        "/injuries"
      )

      res <- .retry_request(url)
      check_status(res)
      resp <- res %>% .resp_text()

      raw <- jsonlite::fromJSON(resp, simplifyVector = FALSE)
      injuries_raw <- raw[["injuries"]]

      if (is.null(injuries_raw) || length(injuries_raw) == 0L) {
        injuries_df <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Injury Information from ESPN.com"),
            Sys.time()
          )
        return(invisible(injuries_df))
      }

      rows <- lapply(injuries_raw, function(entry) {
        team     <- entry[["team"]] %||% list()
        athlete  <- entry[["athlete"]] %||% list()
        injury   <- entry[["injury"]] %||% list()

        data.frame(
          team_id       = team[["id"]]          %||% NA_character_,
          athlete_id    = athlete[["id"]]        %||% NA_character_,
          athlete_name  = athlete[["displayName"]] %||% NA_character_,
          position      = {
            pos <- athlete[["position"]] %||% list()
            pos[["abbreviation"]] %||% NA_character_
          },
          status        = entry[["status"]]      %||% NA_character_,
          date          = entry[["date"]]         %||% NA_character_,
          type          = injury[["type"]]        %||% NA_character_,
          side          = injury[["location"]]    %||% NA_character_,
          returns_at    = entry[["returnDate"]]   %||% NA_character_,
          short_comment = entry[["shortComment"]] %||% NA_character_,
          long_comment  = entry[["longComment"]]  %||% NA_character_,
          stringsAsFactors = FALSE
        )
      })

      injuries_df <- dplyr::bind_rows(rows) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(season = as.integer(season)) %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Injury Information from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(
        e,
        hint = "Invalid arguments or no injury data available for {league} season {season}!",
        args = .args
      )
    },
    warning = function(w) {
      .report_api_warning(w, args = .args)
    },
    finally = {}
  )

  return(injuries_df)
}


#' Internal helper: fetch team-scoped ESPN baseball injuries
#'
#' Fetches the `injuries` array from
#' `https://site.api.espn.com/apis/site/v2/sports/baseball/{league}/teams/{team_id}/injuries`
#' and returns a flat tibble.
#'
#' @param league character(1). One of `"mlb"` or
#'   `"mlb"`.
#' @param team_id character or numeric. ESPN team identifier (passed as-is;
#'   no zero-padding applied).
#' @param ... Currently unused. Reserved for future argument threading.
#'
#' @return A `baseballr_data` tibble (or `NULL` on error). Expected columns
#'   include `team_id`, `athlete_id`, `athlete_name`, `position`, `status`,
#'   `date`, `type`, `side`, `returns_at`, `short_comment`, `long_comment`.
#'
#' @noRd
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows
#' @importFrom janitor clean_names
.espn_baseball_team_injuries <- function(league, team_id, ...) {
  stopifnot(league %in% c("mlb"))

  .args <- list(league = league, team_id = team_id)

  injuries_df <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Team Injury Information from ESPN.com"))

  tryCatch(
    expr = {
      url <- paste0(
        "https://site.api.espn.com/apis/site/v2/sports/baseball/",
        league,
        "/teams/", team_id,
        "/injuries"
      )

      res <- .retry_request(url)
      check_status(res)
      resp <- res %>% .resp_text()

      raw <- jsonlite::fromJSON(resp, simplifyVector = FALSE)
      injuries_raw <- raw[["injuries"]]

      if (is.null(injuries_raw) || length(injuries_raw) == 0L) {
        injuries_df <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Injury Information from ESPN.com"),
            Sys.time()
          )
        return(invisible(injuries_df))
      }

      rows <- lapply(injuries_raw, function(entry) {
        team     <- entry[["team"]] %||% list()
        athlete  <- entry[["athlete"]] %||% list()
        injury   <- entry[["injury"]] %||% list()

        data.frame(
          team_id       = team[["id"]]            %||% NA_character_,
          athlete_id    = athlete[["id"]]          %||% NA_character_,
          athlete_name  = athlete[["displayName"]] %||% NA_character_,
          position      = {
            pos <- athlete[["position"]] %||% list()
            pos[["abbreviation"]] %||% NA_character_
          },
          status        = entry[["status"]]        %||% NA_character_,
          date          = entry[["date"]]           %||% NA_character_,
          type          = injury[["type"]]          %||% NA_character_,
          side          = injury[["location"]]      %||% NA_character_,
          returns_at    = entry[["returnDate"]]     %||% NA_character_,
          short_comment = entry[["shortComment"]]   %||% NA_character_,
          long_comment  = entry[["longComment"]]    %||% NA_character_,
          stringsAsFactors = FALSE
        )
      })

      injuries_df <- dplyr::bind_rows(rows) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Team Injury Information from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(
        e,
        hint = "Invalid arguments or no injury data available for team {team_id} in {league}!",
        args = .args
      )
    },
    warning = function(w) {
      .report_api_warning(w, args = .args)
    },
    finally = {}
  )

  return(injuries_df)
}
