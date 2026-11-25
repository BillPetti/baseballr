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
    expr = .baseballr_with_warnings(".espn_baseball_league_injuries", {
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
    }),
    error = function(e) {
      .report_api_error(
        e,
        hint = "Invalid arguments or no injury data available for {league} season {season}!",
        args = .args
      )
    },
    finally = {}
  )

  return(injuries_df)
}


# Columns of the team injuries frame, in order; the empty frame carries them too.
.espn_baseball_team_injury_cols <- c(
  "team_id", "athlete_id", "athlete_name", "position", "status", "date",
  "type", "side", "returns_at", "short_comment", "long_comment",
  "injury_id", "detail"
)

#' Internal helper: fetch team-scoped ESPN baseball injuries
#'
#' Reads the core API's
#' `https://sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/teams/{team_id}/injuries`
#' list, whose `items` are `$ref` links, then fetches each injury and (once per
#' player) its athlete for the name and position. The site route
#' (`site.api.espn.com/.../teams/{team_id}/injuries`) answers an empty object
#' or HTTP 500 for every team. A failed injury or athlete lookup leaves that
#' row's fields `NA` (ids still come from the `$ref`) and says so.
#'
#' @param league character(1). `"mlb"`.
#' @param team_id character or numeric. ESPN team identifier (passed as-is;
#'   no zero-padding applied).
#' @param ... Currently unused. Reserved for future argument threading.
#'
#' @return A `baseballr_data` tibble with the columns in
#'   `.espn_baseball_team_injury_cols`, all character; zero rows (same
#'   columns) when the team has no injuries or the request fails.
#'
#' @noRd
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows
.espn_baseball_team_injuries <- function(league, team_id, ...) {
  stopifnot(league %in% c("mlb"))

  .args <- list(league = league, team_id = team_id)
  type_label <- paste0("ESPN ", toupper(league), " Team Injury Information from ESPN.com")
  cols <- .espn_baseball_team_injury_cols

  injuries_df <- .empty_baseballr_data(type_label, cols = cols)

  tryCatch(
    expr = .baseballr_with_warnings(".espn_baseball_team_injuries", {
      url <- paste0(
        "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
        league, "/teams/", team_id,
        "/injuries?limit=200&lang=en&region=us"
      )

      res <- .retry_request(url)
      check_status(res)
      raw <- jsonlite::fromJSON(.resp_text(res), simplifyVector = FALSE)

      items <- raw[["items"]] %||% list()
      refs <- vapply(items, function(x) x[["$ref"]] %||% NA_character_, character(1))
      refs <- refs[!is.na(refs)]
      total <- raw[["count"]] %||% length(items)
      if (total > length(items)) {
        cli::cli_alert_warning("{(.baseballr_user_fn())}(): ESPN lists {total} injuries for team {team_id}; only the first {length(items)} are returned.")
      }

      if (length(refs) > 0L) {
        # Each item is a link; one lookup per injury, then one per athlete.
        fetch_ref <- function(ref, what) {
          tryCatch({
            r <- .retry_request(ref)
            check_status(r)
            jsonlite::fromJSON(.resp_text(r), simplifyVector = FALSE)
          }, error = function(e) {
            cli::cli_alert_danger("{(.baseballr_user_fn())}(): {what} lookup {ref} failed, so its fields are NA: {conditionMessage(e)}")
            NULL
          })
        }
        chr <- function(x) if (length(x) == 0L) NA_character_ else as.character(x[[1]])
        na_or <- function(a, b) if (is.na(a)) b else a
        ref_id <- function(ref, kind) {
          if (is.null(ref) || is.na(ref)) return(NA_character_)
          m <- regmatches(ref, regexec(paste0("/", kind, "/(-?[0-9]+)"), ref))[[1]]
          if (length(m) == 2L) m[[2]] else NA_character_
        }

        injuries <- lapply(refs, fetch_ref, what = "Injury")
        athlete_refs <- vapply(seq_along(refs), function(i) {
          injuries[[i]][["athlete"]][["$ref"]] %||%
            sub("/injuries/.*$", "?lang=en&region=us", refs[[i]])
        }, character(1))
        athletes <- lapply(unique(athlete_refs), fetch_ref, what = "Athlete")
        names(athletes) <- unique(athlete_refs)

        rows <- lapply(seq_along(refs), function(i) {
          inj <- injuries[[i]] %||% list()
          ath <- athletes[[athlete_refs[[i]]]] %||% list()
          details <- inj[["details"]] %||% list()
          data.frame(
            team_id       = na_or(ref_id(inj[["team"]][["$ref"]], "teams"), as.character(team_id)),
            athlete_id    = ref_id(refs[[i]], "athletes"),
            athlete_name  = chr(ath[["displayName"]] %||% ath[["fullName"]]),
            position      = chr(ath[["position"]][["abbreviation"]]),
            status        = chr(inj[["status"]]),
            date          = chr(inj[["date"]]),
            type          = chr(details[["type"]]),
            side          = chr(details[["side"]]),
            returns_at    = chr(details[["returnDate"]]),
            short_comment = chr(inj[["shortComment"]]),
            long_comment  = chr(inj[["longComment"]]),
            injury_id     = na_or(chr(inj[["id"]]), ref_id(refs[[i]], "injuries")),
            detail        = chr(details[["detail"]]),
            stringsAsFactors = FALSE
          )
        })

        injuries_df <- dplyr::bind_rows(rows) |>
          dplyr::as_tibble() |>
          make_baseballr_data(type_label, Sys.time())
      }
    }),
    error = function(e) {
      .report_api_error(
        e,
        hint = "Invalid arguments or no injury data available for team {team_id} in {league}!",
        args = .args
      )
    },
    finally = {}
  )

  return(injuries_df)
}
