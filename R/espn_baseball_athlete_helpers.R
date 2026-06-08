# espn_baseball_athlete_helpers.R
# Internal helpers for the ESPN MLB athlete endpoint wrappers.
# Each helper accepts league = "mlb" or league = "mlb".
# None of these functions are exported.

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_info  (2.1)
# site-v2 /athletes/{athlete_id}
# Returns named list: Bio, Team, Position, Status, College, Draft
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete info
#'
#' Fetches
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/athletes/{athlete_id}`
#' and returns a named list of tibbles: `Bio`, `Team`, `Position`, `Status`,
#' `College`, `Draft`. The legacy `site.api.espn.com/apis/site/v2/...` path
#' was discontinued and now 404s for both leagues.
#'
#' @param league character. `"mlb"`.
#' @param athlete_id character or numeric. ESPN athlete identifier.
#' @param ... Unused; absorbed for forward compatibility.
#' @return Named list of data frames.
#' @noRd
.espn_baseball_athlete_info <- function(league, athlete_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id)

  result <- list()

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/athletes/",
    athlete_id
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      ath <- raw[["athlete"]] %||% raw

      # Helper: extract a 1-row data.frame of selected keys from either a
      # data.frame (legacy site.api shape) or a named list (current core.api
      # shape). Skips list-valued fields ($ref siblings, etc.).
      .extract_row <- function(obj, keep) {
        if (is.null(obj)) return(data.frame(stringsAsFactors = FALSE))
        if (is.data.frame(obj)) {
          cols <- intersect(keep, colnames(obj))
          if (length(cols) == 0) return(data.frame(stringsAsFactors = FALSE))
          return(as.data.frame(obj[cols], stringsAsFactors = FALSE))
        }
        if (is.list(obj)) {
          present <- intersect(keep, names(obj))
          scalars <- Filter(function(v) length(v) == 1 && !is.list(v), obj[present])
          if (length(scalars) == 0) return(data.frame(stringsAsFactors = FALSE))
          return(as.data.frame(scalars, stringsAsFactors = FALSE))
        }
        data.frame(stringsAsFactors = FALSE)
      }

      # ---------- Bio ----------
      bio_keep <- c(
        "id", "uid", "guid", "firstName", "lastName", "fullName",
        "displayName", "shortName", "weight", "displayWeight",
        "height", "displayHeight", "age", "dateOfBirth",
        "debutYear", "jersey", "active"
      )
      bio_data <- as.list(.extract_row(ath, bio_keep))
      hs <- ath[["headshot"]]
      if (!is.null(hs)) {
        bio_data[["headshot_href"]] <- if (is.data.frame(hs)) hs[["href"]][[1]] %||% NA_character_
                                       else hs[["href"]] %||% NA_character_
      }
      bp <- ath[["birthPlace"]]
      if (!is.null(bp)) {
        get_bp <- function(k) {
          v <- if (is.data.frame(bp)) bp[[k]][[1]] else bp[[k]]
          v %||% NA_character_
        }
        bio_data[["birth_city"]]    <- get_bp("city")
        bio_data[["birth_state"]]   <- get_bp("state")
        bio_data[["birth_country"]] <- get_bp("country")
      }
      result[["Bio"]] <- data.frame(bio_data, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Bio from ESPN.com"),
          Sys.time()
        )

      # ---------- Team ----------
      team_keep <- c("id", "uid", "slug", "abbreviation", "displayName",
                     "shortDisplayName", "name", "location", "color", "$ref")
      team_df <- .extract_row(ath[["team"]], team_keep)
      # Rename "$ref" -> "ref" before clean_names() to avoid problematic glyph
      if ("$ref" %in% colnames(team_df)) {
        names(team_df)[names(team_df) == "$ref"] <- "ref"
      }
      result[["Team"]] <- team_df %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Team from ESPN.com"),
          Sys.time()
        )

      # ---------- Position ----------
      pos_keep <- c("id", "name", "displayName", "abbreviation", "leaf")
      result[["Position"]] <- .extract_row(ath[["position"]], pos_keep) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Position from ESPN.com"),
          Sys.time()
        )

      # ---------- Status ----------
      stat_keep <- c("id", "name", "type", "abbreviation")
      result[["Status"]] <- .extract_row(ath[["status"]], stat_keep) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Status from ESPN.com"),
          Sys.time()
        )

      # ---------- College ----------
      coll_keep <- c("id", "mascot", "name", "shortName", "abbrev", "$ref")
      college_df <- .extract_row(ath[["college"]], coll_keep)
      if ("$ref" %in% colnames(college_df)) {
        names(college_df)[names(college_df) == "$ref"] <- "ref"
      }
      result[["College"]] <- college_df %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete College from ESPN.com"),
          Sys.time()
        )

      # ---------- Draft ----------
      draft_df <- data.frame(stringsAsFactors = FALSE)
      d_obj <- ath[["draft"]]
      if (!is.null(d_obj) && (is.list(d_obj) || is.data.frame(d_obj))) {
        get_d <- function(k) {
          v <- if (is.data.frame(d_obj)) d_obj[[k]][[1]] else d_obj[[k]]
          if (is.null(v) || is.list(v)) NA_character_ else as.character(v)
        }
        draft_df <- data.frame(
          year      = get_d("year"),
          round     = get_d("round"),
          selection = get_d("selection"),
          display_text = get_d("displayText"),
          stringsAsFactors = FALSE
        )
      }
      result[["Draft"]] <- draft_df %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Draft from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN ", league,
                    " athlete info for athlete_id=", athlete_id),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN ", league,
                    " athlete info for athlete_id=", athlete_id),
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_overview  (2.2)
# web-common-v3 /athletes/{athlete_id}/overview?season={year}
# Returns named list: Statistics, NextGame, Last5Games, Headlines, FantasyOutlook
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete overview
#'
#' Fetches
#' `site.web.api.espn.com/apis/common/v3/sports/baseball/{league}/athletes/{athlete_id}/overview`
#' and returns a named list of tibbles.
#'
#' @param league character.
#' @param athlete_id character or numeric.
#' @param season numeric. Season year.
#' @param ... Unused.
#' @return Named list of data frames.
#' @noRd
.espn_baseball_athlete_overview <- function(league, athlete_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id, season = season)

  result <- list()

  url <- paste0(
    "https://site.web.api.espn.com/apis/common/v3/sports/baseball/",
    league,
    "/athletes/",
    athlete_id,
    "/overview?season=",
    season
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      # ---------- Statistics ----------
      stats_df <- data.frame(stringsAsFactors = FALSE)
      stat_raw <- raw[["statistics"]] %||% raw[["stats"]]
      if (!is.null(stat_raw) && is.data.frame(stat_raw) && nrow(stat_raw) > 0) {
        stats_df <- stat_raw %>%
          data.frame(stringsAsFactors = FALSE)
      }
      result[["Statistics"]] <- stats_df %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Overview Statistics from ESPN.com"),
          Sys.time()
        )

      # ---------- NextGame ----------
      next_df <- data.frame(stringsAsFactors = FALSE)
      ng_raw <- raw[["nextOpponent"]] %||% raw[["nextGame"]] %||% raw[["nextEvent"]]
      if (!is.null(ng_raw) && is.data.frame(ng_raw) && nrow(ng_raw) > 0) {
        ng_keep <- c("id", "date", "name", "shortName")
        ng_data <- ng_raw[intersect(ng_keep, colnames(ng_raw))]
        next_df <- data.frame(ng_data, stringsAsFactors = FALSE)
      } else if (!is.null(ng_raw) && is.list(ng_raw)) {
        next_df <- data.frame(
          id         = as.character(ng_raw[["id"]] %||% NA_character_),
          date       = as.character(ng_raw[["date"]] %||% NA_character_),
          name       = as.character(ng_raw[["name"]] %||% NA_character_),
          short_name = as.character(ng_raw[["shortName"]] %||% NA_character_),
          stringsAsFactors = FALSE
        )
      }
      result[["NextGame"]] <- next_df %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Overview NextGame from ESPN.com"),
          Sys.time()
        )

      # ---------- Last5Games ----------
      last5_df <- data.frame(stringsAsFactors = FALSE)
      l5_raw <- raw[["last5Games"]] %||% raw[["recentGames"]] %||% raw[["gameLog"]]
      if (!is.null(l5_raw) && is.data.frame(l5_raw) && nrow(l5_raw) > 0) {
        last5_df <- l5_raw %>% data.frame(stringsAsFactors = FALSE)
      }
      result[["Last5Games"]] <- last5_df %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Overview Last5Games from ESPN.com"),
          Sys.time()
        )

      # ---------- Headlines ----------
      hdl_df <- data.frame(stringsAsFactors = FALSE)
      hdl_raw <- raw[["news"]] %||% raw[["headlines"]] %||% raw[["articles"]]
      if (!is.null(hdl_raw) && is.data.frame(hdl_raw) && nrow(hdl_raw) > 0) {
        hdl_keep <- c("headline", "description", "published", "byline", "type")
        hdl_df <- hdl_raw %>%
          dplyr::select(dplyr::any_of(hdl_keep)) %>%
          data.frame(stringsAsFactors = FALSE)
      }
      result[["Headlines"]] <- hdl_df %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Overview Headlines from ESPN.com"),
          Sys.time()
        )

      # ---------- FantasyOutlook ----------
      fant_df <- data.frame(stringsAsFactors = FALSE)
      fo_raw <- raw[["fantasyOutlook"]] %||% raw[["fantasy"]]
      if (!is.null(fo_raw) && is.data.frame(fo_raw) && nrow(fo_raw) > 0) {
        fant_df <- fo_raw %>% data.frame(stringsAsFactors = FALSE)
      } else if (!is.null(fo_raw) && is.list(fo_raw)) {
        fant_df <- data.frame(
          outlook = as.character(fo_raw[["outlook"]] %||% NA_character_),
          stringsAsFactors = FALSE
        )
      }
      result[["FantasyOutlook"]] <- fant_df %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Overview FantasyOutlook from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN ", league,
                    " athlete overview for athlete_id=", athlete_id,
                    ", season=", season),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN ", league,
                    " athlete overview for athlete_id=", athlete_id),
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_stats  (2.3)
# web-common-v3 /athletes/{athlete_id}/stats?season={year}
# Returns named list per category: General, Offensive, Defensive, Rebounding,
#   Shooting, Misc
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete stats
#'
#' Fetches
#' `site.web.api.espn.com/apis/common/v3/sports/baseball/{league}/athletes/{athlete_id}/stats`
#' and returns a named list of per-category tibbles.
#'
#' @param league character.
#' @param athlete_id character or numeric.
#' @param season numeric.
#' @param ... Unused.
#' @return Named list of data frames, one per stats category.
#' @noRd
.espn_baseball_athlete_stats <- function(league, athlete_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id, season = season)

  result <- list()

  url <- paste0(
    "https://site.web.api.espn.com/apis/common/v3/sports/baseball/",
    league,
    "/athletes/",
    athlete_id,
    "/stats?season=",
    season
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      categories_raw <- raw[["categories"]] %||% raw[["statCategories"]]

      # Category name mapping: we expect up to 6 categories from the API.
      # If categories is absent or empty, return list with 6 empty tibbles.
      default_cats <- c("General", "Offensive", "Defensive",
                        "Rebounding", "Shooting", "Misc")

      empty_tbl <- data.frame(stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Athlete Stats from ESPN.com"),
          Sys.time()
        )

      if (is.null(categories_raw) ||
          (!is.data.frame(categories_raw) && !is.list(categories_raw))) {
        for (cat_name in default_cats) result[[cat_name]] <- empty_tbl
        return(result)
      }

      # categories_raw may be a data frame (one row per category) or a list
      n_cats <- if (is.data.frame(categories_raw)) nrow(categories_raw)
                else length(categories_raw)

      for (i in seq_len(n_cats)) {
        cat_row <- if (is.data.frame(categories_raw)) {
          categories_raw[i, , drop = FALSE]
        } else {
          categories_raw[[i]]
        }

        # Derive a clean category name
        cat_name_raw <- if (is.data.frame(cat_row)) {
          cat_row[["displayName"]][[1]] %||%
            cat_row[["name"]][[1]] %||%
            paste0("Category", i)
        } else {
          cat_row[["displayName"]] %||% cat_row[["name"]] %||% paste0("Category", i)
        }
        # Normalize to title-case ASCII name matching expected defaults
        cat_name <- as.character(cat_name_raw)

        # stats may be a data frame or a list element named "stats"
        stats_raw <- if (is.data.frame(cat_row)) {
          cat_row[["stats"]][[1]] %||% cat_row[["statistics"]][[1]]
        } else {
          cat_row[["stats"]] %||% cat_row[["statistics"]]
        }

        if (is.null(stats_raw) || length(stats_raw) == 0) {
          result[[cat_name]] <- empty_tbl
          next
        }

        # stats_raw can be a data frame with name/value columns,
        # or a named numeric vector
        cat_df <- tryCatch({
          if (is.data.frame(stats_raw)) {
            stats_raw %>%
              data.frame(stringsAsFactors = FALSE) %>%
              dplyr::as_tibble() %>%
              janitor::clean_names() %>%
              make_baseballr_data(
                paste0("ESPN ", toupper(league), " Athlete Stats from ESPN.com"),
                Sys.time()
              )
          } else if (is.numeric(stats_raw) || is.character(stats_raw)) {
            # Flatten named vector into two columns
            nm <- names(stats_raw)
            if (is.null(nm)) nm <- paste0("stat_", seq_along(stats_raw))
            data.frame(
              stat_name  = nm,
              stat_value = as.character(stats_raw),
              stringsAsFactors = FALSE
            ) %>%
              dplyr::as_tibble() %>%
              make_baseballr_data(
                paste0("ESPN ", toupper(league), " Athlete Stats from ESPN.com"),
                Sys.time()
              )
          } else {
            empty_tbl
          }
        }, error = function(.e) empty_tbl)

        result[[cat_name]] <- cat_df
      }

      # Ensure canonical default slots are always present
      for (cat_name in default_cats) {
        if (is.null(result[[cat_name]])) result[[cat_name]] <- empty_tbl
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN ", league,
                    " athlete stats for athlete_id=", athlete_id,
                    ", season=", season),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN ", league,
                    " athlete stats for athlete_id=", athlete_id),
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_gamelog  (2.4)
# web-common-v3 /athletes/{athlete_id}/gamelog?season={year}
# Returns single tibble (one row per game)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete gamelog
#'
#' Fetches
#' `site.web.api.espn.com/apis/common/v3/sports/baseball/{league}/athletes/{athlete_id}/gamelog`
#' and returns a single tidy tibble (one row per game).
#'
#' @param league character.
#' @param athlete_id character or numeric.
#' @param season numeric.
#' @param ... Unused.
#' @return A tibble.
#' @noRd
.espn_baseball_athlete_gamelog <- function(league, athlete_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Athlete Gamelog from ESPN.com"))

  url <- paste0(
    "https://site.web.api.espn.com/apis/common/v3/sports/baseball/",
    league,
    "/athletes/",
    athlete_id,
    "/gamelog?season=",
    season
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      # The gamelog payload nests stats under events/categories.
      # Attempt to parse the flat events table first; fall back to
      # stats[[]] if the response shape differs.
      events_raw <- raw[["events"]] %||% raw[["games"]] %||% raw[["gameLog"]]
      labels_raw <- raw[["labels"]] %||% raw[["statNames"]] %||% raw[["names"]]

      if (is.null(events_raw) ||
          (!is.data.frame(events_raw) && !is.list(events_raw))) {
        result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
      } else {
        if (is.data.frame(events_raw) && nrow(events_raw) > 0) {
          result <- events_raw %>%
            data.frame(stringsAsFactors = FALSE) %>%
            dplyr::as_tibble() %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              paste0("ESPN ", toupper(league), " Athlete Gamelog from ESPN.com"),
              Sys.time()
            )
        } else if (is.list(events_raw) && length(events_raw) > 0) {
          # Each element is keyed by event id; the value is a list with
          # per-game metadata (gameDate, opponent, score, atVs, gameResult,
          # eventNote, leagueName, team, week, links, etc.). Stats themselves
          # live in raw$seasonTypes and are merged separately if present.
          rows <- lapply(seq_along(events_raw), function(i) {
            ev <- events_raw[[i]]
            if (!is.list(ev)) return(NULL)

            event_id <- as.character(
              ev[["eventId"]] %||% ev[["id"]] %||%
                names(events_raw)[i] %||% NA_character_
            )

            # Capture scalar (length-1, non-list) fields verbatim.
            scalars <- Filter(function(v) length(v) == 1 && !is.list(v), ev)
            row_df <- if (length(scalars) > 0) {
              as.data.frame(lapply(scalars, function(v) as.character(v[[1]])),
                            stringsAsFactors = FALSE)
            } else {
              data.frame(stringsAsFactors = FALSE)
            }

            # Pull common nested 1-row objects (team, opponent) onto the row.
            for (nm in c("team", "opponent")) {
              sub <- ev[[nm]]
              if (is.list(sub) && !is.data.frame(sub)) {
                sub_scalars <- Filter(function(v) length(v) == 1 && !is.list(v), sub)
                if (length(sub_scalars) > 0) {
                  add <- as.data.frame(
                    lapply(sub_scalars, function(v) as.character(v[[1]])),
                    stringsAsFactors = FALSE
                  )
                  names(add) <- paste0(nm, "_", names(add))
                  row_df <- if (nrow(row_df) > 0) cbind(row_df, add) else add
                }
              }
            }

            # Optionally append stats vector if it's bundled inline with labels.
            stats_v <- ev[["stats"]] %||% ev[["values"]]
            if (!is.null(stats_v) && !is.null(labels_raw) &&
                length(labels_raw) == length(stats_v)) {
              stat_cols <- as.list(as.character(stats_v))
              names(stat_cols) <- as.character(labels_raw)
              stat_df <- as.data.frame(stat_cols, stringsAsFactors = FALSE)
              row_df <- if (nrow(row_df) > 0) cbind(row_df, stat_df) else stat_df
            }

            if (nrow(row_df) == 0) {
              row_df <- data.frame(stringsAsFactors = FALSE)
            }
            row_df[["event_id"]] <- event_id
            row_df
          })
          rows <- Filter(Negate(is.null), rows)
          if (length(rows) > 0) {
            result <- dplyr::bind_rows(rows) %>%
              dplyr::as_tibble() %>%
              janitor::clean_names() %>%
              make_baseballr_data(
                paste0("ESPN ", toupper(league), " Athlete Gamelog from ESPN.com"),
                Sys.time()
              )
          } else {
            result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
          }
        } else {
          result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
        }
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN ", league,
                    " athlete gamelog for athlete_id=", athlete_id,
                    ", season=", season),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN ", league,
                    " athlete gamelog for athlete_id=", athlete_id),
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_splits  (2.5)
# web-common-v3 /athletes/{athlete_id}/splits?season={year}
# Returns single tibble (long-format splits)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete splits
#'
#' Fetches
#' `site.web.api.espn.com/apis/common/v3/sports/baseball/{league}/athletes/{athlete_id}/splits`
#' and returns a single long-format tibble.
#'
#' @param league character.
#' @param athlete_id character or numeric.
#' @param season numeric.
#' @param ... Unused.
#' @return A tibble.
#' @noRd
.espn_baseball_athlete_splits <- function(league, athlete_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Athlete Splits from ESPN.com"))

  url <- paste0(
    "https://site.web.api.espn.com/apis/common/v3/sports/baseball/",
    league,
    "/athletes/",
    athlete_id,
    "/splits?season=",
    season
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      splits_raw <- raw[["splitCategories"]] %||% raw[["splits"]] %||%
                      raw[["categories"]] %||% raw[["data"]]
      labels_raw <- raw[["labels"]] %||% raw[["names"]] %||% raw[["statNames"]]

      if (is.null(splits_raw)) {
        result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
      } else if (is.data.frame(splits_raw) && nrow(splits_raw) > 0) {
        result <- splits_raw %>%
          data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          janitor::clean_names() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Athlete Splits from ESPN.com"),
            Sys.time()
          )
      } else if (is.list(splits_raw) && length(splits_raw) > 0) {
        # Flatten each split category into long rows
        rows <- lapply(splits_raw, function(cat_item) {
          cat_name   <- as.character(cat_item[["displayName"]] %||%
                                      cat_item[["name"]] %||% NA_character_)
          splits_lst <- cat_item[["splits"]] %||% cat_item[["rows"]]

          if (is.null(splits_lst) || length(splits_lst) == 0) {
            return(data.frame(category = cat_name, stringsAsFactors = FALSE))
          }

          if (is.data.frame(splits_lst)) {
            cbind(
              data.frame(category = rep(cat_name, nrow(splits_lst)),
                         stringsAsFactors = FALSE),
              splits_lst
            )
          } else if (is.list(splits_lst)) {
            inner_rows <- lapply(splits_lst, function(sp) {
              split_name <- as.character(sp[["displayName"]] %||%
                                          sp[["name"]] %||% NA_character_)
              stats_v    <- sp[["stats"]] %||% sp[["values"]]
              if (!is.null(stats_v) && !is.null(labels_raw) &&
                  length(labels_raw) == length(stats_v)) {
                row_vals <- as.list(as.character(stats_v))
                names(row_vals) <- as.character(labels_raw)
                row_vals[["category"]]   <- cat_name
                row_vals[["split_name"]] <- split_name
                data.frame(row_vals, stringsAsFactors = FALSE)
              } else {
                data.frame(category   = cat_name,
                           split_name = split_name,
                           stringsAsFactors = FALSE)
              }
            })
            dplyr::bind_rows(inner_rows)
          } else {
            data.frame(category = cat_name, stringsAsFactors = FALSE)
          }
        })
        rows <- Filter(Negate(is.null), rows)
        if (length(rows) > 0) {
          result <- dplyr::bind_rows(rows) %>%
            dplyr::as_tibble() %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              paste0("ESPN ", toupper(league), " Athlete Splits from ESPN.com"),
              Sys.time()
            )
        } else {
          result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
        }
      } else {
        result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN ", league,
                    " athlete splits for athlete_id=", athlete_id,
                    ", season=", season),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN ", league,
                    " athlete splits for athlete_id=", athlete_id),
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_eventlog  (2.6)
# core-v2 /athletes/{athlete_id}/eventlog?season={year}
# Returns single tibble; $ref URLs returned as character columns, not resolved
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete eventlog
#'
#' Fetches
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/athletes/{athlete_id}/eventlog`
#' and returns a single tidy tibble. Per-event `statistics.$ref` URLs are
#' returned as a character column `statistics_ref` -- they are NOT resolved.
#'
#' @param league character.
#' @param athlete_id character or numeric.
#' @param season numeric.
#' @param ... Unused.
#' @return A tibble.
#' @noRd
.espn_baseball_athlete_eventlog <- function(league, athlete_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Athlete Eventlog from ESPN.com"))

  # Eventlog is scoped under /seasons/{year}/athletes/{id}/eventlog;
  # the flat /athletes/{id}/eventlog?season= form 404s.
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/seasons/", as.integer(season),
    "/athletes/", athlete_id,
    "/eventlog"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      # core-v2 eventlog returns:
      #   raw$events = {count, pageIndex, pageSize, pageCount, items[]}
      # where items is a data frame with cols
      #   {event, competition, statistics, teamId, played}.
      # The `event/competition/statistics` cols may be character $ref vectors
      # (collapsed by simplifyDataFrame) or 1-col data frames named "$ref".
      events_obj <- raw[["events"]]
      events_raw <- if (is.list(events_obj) && !is.data.frame(events_obj))
                      events_obj[["items"]] %||% events_obj
                    else events_obj
      events_raw <- events_raw %||% raw[["items"]]

      if (is.null(events_raw) ||
          (!is.data.frame(events_raw) && !is.list(events_raw)) ||
          length(events_raw) == 0) {
        result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
      } else {
        if (is.data.frame(events_raw) && nrow(events_raw) > 0) {
          # Flatten $ref columns from nested data frames or scalar URL columns
          ev <- events_raw

          extract_ref <- function(col_name) {
            x <- ev[[col_name]]
            if (is.null(x)) return(rep(NA_character_, nrow(ev)))
            if (is.character(x)) return(x)
            if (is.data.frame(x) && "$ref" %in% colnames(x)) {
              return(as.character(x[["$ref"]]))
            }
            if (is.list(x)) {
              return(vapply(x, function(item) {
                if (is.list(item) && !is.null(item[["$ref"]])) {
                  as.character(item[["$ref"]])
                } else {
                  NA_character_
                }
              }, character(1)))
            }
            rep(NA_character_, nrow(ev))
          }

          flat_df <- data.frame(
            event_ref       = extract_ref("event"),
            competition_ref = extract_ref("competition"),
            team_ref        = extract_ref("team"),
            statistics_ref  = extract_ref("statistics"),
            stringsAsFactors = FALSE
          )

          # Append any plain scalar columns not already in flat_df
          scalar_cols <- setdiff(
            names(ev)[vapply(ev, function(x) {
              !is.data.frame(x) && !is.list(x)
            }, logical(1))],
            names(flat_df)
          )
          if (length(scalar_cols) > 0) {
            flat_df <- cbind(
              flat_df,
              ev[scalar_cols],
              stringsAsFactors = FALSE
            )
          }

          result <- flat_df %>%
            dplyr::as_tibble() %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              paste0("ESPN ", toupper(league), " Athlete Eventlog from ESPN.com"),
              Sys.time()
            )
        } else {
          # list form
          rows <- lapply(events_raw, function(ev_item) {
            ev_ref   <- ev_item[["event"]][["$ref"]] %||%
                        ev_item[["event"]]           %||% NA_character_
            comp_ref <- ev_item[["competition"]][["$ref"]] %||%
                        ev_item[["competition"]]      %||% NA_character_
            team_ref <- ev_item[["team"]][["$ref"]] %||%
                        ev_item[["team"]]            %||% NA_character_
            stats_ref <- ev_item[["statistics"]][["$ref"]] %||%
                         ev_item[["statistics"]]      %||% NA_character_
            data.frame(
              event_ref       = as.character(ev_ref),
              competition_ref = as.character(comp_ref),
              team_ref        = as.character(team_ref),
              statistics_ref  = as.character(stats_ref),
              stringsAsFactors = FALSE
            )
          })
          result <- dplyr::bind_rows(rows) %>%
            dplyr::as_tibble() %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              paste0("ESPN ", toupper(league), " Athlete Eventlog from ESPN.com"),
              Sys.time()
            )
        }
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN ", league,
                    " athlete eventlog for athlete_id=", athlete_id,
                    ", season=", season),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN ", league,
                    " athlete eventlog for athlete_id=", athlete_id),
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_awards  (2.7)
# core-v2 /athletes/{athlete_id}/awards
# Returns single tibble; often empty
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete awards
#'
#' Fetches
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/athletes/{athlete_id}/awards`
#' and returns a single tidy tibble. This endpoint is sparse; many athletes
#' return no data, in which case an empty tibble with canonical columns is
#' returned.
#'
#' @param league character.
#' @param athlete_id character or numeric.
#' @param ... Unused.
#' @return A tibble with columns `season`, `award_id`, `name`, `description`,
#'   `date`, `type`.
#' @noRd
.espn_baseball_athlete_awards <- function(league, athlete_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id)

  empty_awards <- data.frame(
    season      = character(0),
    award_id    = character(0),
    name        = character(0),
    description = character(0),
    date        = character(0),
    type        = character(0),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::as_tibble() %>%
    make_baseballr_data(
      paste0("ESPN ", toupper(league), " Athlete Awards from ESPN.com"),
      Sys.time()
    )

  result <- empty_awards

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/athletes/",
    athlete_id,
    "/awards"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      items_raw <- raw[["items"]] %||% raw[["awards"]]

      if (is.null(items_raw) ||
          (!is.data.frame(items_raw) && !is.list(items_raw)) ||
          length(items_raw) == 0) {
        return(result)  # empty tibble
      }

      if (is.data.frame(items_raw) && nrow(items_raw) > 0) {
        keep_cols <- c("season", "award_id", "id", "name", "displayName",
                       "description", "date", "type", "$ref")
        avail <- intersect(keep_cols, colnames(items_raw))
        award_df <- items_raw[avail] %>%
          data.frame(stringsAsFactors = FALSE, check.names = FALSE)

        # Normalize to canonical column names
        if ("id" %in% colnames(award_df) && !"award_id" %in% colnames(award_df)) {
          award_df[["award_id"]] <- award_df[["id"]]
        }
        if ("displayName" %in% colnames(award_df) && !"name" %in% colnames(award_df)) {
          award_df[["name"]] <- award_df[["displayName"]]
        }
        # Surface $ref URLs (core-v2 often returns ref-only payloads) without
        # auto-resolving them. NA-fill canonical columns so the output schema
        # is stable.
        n <- nrow(award_df)
        ref_url <- if ("$ref" %in% colnames(items_raw)) {
          as.character(items_raw[["$ref"]])
        } else {
          rep(NA_character_, n)
        }
        canonical <- data.frame(
          season      = if ("season" %in% colnames(award_df)) as.character(award_df[["season"]]) else rep(NA_character_, n),
          award_id    = if ("award_id" %in% colnames(award_df)) as.character(award_df[["award_id"]]) else rep(NA_character_, n),
          name        = if ("name" %in% colnames(award_df)) as.character(award_df[["name"]]) else rep(NA_character_, n),
          description = if ("description" %in% colnames(award_df)) as.character(award_df[["description"]]) else rep(NA_character_, n),
          date        = if ("date" %in% colnames(award_df)) as.character(award_df[["date"]]) else rep(NA_character_, n),
          type        = if ("type" %in% colnames(award_df)) as.character(award_df[["type"]]) else rep(NA_character_, n),
          ref_url     = ref_url,
          stringsAsFactors = FALSE
        )

        result <- canonical %>%
          dplyr::as_tibble() %>%
          janitor::clean_names() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Athlete Awards from ESPN.com"),
            Sys.time()
          )
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN ", league,
                    " athlete awards for athlete_id=", athlete_id),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN ", league,
                    " athlete awards for athlete_id=", athlete_id),
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_statisticslog  (2.8)
# core-v2 /athletes/{athlete_id}/statisticslog?season={year}
# Returns single tibble
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete statisticslog
#'
#' Fetches
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/athletes/{athlete_id}/statisticslog`
#' and returns a single tidy tibble.
#'
#' @param league character.
#' @param athlete_id character or numeric.
#' @param season numeric.
#' @param ... Unused.
#' @return A tibble.
#' @noRd
.espn_baseball_athlete_statisticslog <- function(league, athlete_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Athlete Statisticslog from ESPN.com"))

  # ESPN's core-v2 statisticslog endpoint does NOT accept ?season=YYYY
  # (returns 404). It returns the full chronological log; the season arg
  # is retained on the public wrapper signature for API symmetry but only
  # used to filter the resulting frame downstream.
  invisible(season)
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/athletes/",
    athlete_id,
    "/statisticslog"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      entries_raw <- raw[["entries"]] %||% raw[["items"]] %||% raw[["statistics"]]

      if (is.null(entries_raw) ||
          (!is.data.frame(entries_raw) && !is.list(entries_raw)) ||
          length(entries_raw) == 0) {
        result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
      } else if (is.data.frame(entries_raw) && nrow(entries_raw) > 0) {
        result <- entries_raw %>%
          data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          janitor::clean_names() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Athlete Statisticslog from ESPN.com"),
            Sys.time()
          )
      } else if (is.list(entries_raw) && length(entries_raw) > 0) {
        rows <- lapply(entries_raw, function(entry) {
          event_ref <- NA_character_
          stats_ref <- NA_character_

          if (!is.null(entry[["event"]]) && is.list(entry[["event"]])) {
            event_ref <- as.character(entry[["event"]][["$ref"]] %||% NA_character_)
          }
          if (!is.null(entry[["statistics"]]) && is.list(entry[["statistics"]])) {
            stats_ref <- as.character(entry[["statistics"]][["$ref"]] %||% NA_character_)
          }

          # Collect scalar fields
          scalars <- Filter(function(x) !is.list(x) && !is.data.frame(x), entry)
          row_df <- data.frame(
            event_ref      = event_ref,
            statistics_ref = stats_ref,
            stringsAsFactors = FALSE
          )
          if (length(scalars) > 0) {
            sc_df <- data.frame(
              lapply(scalars, function(v) as.character(v[[1]] %||% NA_character_)),
              stringsAsFactors = FALSE
            )
            names(sc_df) <- names(scalars)
            row_df <- cbind(row_df, sc_df)
          }
          row_df
        })
        result <- dplyr::bind_rows(rows) %>%
          dplyr::as_tibble() %>%
          janitor::clean_names() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Athlete Statisticslog from ESPN.com"),
            Sys.time()
          )
      } else {
        result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN ", league,
                    " athlete statisticslog for athlete_id=", athlete_id,
                    ", season=", season),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN ", league,
                    " athlete statisticslog for athlete_id=", athlete_id),
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_contracts (MLB-only data)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athlete contracts index
#'
#' Fetches the paginated contract-year index for one athlete:
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/athletes/{athlete_id}/contracts`.
#' ESPN currently populates contract data only for MLB athletes.
#'
#' @param league character.
#' @param athlete_id character or numeric. ESPN athlete identifier.
#' @param ... Unused.
#' @return A `baseballr_data` tibble with one row per contract year, or `NULL` on error.
#' @noRd
.espn_baseball_athlete_contracts <- function(league, athlete_id, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Athlete Contracts Index"))

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/athletes/", athlete_id, "/contracts?lang=en&region=us"
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
      seasons <- if (length(refs) == 0L) character(0) else
        sub(".*/contracts/([0-9]+).*", "\\1", refs)
      result <- data.frame(
        athlete_id = rep(as.character(athlete_id), length(refs)),
        season     = suppressWarnings(as.integer(seasons)),
        ref        = refs,
        league     = rep(league, length(refs)),
        stringsAsFactors = FALSE
      ) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league),
                 " Athlete Contracts Index"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} athlete contracts for athlete_id={athlete_id}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} athlete contracts for athlete_id={athlete_id}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_athlete_contract (MLB-only data)
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single-year athlete contract
#'
#' Fetches one season's contract for one athlete:
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/athletes/{athlete_id}/contracts/{season}`.
#' Returns a single-row tibble with salary + cap-rule flags + trade
#' protections + the `$ref` URLs for the season and team.
#'
#' @param league character.
#' @param athlete_id character or numeric. ESPN athlete identifier.
#' @param season numeric. Season year (e.g. 2025).
#' @param ... Unused.
#' @return A single-row `baseballr_data` tibble, or `NULL` on error.
#' @noRd
.espn_baseball_athlete_contract <- function(league, athlete_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, athlete_id = athlete_id, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Athlete Contract from ESPN.com"))

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/athletes/", athlete_id, "/contracts/", season,
    "?lang=en&region=us"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

      row <- list(
        athlete_id               = as.character(athlete_id),
        season                   = as.integer(season),
        bird_status              = raw[["birdStatus"]] %||% NA_integer_,
        salary                   = raw[["salary"]] %||% NA_real_,
        salary_remaining         = raw[["salaryRemaining"]] %||% NA_real_,
        years_remaining          = raw[["yearsRemaining"]] %||% NA_integer_,
        incoming_trade_value     = raw[["incomingTradeValue"]] %||% NA_real_,
        outgoing_trade_value     = raw[["outgoingTradeValue"]] %||% NA_real_,
        option_type              = raw[["optionType"]] %||% NA_integer_,
        minimum_salary_exception = raw[["minimumSalaryException"]] %||% NA,
        trade_restriction        = raw[["tradeRestriction"]] %||% NA,
        unsigned_foreign_pick    = raw[["unsignedForeignPick"]] %||% NA,
        active                   = raw[["active"]] %||% NA
      )

      byc <- raw[["baseYearCompensation"]]
      row[["base_year_compensation_active"]] <-
        if (is.list(byc)) byc[["active"]] %||% NA else NA
      ppp <- raw[["poisonPillProvision"]]
      row[["poison_pill_provision_active"]] <-
        if (is.list(ppp)) ppp[["active"]] %||% NA else NA

      tk <- raw[["tradeKicker"]]
      row[["trade_kicker_active"]]      <- if (is.list(tk)) tk[["active"]] %||% NA else NA
      row[["trade_kicker_percentage"]]  <- if (is.list(tk)) tk[["percentage"]] %||% NA_real_ else NA_real_
      row[["trade_kicker_value"]]       <- if (is.list(tk)) tk[["value"]] %||% NA_real_ else NA_real_
      row[["trade_kicker_trade_value"]] <- if (is.list(tk)) tk[["tradeValue"]] %||% NA_real_ else NA_real_

      sref <- raw[["season"]]
      tref <- raw[["team"]]
      row[["season_ref"]] <- if (is.list(sref)) sref[["$ref"]] %||% NA_character_ else NA_character_
      row[["team_ref"]]   <- if (is.list(tref)) tref[["$ref"]] %||% NA_character_ else NA_character_
      row[["team_id"]] <- if (!is.na(row[["team_ref"]]))
        sub(".*/teams/([0-9]+).*", "\\1", row[["team_ref"]]) else NA_character_
      row[["league"]] <- league

      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league),
                 " Athlete Contract from ESPN.com"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} athlete contract for athlete_id={athlete_id}, season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} athlete contract for athlete_id={athlete_id}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}
