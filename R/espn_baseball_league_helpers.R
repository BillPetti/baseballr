# espn_baseball_league_helpers.R
# Internal helpers shared by the ESPN MLB league-wide catalog wrappers
# (`espn_mlb_*()`). Each helper takes `league = "mlb"`. None are exported.
# League validators (.espn_baseball_validate_league[_cat]) live in
# R/utils_espn.R (single source of truth, valid = "mlb").

# ---------------------------------------------------------------------------
# .espn_baseball_leaders
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball league-wide statistical leaders
#'
#' Fetches `site.web.api.espn.com/apis/common/v3/sports/baseball/{league}/statistics/byathlete`
#' for the given season and season type and returns a flat tibble of leaders
#' (one row per category-athlete pair).
#'
#' @param league character. `"mlb"`.
#' @param season numeric. Season year.
#' @param season_type integer. 1 = preseason, 2 = regular (default), 3 = postseason.
#' @param ... Unused; reserved for forward compatibility.
#' @return A `baseballr_data` tibble, or `NULL` on error.
#' @noRd
.espn_baseball_leaders <- function(league, season, season_type = 2, ...) {
  .espn_baseball_validate_league_cat(league)
  .args <- list(league = league, season = season, season_type = season_type)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Leaders from ESPN.com"))

  # core-v2 leaders endpoint. The web-common-v3 statistics/byathlete URL the
  # gist documents does not exist for MLB baseball -- it 404s. The
  # core-v2 path is shape-compatible (categories[] -> leaders[] with athlete
  # and team as $ref URLs) and is what ESPN's own UI calls.
  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/seasons/", season,
    "/types/", season_type,
    "/leaders"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      # The response has $categories, each with $leaders
      cats_raw <- raw[["categories"]]

      if (is.null(cats_raw) || !is.data.frame(cats_raw) || nrow(cats_raw) == 0) {
        result <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Leaders from ESPN.com"),
            Sys.time()
          )
        return(result)
      }

      rows <- vector("list", nrow(cats_raw))
      for (i in seq_len(nrow(cats_raw))) {
        cat_row     <- cats_raw[i, , drop = FALSE]
        cat_name    <- as.character(cat_row[["name"]] %||% NA_character_)
        cat_abbrev  <- as.character(cat_row[["abbreviation"]] %||% NA_character_)
        leaders_lst <- cat_row[["leaders"]]

        if (is.null(leaders_lst) || length(leaders_lst) == 0) next

        ldf <- if (is.data.frame(leaders_lst)) {
          leaders_lst
        } else if (is.list(leaders_lst)) {
          leaders_lst[[1]]
        } else {
          NULL
        }

        if (is.null(ldf) || !is.data.frame(ldf) || nrow(ldf) == 0) next

        n_rows        <- nrow(ldf)
        athlete_id    <- rep(NA_character_, n_rows)
        athlete_name  <- rep(NA_character_, n_rows)
        team_id       <- rep(NA_character_, n_rows)
        team_abbrev   <- rep(NA_character_, n_rows)
        val           <- rep(NA_real_,      n_rows)
        rank_col      <- seq_len(n_rows)
        display_val   <- rep(NA_character_, n_rows)

        if (!is.null(ldf[["value"]])) val <- as.numeric(ldf[["value"]])
        if (!is.null(ldf[["displayValue"]])) display_val <- as.character(ldf[["displayValue"]])
        if (!is.null(ldf[["rank"]])) rank_col <- as.integer(ldf[["rank"]])

        # core-v2 returns athlete/team as $ref URLs only, no inline data.
        # Extract IDs from the URL pattern: .../athletes/{id}?...
        extract_id_from_ref <- function(refs, kind) {
          if (is.null(refs)) return(rep(NA_character_, n_rows))
          ref_chr <- if (is.data.frame(refs)) {
            as.character(refs[["$ref"]] %||% rep(NA_character_, n_rows))
          } else if (is.list(refs)) {
            vapply(refs, function(r) {
              if (is.list(r)) as.character(r[["$ref"]] %||% NA_character_)
              else if (is.character(r)) r else NA_character_
            }, character(1))
          } else if (is.character(refs)) {
            refs
          } else {
            rep(NA_character_, n_rows)
          }
          pat <- paste0("/", kind, "/(\\d+)")
          out <- rep(NA_character_, length(ref_chr))
          m <- regmatches(ref_chr, regexec(pat, ref_chr))
          out <- vapply(m, function(x) if (length(x) >= 2) x[[2]] else NA_character_,
                        character(1))
          out
        }

        if (!is.null(ldf[["athlete"]])) {
          ath <- ldf[["athlete"]]
          # Inline athlete data (legacy web-common-v3 shape)
          if (is.data.frame(ath) && "id" %in% colnames(ath)) {
            athlete_id   <- as.character(ath[["id"]] %||% NA_character_)
            athlete_name <- as.character(
              ath[["displayName"]] %||% ath[["fullName"]] %||% NA_character_
            )
          } else {
            # Ref-only athlete (core-v2 shape)
            athlete_id   <- extract_id_from_ref(ath, "athletes")
            athlete_name <- rep(NA_character_, n_rows)
          }
        }

        if (!is.null(ldf[["team"]])) {
          tm <- ldf[["team"]]
          if (is.data.frame(tm) && "id" %in% colnames(tm)) {
            team_id     <- as.character(tm[["id"]] %||% NA_character_)
            team_abbrev <- as.character(tm[["abbreviation"]] %||% NA_character_)
          } else {
            team_id     <- extract_id_from_ref(tm, "teams")
            team_abbrev <- rep(NA_character_, n_rows)
          }
        }

        rows[[i]] <- data.frame(
          season         = as.integer(season),
          season_type    = as.integer(season_type),
          category       = cat_name,
          abbreviation   = cat_abbrev,
          athlete_id     = athlete_id,
          athlete_name   = athlete_name,
          team_id        = team_id,
          team_abbrev    = team_abbrev,
          value          = val,
          rank           = rank_col,
          display_value  = display_val,
          stringsAsFactors = FALSE
        )
      }

      rows <- Filter(Negate(is.null), rows)

      if (length(rows) == 0) {
        result <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Leaders from ESPN.com"),
            Sys.time()
          )
      } else {
        result <- dplyr::bind_rows(rows) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Leaders from ESPN.com"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} leaders for season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} leaders for season={season}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_venues
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball venues catalog
#'
#' Fetches `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/venues`
#' with standard core-v2 pagination and returns a flat tibble of venue rows.
#'
#' @param league character. `"mlb"`.
#' @param ... Unused; reserved for forward compatibility.
#' @return A `baseballr_data` tibble, or `NULL` on error.
#' @noRd
.espn_baseball_venues <- function(league, ...) {
  .espn_baseball_validate_league_cat(league)
  .args <- list(league = league)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Venues from ESPN.com"))

  base_url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/venues?limit=100"
  )

  tryCatch(
    expr = {
      all_rows <- list()
      page_idx <- 1L
      page_cnt <- 1L
      max_pages <- 100L

      while (page_idx <= page_cnt && page_idx <= max_pages) {
        url <- paste0(base_url, "&page=", page_idx)
        res <- .retry_request(url)
        check_status(res)
        raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

        page_cnt <- as.integer(raw[["pageCount"]] %||% 1L)
        items    <- raw[["items"]]

        if (!is.null(items) && is.data.frame(items) && nrow(items) > 0) {
          n_rows      <- nrow(items)
          venue_id    <- as.character(items[["id"]] %||% NA_character_)
          name        <- as.character(items[["name"]] %||% NA_character_)
          full_name   <- as.character(items[["fullName"]] %||% NA_character_)
          capacity    <- as.integer(items[["capacity"]] %||% NA_integer_)
          indoor      <- as.logical(items[["indoor"]] %||% NA)
          grass       <- as.logical(items[["grass"]] %||% NA)

          addr_city   <- rep(NA_character_, n_rows)
          addr_state  <- rep(NA_character_, n_rows)
          if (!is.null(items[["address"]]) && is.data.frame(items[["address"]])) {
            addr <- items[["address"]]
            addr_city  <- as.character(addr[["city"]] %||% NA_character_)
            addr_state <- as.character(addr[["state"]] %||% NA_character_)
          }

          images_url  <- rep(NA_character_, n_rows)
          if (!is.null(items[["images"]]) && is.list(items[["images"]])) {
            images_url <- vapply(items[["images"]], function(img) {
              if (is.data.frame(img) && "href" %in% colnames(img) && nrow(img) > 0) {
                as.character(img[["href"]][[1]] %||% NA_character_)
              } else {
                NA_character_
              }
            }, character(1))
          }

          page_df <- data.frame(
            venue_id      = venue_id,
            name          = name,
            full_name     = full_name,
            address_city  = addr_city,
            address_state = addr_state,
            capacity      = capacity,
            indoor        = indoor,
            grass         = grass,
            images_url    = images_url,
            stringsAsFactors = FALSE
          )
          all_rows[[page_idx]] <- page_df
        }

        page_idx <- page_idx + 1L
      }

      if (length(all_rows) == 0) {
        result <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Venues from ESPN.com"),
            Sys.time()
          )
      } else {
        result <- dplyr::bind_rows(all_rows) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Venues from ESPN.com"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} venues",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} venues",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_coaches
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball coaches catalog
#'
#' Fetches `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/seasons/{year}/coaches`
#' with standard core-v2 pagination and returns a flat tibble with one row per coach.
#'
#' @param league character. `"mlb"`.
#' @param season numeric. Season year.
#' @param ... Unused; reserved for forward compatibility.
#' @return A `baseballr_data` tibble, or `NULL` on error.
#' @noRd
.espn_baseball_coaches <- function(league, season, ...) {
  .espn_baseball_validate_league_cat(league)
  .args <- list(league = league, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Coaches from ESPN.com"))

  base_url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/seasons/",
    season,
    "/coaches?limit=100"
  )

  tryCatch(
    expr = {
      all_rows <- list()
      page_idx <- 1L
      page_cnt <- 1L
      max_pages <- 100L

      while (page_idx <= page_cnt && page_idx <= max_pages) {
        url <- paste0(base_url, "&page=", page_idx)
        res <- .retry_request(url)
        check_status(res)
        raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

        page_cnt <- as.integer(raw[["pageCount"]] %||% 1L)
        items    <- raw[["items"]]

        if (!is.null(items) && is.data.frame(items) && nrow(items) > 0) {
          n_rows     <- nrow(items)
          coach_id   <- as.character(items[["id"]] %||% NA_character_)
          first_name <- as.character(items[["firstName"]] %||% NA_character_)
          last_name  <- as.character(items[["lastName"]] %||% NA_character_)
          full_name  <- as.character(items[["fullName"]] %||% NA_character_)
          experience <- as.integer(items[["experience"]] %||% NA_integer_)

          team_id   <- rep(NA_character_, n_rows)
          if (!is.null(items[["team"]]) && is.data.frame(items[["team"]])) {
            team_id <- as.character(items[["team"]][["id"]] %||% NA_character_)
          }

          page_df <- data.frame(
            coach_id   = coach_id,
            first_name = first_name,
            last_name  = last_name,
            full_name  = full_name,
            experience = experience,
            team_id    = team_id,
            stringsAsFactors = FALSE
          )
          all_rows[[page_idx]] <- page_df
        }

        page_idx <- page_idx + 1L
      }

      if (length(all_rows) == 0) {
        result <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Coaches from ESPN.com"),
            Sys.time()
          )
      } else {
        result <- dplyr::bind_rows(all_rows) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Coaches from ESPN.com"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} coaches for season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} coaches for season={season}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_athletes_index
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball athletes index
#'
#' Fetches `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/seasons/{year}/athletes`
#' with pagination. Progress messages are emitted via `cli::cli_inform()` since
#' some leagues. rosters can exceed 10,000 entries across many pages.
#'
#' @param league character. `"mlb"`.
#' @param season numeric. Season year.
#' @param active logical. Return only active athletes when `TRUE`.
#' @param limit integer. Maximum number of rows to return.
#' @param ... Unused; reserved for forward compatibility.
#' @return A `baseballr_data` tibble, or `NULL` on error.
#' @noRd
.espn_baseball_athletes_index <- function(league, season, active = TRUE,
                                             limit = 25000L, ...) {
  .espn_baseball_validate_league_cat(league)
  .args <- list(
    league = league, season = season, active = active, limit = limit
  )

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Athletes Index from ESPN.com"))

  # The `active` query parameter is NOT supported for baseball leagues
  # (ESPN responds 400 "'active' query param not supported for sport/league").
  # We accept it for API symmetry with other sports but ignore it on the wire.
  base_url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/seasons/",
    season,
    "/athletes?limit=100"
  )

  tryCatch(
    expr = {
      all_rows <- list()
      page_idx <- 1L
      page_cnt <- 1L
      max_pages <- 100L
      total_rows <- 0L

      while (page_idx <= page_cnt && page_idx <= max_pages &&
             total_rows < limit) {
        url <- paste0(base_url, "&page=", page_idx)
        cli::cli_inform(
          "Fetching page {page_idx} of {page_cnt} for {league} athletes (season={season})..."
        )
        res <- .retry_request(url)
        check_status(res)
        raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

        page_cnt <- as.integer(raw[["pageCount"]] %||% 1L)
        items    <- raw[["items"]]

        if (!is.null(items) && is.data.frame(items) && nrow(items) > 0) {
          n_rows <- nrow(items)

          # Detect ref-only payload (core-v2 default): items has only $ref.
          # Extract athlete_id from the URL and surface ref_url so users can
          # follow up with espn_{league}_athlete_info() for full bio data.
          ref_url <- if ("$ref" %in% colnames(items)) {
            as.character(items[["$ref"]])
          } else {
            rep(NA_character_, n_rows)
          }

          athlete_id <- if ("id" %in% colnames(items)) {
            as.character(items[["id"]])
          } else {
            m <- regmatches(ref_url, regexec("/athletes/(\\d+)", ref_url))
            vapply(m, function(x) if (length(x) >= 2) x[[2]] else NA_character_,
                   character(1))
          }

          full_name <- if ("fullName" %in% colnames(items)) {
            as.character(items[["fullName"]])
          } else {
            rep(NA_character_, n_rows)
          }

          jersey <- if ("jersey" %in% colnames(items)) {
            as.character(items[["jersey"]])
          } else {
            rep(NA_character_, n_rows)
          }

          status_col <- rep(NA_character_, n_rows)
          if ("status" %in% colnames(items) && is.data.frame(items[["status"]])) {
            status_col <- as.character(
              items[["status"]][["type"]] %||% rep(NA_character_, n_rows)
            )
          }

          link_col <- ref_url  # default to ref_url; override if links column present
          if ("links" %in% colnames(items) && is.list(items[["links"]])) {
            link_col <- vapply(items[["links"]], function(lnk) {
              if (is.data.frame(lnk) && "href" %in% colnames(lnk) &&
                  nrow(lnk) > 0) {
                as.character(lnk[["href"]][[1]] %||% NA_character_)
              } else {
                NA_character_
              }
            }, character(1))
          }

          position <- rep(NA_character_, n_rows)
          if ("position" %in% colnames(items) && is.data.frame(items[["position"]])) {
            position <- as.character(
              items[["position"]][["abbreviation"]] %||% rep(NA_character_, n_rows)
            )
          }

          team_id <- rep(NA_character_, n_rows)
          if ("team" %in% colnames(items) && is.data.frame(items[["team"]])) {
            team_id <- as.character(
              items[["team"]][["id"]] %||% rep(NA_character_, n_rows)
            )
          }

          headshot <- rep(NA_character_, n_rows)
          if ("headshot" %in% colnames(items) && is.data.frame(items[["headshot"]])) {
            headshot <- as.character(
              items[["headshot"]][["href"]] %||% rep(NA_character_, n_rows)
            )
          }

          page_df <- data.frame(
            athlete_id = athlete_id,
            full_name  = full_name,
            jersey     = jersey,
            position   = position,
            team_id    = team_id,
            headshot   = headshot,
            status     = status_col,
            link       = link_col,
            ref_url    = ref_url,
            stringsAsFactors = FALSE
          )
          all_rows[[page_idx]] <- page_df
          total_rows <- total_rows + n_rows
        }

        page_idx <- page_idx + 1L
      }

      if (length(all_rows) == 0) {
        result <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Athletes Index from ESPN.com"),
            Sys.time()
          )
      } else {
        combined <- dplyr::bind_rows(all_rows)
        # Respect user limit
        if (nrow(combined) > limit) {
          combined <- combined[seq_len(limit), , drop = FALSE]
        }
        result <- combined %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Athletes Index from ESPN.com"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} athletes for season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} athletes for season={season}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_seasons
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball seasons catalog
#'
#' Fetches `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/seasons?limit=200`
#' and returns a flat tibble of season rows.
#'
#' @param league character. `"mlb"`.
#' @param ... Unused; reserved for forward compatibility.
#' @return A `baseballr_data` tibble, or `NULL` on error.
#' @noRd
.espn_baseball_seasons <- function(league, ...) {
  .espn_baseball_validate_league_cat(league)
  .args <- list(league = league)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Seasons from ESPN.com"))

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/seasons?limit=200"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      items <- raw[["items"]]

      if (is.null(items) || !is.data.frame(items) || nrow(items) == 0) {
        result <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Seasons from ESPN.com"),
            Sys.time()
          )
        return(result)
      }

      season_year    <- as.integer(items[["year"]] %||% NA_integer_)
      start_date     <- as.character(items[["startDate"]] %||% NA_character_)
      end_date       <- as.character(items[["endDate"]] %||% NA_character_)
      display_name   <- as.character(items[["displayName"]] %||% NA_character_)
      season_type_ct <- NA_integer_
      if (!is.null(items[["types"]]) && is.data.frame(items[["types"]])) {
        season_type_ct <- as.integer(items[["types"]][["count"]] %||% NA_integer_)
      }

      seasons_df <- data.frame(
        season             = season_year,
        start_date         = start_date,
        end_date           = end_date,
        display_name       = display_name,
        season_type_count  = season_type_ct,
        stringsAsFactors   = FALSE
      )

      result <- seasons_df %>%
        dplyr::as_tibble() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Seasons from ESPN.com"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} seasons",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} seasons",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_season_info
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball single-season info
#'
#' Fetches `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/seasons/{year}`
#' and returns a named list with components `Info`, `Types`, `Athletes`,
#' `Coaches`, `Teams`, and `Awards`. Components that contain only `$ref`
#' URLs are returned as character vectors -- they are NOT auto-resolved.
#'
#' @param league character. `"mlb"`.
#' @param season numeric. Season year.
#' @param ... Unused; reserved for forward compatibility.
#' @return Named list of tibbles/data frames, or an empty list on error.
#' @noRd
.espn_baseball_season_info <- function(league, season, ...) {
  .espn_baseball_validate_league_cat(league)
  .args <- list(league = league, season = season)

  result <- list()

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league,
    "/seasons/",
    season
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      # ---------- Info ----------
      info_keep <- c("year", "startDate", "endDate", "displayName", "type")
      info_vals <- raw[intersect(info_keep, names(raw))]
      # type may be nested
      if (!is.null(raw[["type"]])) {
        t <- raw[["type"]]
        if (is.list(t) && !is.data.frame(t)) {
          info_vals[["type_id"]]   <- as.character(t[["id"]] %||% NA_character_)
          info_vals[["type_name"]] <- as.character(t[["name"]] %||% NA_character_)
          info_vals[["type"]]      <- NULL
        }
      }
      result[["Info"]] <- data.frame(info_vals, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Season Info from ESPN.com"),
          Sys.time()
        )

      # Helper to build a ref-summary tibble for a list component
      .parse_ref_block <- function(block, label) {
        if (is.null(block)) {
          return(
            data.frame(stringsAsFactors = FALSE) %>%
              dplyr::as_tibble() %>%
              make_baseballr_data(
                paste0("ESPN ", toupper(league), " Season ", label, " from ESPN.com"),
                Sys.time()
              )
          )
        }
        cnt <- as.integer(block[["count"]] %||% NA_integer_)
        ref <- as.character(block[["$ref"]] %||% NA_character_)
        data.frame(
          count = cnt,
          ref   = ref,
          stringsAsFactors = FALSE
        ) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Season ", label, " from ESPN.com"),
            Sys.time()
          )
      }

      # ---------- Types ----------
      result[["Types"]] <- .parse_ref_block(raw[["types"]], "Types")

      # ---------- Athletes ----------
      result[["Athletes"]] <- .parse_ref_block(raw[["athletes"]], "Athletes")

      # ---------- Coaches ----------
      result[["Coaches"]] <- .parse_ref_block(raw[["coaches"]], "Coaches")

      # ---------- Teams ----------
      result[["Teams"]] <- .parse_ref_block(raw[["teams"]], "Teams")

      # ---------- Awards ----------
      result[["Awards"]] <- .parse_ref_block(raw[["awards"]], "Awards")
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} season info for season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} season info for season={season}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}
