# espn_baseball_team_helpers.R
# Internal helpers shared by the ESPN MLB team-detail wrappers
# (`espn_mlb_team_*()`). Each helper takes `league = "mlb"`. None are exported.
# League validators (.espn_baseball_validate_league[_cat]) live in
# R/utils_espn.R (single source of truth, valid = "mlb").

# ---------------------------------------------------------------------------
# .espn_baseball_team
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball team detail
#'
#' Fetches `site.api.espn.com/apis/site/v2/sports/baseball/{league}/teams/{team_id}`
#' and parses the response into a named list of tibbles:
#' `Info`, `Record`, `NextEvent`, `StandingSummary`, `Coaches`.
#'
#' @param league character. `"mlb"`.
#' @param team_id character or numeric. ESPN team identifier.
#' @param season numeric. Season year.
#' @param ... Unused; absorbed for forward compatibility.
#' @return Named list of data frames.
#' @noRd
.espn_baseball_team <- function(league, team_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, team_id = team_id, season = season)

  result <- list()

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/baseball/",
    league,
    "/teams/",
    team_id,
    "?enable=roster,projection,stats&season=",
    season
  )

  tryCatch(
    expr = {
      res  <- .retry_request(url)
      check_status(res)
      raw  <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      team_obj <- raw[["team"]]

      # ---------- Info ----------
      info_keep <- c(
        "id", "uid", "slug", "abbreviation", "displayName",
        "shortDisplayName", "name", "nickname", "location",
        "color", "alternateColor"
      )
      info_data <- team_obj[intersect(info_keep, names(team_obj))]
      # add logo href if available
      if (!is.null(team_obj[["logos"]])) {
        logos <- team_obj[["logos"]]
        if (is.data.frame(logos) && "href" %in% colnames(logos)) {
          info_data[["logo"]] <- logos$href[[1]]
          if (nrow(logos) >= 2) info_data[["logo_dark"]] <- logos$href[[2]]
        }
      }
      info_df <- data.frame(info_data, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " Team Info from ESPN.com"),
          Sys.time()
        )
      result[["Info"]] <- info_df

      # ---------- Record ----------
      record_df <- data.frame(stringsAsFactors = FALSE)
      if (!is.null(team_obj[["record"]])) {
        rec <- team_obj[["record"]]
        if (!is.null(rec[["items"]]) && is.data.frame(rec[["items"]])) {
          record_df <- rec[["items"]] %>%
            data.frame(stringsAsFactors = FALSE) %>%
            dplyr::as_tibble() %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              paste0("ESPN ", toupper(league), " Team Record from ESPN.com"),
              Sys.time()
            )
        }
      }
      result[["Record"]] <- record_df

      # ---------- NextEvent ----------
      next_event_df <- data.frame(stringsAsFactors = FALSE)
      if (!is.null(team_obj[["nextEvent"]])) {
        ne <- team_obj[["nextEvent"]]
        if (is.data.frame(ne) && nrow(ne) > 0) {
          ne_flat <- ne %>%
            dplyr::select(dplyr::any_of(c("id", "uid", "date", "name", "shortName"))) %>%
            data.frame(stringsAsFactors = FALSE)
          next_event_df <- ne_flat %>%
            dplyr::as_tibble() %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              paste0("ESPN ", toupper(league), " Team Next Event from ESPN.com"),
              Sys.time()
            )
        }
      }
      result[["NextEvent"]] <- next_event_df

      # ---------- StandingSummary ----------
      standing_df <- data.frame(stringsAsFactors = FALSE)
      if (!is.null(team_obj[["standingSummary"]])) {
        ss <- team_obj[["standingSummary"]]
        if (is.character(ss)) {
          standing_df <- data.frame(standing_summary = ss, stringsAsFactors = FALSE)
        }
        standing_df <- standing_df %>%
          dplyr::as_tibble() %>%
          janitor::clean_names() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Standing Summary from ESPN.com"),
            Sys.time()
          )
      }
      result[["StandingSummary"]] <- standing_df

      # ---------- Coaches ----------
      coaches_df <- data.frame(stringsAsFactors = FALSE)
      coaches_raw <- team_obj[["coaches"]] %||% team_obj[["coach"]]
      if (!is.null(coaches_raw) && is.data.frame(coaches_raw) && nrow(coaches_raw) > 0) {
        coaches_df <- coaches_raw %>%
          data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          janitor::clean_names() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Coaches from ESPN.com"),
            Sys.time()
          )
      }
      result[["Coaches"]] <- coaches_df
    },
    error   = function(e) .report_api_error(
      e,
      hint = "No team data available for team_id={team_id}, season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning fetching team data for team_id={team_id}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_team_roster
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball team roster
#'
#' Fetches `site.api.espn.com/apis/site/v2/sports/baseball/{league}/teams/{team_id}/roster`
#' and returns a single tidy tibble of athlete rows.
#'
#' @param league character.
#' @param team_id character or numeric.
#' @param season numeric.
#' @param ... Unused.
#' @return A tibble.
#' @noRd
.espn_baseball_team_roster <- function(league, team_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, team_id = team_id, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Team Roster from ESPN.com"))

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/baseball/",
    league,
    "/teams/",
    team_id,
    "/roster"
  )
  # ESPN returns an empty athletes list when ?season=YYYY is appended to the
  # roster endpoint -- it only serves the current/most recent roster. We keep
  # the `season` parameter for API symmetry and attach it as an output column
  # below, but do not pass it on the URL.
  invisible(season)

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      athletes_raw <- raw[["athletes"]]
      if (is.null(athletes_raw)) {
        cli::cli_alert_warning("No athletes in roster response for team_id={team_id}.")
        result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
      } else {
        # athletes can come back as a list of data frames (one per position group)
        # or directly as a data frame
        if (is.list(athletes_raw) && !is.data.frame(athletes_raw)) {
          athletes_raw <- dplyr::bind_rows(
            lapply(athletes_raw, function(g) {
              if (!is.null(g[["items"]]) && is.data.frame(g[["items"]])) g[["items"]]
              else if (is.data.frame(g)) g
              else data.frame()
            })
          )
        }

        if (!is.data.frame(athletes_raw) || nrow(athletes_raw) == 0) {
          result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
        } else {
          # Extract scalar columns
          ath <- athletes_raw

          athlete_id    <- as.character(ath[["id"]] %||% NA_character_)
          full_name     <- as.character(ath[["fullName"]] %||% NA_character_)
          jersey        <- as.character(ath[["jersey"]] %||% NA_character_)
          height        <- as.character(ath[["displayHeight"]] %||% ath[["height"]] %||% NA_character_)
          weight        <- as.character(ath[["displayWeight"]] %||% ath[["weight"]] %||% NA_character_)
          age           <- as.character(ath[["age"]] %||% NA_character_)
          birth_date    <- as.character(ath[["dateOfBirth"]] %||% NA_character_)

          # Nested: position
          pos_abbrev <- NA_character_
          pos_name   <- NA_character_
          if (!is.null(ath[["position"]]) && is.data.frame(ath[["position"]])) {
            pos_abbrev <- as.character(ath[["position"]][["abbreviation"]] %||% NA_character_)
            pos_name   <- as.character(ath[["position"]][["displayName"]] %||% NA_character_)
          }

          # Nested: birthPlace
          birth_place <- NA_character_
          if (!is.null(ath[["birthPlace"]]) && is.data.frame(ath[["birthPlace"]])) {
            bp_city  <- ath[["birthPlace"]][["city"]] %||% ""
            bp_state <- ath[["birthPlace"]][["state"]] %||% ""
            birth_place <- trimws(paste(bp_city, bp_state))
          }

          # Nested: headshot
          headshot <- NA_character_
          if (!is.null(ath[["headshot"]]) && is.data.frame(ath[["headshot"]])) {
            headshot <- as.character(ath[["headshot"]][["href"]] %||% NA_character_)
          }

          # Nested: links -- take first href
          link_web <- NA_character_
          if (!is.null(ath[["links"]]) && is.list(ath[["links"]])) {
            first_link <- tryCatch({
              lnk <- ath[["links"]][[1]]
              if (is.data.frame(lnk)) as.character(lnk[["href"]][[1]]) else NA_character_
            }, error = function(...) NA_character_)
            link_web <- first_link
          }

          # Nested: status
          status <- NA_character_
          if (!is.null(ath[["status"]]) && is.data.frame(ath[["status"]])) {
            status <- as.character(ath[["status"]][["type"]] %||% NA_character_)
          }

          roster_df <- data.frame(
            athlete_id      = athlete_id,
            full_name       = full_name,
            jersey          = jersey,
            position_abbrev = pos_abbrev,
            position_name   = pos_name,
            height          = height,
            weight          = weight,
            age             = age,
            birth_date      = birth_date,
            birth_place     = birth_place,
            headshot        = headshot,
            link_web        = link_web,
            status          = status,
            team_id         = as.character(team_id),
            season          = as.integer(season),
            stringsAsFactors = FALSE
          )

          result <- roster_df %>%
            dplyr::as_tibble() %>%
            make_baseballr_data(
              paste0("ESPN ", toupper(league), " Team Roster from ESPN.com"),
              Sys.time()
            )
        }
      }
    },
    error   = function(e) .report_api_error(
      e,
      hint = "No roster data available for team_id={team_id}, season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning fetching roster for team_id={team_id}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_team_schedule
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball team schedule
#'
#' Fetches
#' `site.api.espn.com/apis/site/v2/sports/baseball/{league}/teams/{team_id}/schedule`
#' and returns a single tidy tibble (one row per event).
#'
#' @param league character.
#' @param team_id character or numeric.
#' @param season numeric.
#' @param season_type integer. 1 = preseason, 2 = regular, 3 = postseason.
#' @param ... Unused.
#' @return A tibble.
#' @noRd
.espn_baseball_team_schedule <- function(league, team_id, season, season_type, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(
    league = league, team_id = team_id,
    season = season, season_type = season_type
  )

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Team Schedule from ESPN.com"))

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/baseball/",
    league,
    "/teams/",
    team_id,
    "/schedule?season=",
    season,
    "&seasontype=",
    season_type
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      events_raw <- raw[["events"]]
      if (is.null(events_raw) || !is.data.frame(events_raw) || nrow(events_raw) == 0) {
        result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
      } else {
        ev <- events_raw

        # scalar fields
        event_id   <- as.character(ev[["id"]] %||% NA_character_)
        ev_name    <- as.character(ev[["name"]] %||% NA_character_)
        short_name <- as.character(ev[["shortName"]] %||% NA_character_)
        date       <- as.character(ev[["date"]] %||% NA_character_)

        # season block
        seas_year <- NA_integer_
        seas_type <- NA_integer_
        if (!is.null(ev[["season"]]) && is.data.frame(ev[["season"]])) {
          seas_year <- as.integer(ev[["season"]][["year"]] %||% NA_integer_)
          seas_type <- as.integer(ev[["season"]][["type"]] %||% NA_integer_)
        }

        # week
        week_num <- NA_integer_
        if (!is.null(ev[["week"]]) && is.data.frame(ev[["week"]])) {
          week_num <- as.integer(ev[["week"]][["number"]] %||% NA_integer_)
        }

        n_events <- nrow(ev)

        # per-event competitor parsing
        opponent_id           <- rep(NA_character_, n_events)
        opponent_abbrev       <- rep(NA_character_, n_events)
        home_away             <- rep(NA_character_, n_events)
        neutral_site          <- rep(NA, n_events)
        conference_comp       <- rep(NA, n_events)
        venue_id              <- rep(NA_character_, n_events)
        venue_name            <- rep(NA_character_, n_events)
        venue_city            <- rep(NA_character_, n_events)
        venue_state           <- rep(NA_character_, n_events)
        broadcast             <- rep(NA_character_, n_events)
        result_txt            <- rep(NA_character_, n_events)
        team_score_col        <- rep(NA_character_, n_events)
        opponent_score_col    <- rep(NA_character_, n_events)
        winner_col            <- rep(NA, n_events)

        competitions_col <- ev[["competitions"]]

        for (i in seq_len(n_events)) {
          comp <- tryCatch({
            if (is.list(competitions_col)) competitions_col[[i]]
            else NULL
          }, error = function(...) NULL)

          if (is.null(comp)) next

          # competitions is a list of one element per event
          comp1 <- if (is.data.frame(comp)) comp[1, , drop = FALSE] else if (is.list(comp)) comp[[1]] else NULL
          if (is.null(comp1)) next

          # neutral site / conference
          neutral_site[i]    <- comp1[["neutralSite"]] %||% NA
          conference_comp[i] <- comp1[["conferenceCompetition"]] %||% NA

          # venue
          if (!is.null(comp1[["venue"]]) && is.data.frame(comp1[["venue"]])) {
            v <- comp1[["venue"]]
            venue_id[i]   <- as.character(v[["id"]][[1]] %||% NA_character_)
            venue_name[i] <- as.character(v[["fullName"]][[1]] %||% NA_character_)
            if (!is.null(v[["address"]]) && is.data.frame(v[["address"]])) {
              venue_city[i]  <- as.character(v[["address"]][["city"]][[1]] %||% NA_character_)
              venue_state[i] <- as.character(v[["address"]][["state"]][[1]] %||% NA_character_)
            }
          }

          # broadcast
          if (!is.null(comp1[["broadcasts"]]) && is.data.frame(comp1[["broadcasts"]])) {
            bc <- comp1[["broadcasts"]]
            if (!is.null(bc[["names"]]) && length(bc[["names"]]) > 0) {
              broadcast[i] <- paste(unlist(bc[["names"]]), collapse = ", ")
            } else if ("market" %in% colnames(bc)) {
              broadcast[i] <- as.character(bc[["market"]][[1]] %||% NA_character_)
            }
          }

          # competitors
          competitors <- comp1[["competitors"]]
          if (is.null(competitors)) next
          if (is.data.frame(competitors) && nrow(competitors) >= 1) {
            # figure out which row is the focal team vs opponent
            team_row <- NA_integer_
            opp_row  <- NA_integer_
            if ("id" %in% colnames(competitors)) {
              team_row <- which(as.character(competitors[["id"]]) == as.character(team_id))
              if (length(team_row) == 0) team_row <- NA_integer_
              opp_row  <- setdiff(seq_len(nrow(competitors)), team_row)
              if (length(opp_row) == 0) opp_row <- NA_integer_
              team_row <- team_row[[1]]
              opp_row  <- opp_row[[1]]
            }

            if (!is.na(opp_row) && "team" %in% colnames(competitors) && is.data.frame(competitors[["team"]])) {
              opp_team <- competitors[["team"]][opp_row, , drop = FALSE]
              opponent_id[i]     <- as.character(opp_team[["id"]][[1]] %||% NA_character_)
              opponent_abbrev[i] <- as.character(opp_team[["abbreviation"]][[1]] %||% NA_character_)
            }

            if (!is.na(team_row) && "homeAway" %in% colnames(competitors)) {
              home_away[i] <- as.character(competitors[["homeAway"]][[team_row]] %||% NA_character_)
            }

            if (!is.na(team_row) && "score" %in% colnames(competitors)) {
              team_score_col[i]     <- as.character(competitors[["score"]][[team_row]] %||% NA_character_)
            }
            if (!is.na(opp_row) && "score" %in% colnames(competitors)) {
              opponent_score_col[i] <- as.character(competitors[["score"]][[opp_row]] %||% NA_character_)
            }

            if (!is.na(team_row) && "winner" %in% colnames(competitors)) {
              winner_col[i] <- as.logical(competitors[["winner"]][[team_row]] %||% NA)
            }
          }

          # result text
          if (!is.null(comp1[["status"]]) && is.data.frame(comp1[["status"]])) {
            st <- comp1[["status"]]
            if (!is.null(st[["type"]]) && is.data.frame(st[["type"]])) {
              result_txt[i] <- as.character(st[["type"]][["description"]][[1]] %||% NA_character_)
            }
          }
        }

        sched_df <- data.frame(
          event_id              = event_id,
          season                = seas_year,
          season_type           = seas_type,
          week                  = week_num,
          date                  = date,
          name                  = ev_name,
          short_name            = short_name,
          opponent_id           = opponent_id,
          opponent_abbrev       = opponent_abbrev,
          home_away             = home_away,
          neutral_site          = as.logical(neutral_site),
          conference_competition = as.logical(conference_comp),
          venue_id              = venue_id,
          venue_name            = venue_name,
          venue_city            = venue_city,
          venue_state           = venue_state,
          broadcast             = broadcast,
          result                = result_txt,
          team_score            = team_score_col,
          opponent_score        = opponent_score_col,
          winner                = as.logical(winner_col),
          stringsAsFactors      = FALSE
        )

        result <- sched_df %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " Team Schedule from ESPN.com"),
            Sys.time()
          )
      }
    },
    error   = function(e) .report_api_error(
      e,
      hint = "No schedule data available for team_id={team_id}, season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning fetching schedule for team_id={team_id}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_team_leaders
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball team leaders
#'
#' Fetches
#' `site.api.espn.com/apis/site/v2/sports/baseball/{league}/teams/{team_id}/leaders`
#' and returns a long-format tibble (one row per category-rank-athlete triple).
#'
#' @param league character.
#' @param team_id character or numeric.
#' @param season numeric.
#' @param ... Unused.
#' @return A tibble.
#' @noRd
.espn_baseball_team_leaders <- function(league, team_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, team_id = team_id, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Team Leaders from ESPN.com"))

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/baseball/",
    league,
    "/teams/",
    team_id,
    "/leaders"
  )
  # ESPN's leaders endpoint does not honor `?season=YYYY`; it serves the
  # current season's leaders. Kept as a parameter for API symmetry, attached
  # as a column on the result below.
  invisible(season)

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyDataFrame = TRUE)

      leaders_raw <- raw[["leaders"]]
      if (is.null(leaders_raw) || !is.data.frame(leaders_raw) || nrow(leaders_raw) == 0) {
        result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
      } else {
        rows <- vector("list", nrow(leaders_raw))
        for (i in seq_len(nrow(leaders_raw))) {
          cat_row     <- leaders_raw[i, , drop = FALSE]
          category    <- as.character(cat_row[["displayName"]] %||% cat_row[["name"]] %||% NA_character_)
          leaders_lst <- cat_row[["leaders"]]

          if (is.null(leaders_lst) || length(leaders_lst) == 0) next

          # leaders_lst may be a list or a data frame
          ldf <- if (is.data.frame(leaders_lst)) leaders_lst
                 else if (is.list(leaders_lst)) leaders_lst[[1]]
                 else NULL

          if (is.null(ldf) || !is.data.frame(ldf) || nrow(ldf) == 0) next

          n_ldrs    <- nrow(ldf)
          ath_id    <- rep(NA_character_, n_ldrs)
          ath_name  <- rep(NA_character_, n_ldrs)
          val       <- rep(NA_real_, n_ldrs)
          rank_col  <- seq_len(n_ldrs)

          if (!is.null(ldf[["value"]])) val <- as.numeric(ldf[["value"]])

          if (!is.null(ldf[["athlete"]]) && is.data.frame(ldf[["athlete"]])) {
            ath_df   <- ldf[["athlete"]]
            ath_id   <- as.character(ath_df[["id"]] %||% NA_character_)
            ath_name <- as.character(ath_df[["displayName"]] %||% ath_df[["fullName"]] %||% NA_character_)
          }

          rows[[i]] <- data.frame(
            team_id      = as.character(team_id),
            season       = as.integer(season),
            category     = category,
            display_name = category,
            athlete_id   = ath_id,
            athlete_name = ath_name,
            value        = val,
            rank         = rank_col,
            stringsAsFactors = FALSE
          )
        }

        rows <- Filter(Negate(is.null), rows)
        if (length(rows) == 0) {
          result <- data.frame(stringsAsFactors = FALSE) %>% dplyr::as_tibble()
        } else {
          result <- dplyr::bind_rows(rows) %>%
            dplyr::as_tibble() %>%
            make_baseballr_data(
              paste0("ESPN ", toupper(league), " Team Leaders from ESPN.com"),
              Sys.time()
            )
        }
      }
    },
    error   = function(e) .report_api_error(
      e,
      hint = "No leaders data available for team_id={team_id}, season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning fetching leaders for team_id={team_id}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# .espn_baseball_team_season_profile
# ---------------------------------------------------------------------------

#' Internal: ESPN baseball team-in-season profile
#'
#' Fetches the core-v2 per-season team profile
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/{league}/seasons/{season}/teams/{team_id}`
#' and returns a single-row tibble of the team's era-correct identity scalars
#' plus the available `$ref` URLs for deeper resources (record, statistics,
#' leaders, coaches, etc.). Older seasons return fewer `$ref` keys; missing
#' refs become `NA_character_`.
#'
#' @param league character. `"mlb"`.
#' @param team_id character or numeric. ESPN team identifier.
#' @param season numeric. Season year (e.g. 2025).
#' @param ... Unused; absorbed for forward compatibility.
#' @return A single-row `baseballr_data` tibble, or `NULL` on error.
#' @noRd
.espn_baseball_team_season_profile <- function(league, team_id, season, ...) {
  .espn_baseball_validate_league(league)
  .args <- list(league = league, team_id = team_id, season = season)

  result <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " Team Season Profile from ESPN.com"))

  url <- paste0(
    "https://sports.core.api.espn.com/v2/sports/baseball/leagues/",
    league, "/seasons/", season, "/teams/", team_id,
    "?lang=en&region=us"
  )

  ref_keys <- c(
    "record", "venue", "groups", "ranks", "statistics", "leaders",
    "injuries", "notes", "againstTheSpreadRecords", "awards",
    "franchise", "depthCharts", "events", "transactions", "coaches",
    "athletes", "oddsRecords", "college"
  )

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      raw <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

      scalar_keys <- c(
        "id", "guid", "uid", "slug", "location", "name", "nickname",
        "abbreviation", "displayName", "shortDisplayName",
        "color", "alternateColor", "isActive", "isAllStar"
      )
      row <- list()
      for (k in scalar_keys) {
        v <- raw[[k]]
        row[[k]] <- if (is.null(v)) NA else v
      }
      row[["season"]] <- as.integer(season)

      # logos: first two hrefs (light + dark, if present)
      logos <- raw[["logos"]]
      logo_hrefs <- if (is.list(logos)) {
        vapply(logos, function(x) x[["href"]] %||% NA_character_,
               character(1))
      } else character(0)
      row[["logo"]]      <- if (length(logo_hrefs) >= 1L) logo_hrefs[1L] else NA_character_
      row[["logo_dark"]] <- if (length(logo_hrefs) >= 2L) logo_hrefs[2L] else NA_character_

      # $ref children — preserve URL for downstream wrappers
      for (k in ref_keys) {
        v <- raw[[k]]
        url_v <- if (is.list(v)) v[["$ref"]] %||% NA_character_ else NA_character_
        row[[paste0(k, "_ref")]] <- url_v
      }

      result <- data.frame(row, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league),
                 " Team Season Profile from ESPN.com"),
          Sys.time()
        )
    },
    error   = function(e) .report_api_error(
      e,
      hint = "Failed to retrieve ESPN {league} team-season profile for team_id={team_id}, season={season}",
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = "Warning retrieving ESPN {league} team-season profile for team_id={team_id}",
      args = .args
    ),
    finally = {}
  )
  return(result)
}
