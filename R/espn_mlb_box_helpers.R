# Baseball-specific box-score parsers for the ESPN MLB game-summary endpoint.
# ESPN's baseball boxscore groups statistics by side (batting / pitching /
# fielding) rather than the flat basketball stat list, so these replace the
# basketball parsers that previously lived in espn_mlb_data.R. Both are called
# by espn_mlb_team_box(), espn_mlb_player_box(), and espn_mlb_game_all().

#' **Parse ESPN MLB Team Box, helper function**
#' @param resp Response object (text) from the ESPN MLB game-summary endpoint.
#' @return Returns a `baseballr_data` tibble (one row per team), or `NULL`.
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows left_join
#' @importFrom janitor clean_names
#' @importFrom lubridate with_tz ymd_hm
#' @keywords internal
#' @export
helper_espn_mlb_team_box <- function(resp) {
  game_json <- jsonlite::fromJSON(resp, simplifyVector = FALSE)
  bx <- game_json[["boxscore"]]
  if (is.null(bx) || is.null(bx[["teams"]]) || length(bx[["teams"]]) < 1) {
    return(NULL)
  }
  header <- game_json[["header"]]
  comp <- header[["competitions"]][[1]]

  game_id <- as.integer(header[["id"]])
  season <- header[["season"]][["year"]]
  season_type <- header[["season"]][["type"]]
  raw_date <- comp[["date"]]
  game_date_time <- lubridate::with_tz(
    lubridate::ymd_hm(substr(raw_date, 1, nchar(raw_date) - 1)),
    tzone = "America/New_York"
  )
  game_date <- as.Date(substr(game_date_time, 1, 10))

  # Per-competitor homeAway / score / winner, keyed by team id.
  competitors <- comp[["competitors"]]
  comp_meta <- dplyr::bind_rows(lapply(competitors, function(c) data.frame(
    team_id     = as.integer(c[["id"]]),
    home_away   = c[["homeAway"]] %||% NA_character_,
    team_score  = suppressWarnings(as.integer(c[["score"]] %||% NA)),
    team_winner = c[["winner"]] %||% NA,
    stringsAsFactors = FALSE
  )))

  teams_box <- bx[["teams"]]
  team_rows <- vector("list", length(teams_box))
  for (t in seq_along(teams_box)) {
    tb <- teams_box[[t]]
    tm <- tb[["team"]]
    base <- data.frame(
      game_id                 = game_id,
      season                  = season,
      season_type             = season_type,
      game_date               = game_date,
      game_date_time          = game_date_time,
      team_id                 = as.integer(tm[["id"]]),
      team_uid                = tm[["uid"]] %||% NA_character_,
      team_slug               = tm[["slug"]] %||% NA_character_,
      team_location           = tm[["location"]] %||% NA_character_,
      team_name               = tm[["name"]] %||% NA_character_,
      team_abbreviation       = tm[["abbreviation"]] %||% NA_character_,
      team_display_name       = tm[["displayName"]] %||% NA_character_,
      team_short_display_name = tm[["shortDisplayName"]] %||% NA_character_,
      team_color              = tm[["color"]] %||% NA_character_,
      team_alternate_color    = tm[["alternateColor"]] %||% NA_character_,
      team_logo               = tm[["logo"]] %||% NA_character_,
      stringsAsFactors = FALSE
    )
    # Flatten batting/pitching/fielding stat groups to <group>_<stat> columns.
    stat_cols <- list()
    for (grp in tb[["statistics"]]) {
      gname <- grp[["name"]]
      if (is.null(gname) || length(grp[["stats"]]) == 0) next
      for (s in grp[["stats"]]) {
        nm <- s[["name"]]
        if (is.null(nm)) next
        stat_cols[[paste0(gname, "_", nm)]] <-
          as.character(s[["displayValue"]] %||% s[["value"]] %||% NA_character_)
      }
    }
    team_rows[[t]] <- if (length(stat_cols) > 0) {
      cbind(base, as.data.frame(stat_cols, stringsAsFactors = FALSE, check.names = FALSE))
    } else {
      base
    }
  }

  out <- dplyr::bind_rows(team_rows)
  out <- dplyr::left_join(out, comp_meta, by = "team_id")

  # Opponent identity (2-team game): graft the other row's team columns.
  if (nrow(out) == 2L) {
    opp_cols <- c("team_id", "team_location", "team_name", "team_abbreviation",
                  "team_display_name", "team_logo", "team_color",
                  "team_alternate_color", "team_score")
    opp <- out[c(2L, 1L), opp_cols, drop = FALSE]
    names(opp) <- paste0("opponent_", names(opp))
    rownames(opp) <- NULL
    out <- cbind(out, opp)
  }

  out |>
    janitor::clean_names() |>
    make_baseballr_data("ESPN MLB Team Box Information from ESPN.com", Sys.time())
}

#' **Parse ESPN MLB Player Box, helper function**
#' @param resp Response object (text) from the ESPN MLB game-summary endpoint.
#' @return Returns a `baseballr_data` tibble (one row per athlete-side), or
#'   `NULL`. A two-way player appears once for `batting` and once for
#'   `pitching` (`stat_group`).
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @importFrom lubridate with_tz ymd_hm
#' @importFrom stats setNames
#' @keywords internal
#' @export
helper_espn_mlb_player_box <- function(resp) {
  game_json <- jsonlite::fromJSON(resp, simplifyVector = FALSE)
  bx <- game_json[["boxscore"]]
  if (is.null(bx) || is.null(bx[["players"]]) || length(bx[["players"]]) < 1) {
    return(NULL)
  }
  header <- game_json[["header"]]
  comp <- header[["competitions"]][[1]]

  game_id <- as.integer(header[["id"]])
  season <- header[["season"]][["year"]]
  season_type <- header[["season"]][["type"]]
  raw_date <- comp[["date"]]
  game_date_time <- lubridate::with_tz(
    lubridate::ymd_hm(substr(raw_date, 1, nchar(raw_date) - 1)),
    tzone = "America/New_York"
  )
  game_date <- as.Date(substr(game_date_time, 1, 10))

  rows <- list()
  ridx <- 1L
  for (tb in bx[["players"]]) {
    tm <- tb[["team"]]
    for (grp in tb[["statistics"]]) {
      gtype <- grp[["type"]] %||% grp[["name"]] %||% NA_character_
      nms <- unlist(grp[["names"]])
      ath <- grp[["athletes"]]
      if (length(ath) == 0) next
      for (a in ath) {
        atl <- a[["athlete"]]
        pos <- a[["position"]] %||% atl[["position"]]
        stat_vals <- vapply(
          a[["stats"]],
          function(x) if (is.null(x)) NA_character_ else as.character(x)[1],
          character(1)
        )
        base <- data.frame(
          game_id                       = game_id,
          season                        = season,
          season_type                   = season_type,
          game_date                     = game_date,
          game_date_time                = game_date_time,
          stat_group                    = gtype,
          team_id                       = as.integer(tm[["id"]]),
          team_name                     = tm[["name"]] %||% NA_character_,
          team_abbreviation             = tm[["abbreviation"]] %||% NA_character_,
          team_display_name             = tm[["displayName"]] %||% NA_character_,
          athlete_id                    = suppressWarnings(as.integer(atl[["id"]])),
          athlete_display_name          = atl[["displayName"]] %||% NA_character_,
          athlete_short_name            = atl[["shortName"]] %||% NA_character_,
          athlete_position_name         = pos[["name"]] %||% NA_character_,
          athlete_position_abbreviation = pos[["abbreviation"]] %||% NA_character_,
          starter                       = a[["starter"]] %||% NA,
          bat_order                     = suppressWarnings(as.integer(a[["batOrder"]] %||% NA)),
          active                        = a[["active"]] %||% NA,
          stringsAsFactors = FALSE
        )
        if (length(nms) > 0 && length(stat_vals) == length(nms)) {
          stat_df <- stats::setNames(
            as.data.frame(as.list(stat_vals), stringsAsFactors = FALSE, check.names = FALSE),
            nms
          )
          rows[[ridx]] <- cbind(base, stat_df)
        } else {
          rows[[ridx]] <- base
        }
        ridx <- ridx + 1L
      }
    }
  }
  if (length(rows) == 0) return(NULL)

  dplyr::bind_rows(rows) |>
    janitor::clean_names() |>
    make_baseballr_data("ESPN MLB Player Box Information from ESPN.com", Sys.time())
}
