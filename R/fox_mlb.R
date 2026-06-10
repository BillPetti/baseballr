# fox_mlb.R -- Fox Sports "Bifrost" MLB wrappers.
#
# Read-only wrappers over api.foxsports.com/bifrost/v1/mlb/*, flattening Fox's
# layout JSON (sections -> tables -> rows -> cells) into tidy baseballr tibbles.
# NOTE: Fox does not expose MLB play-by-play or boxscore via event/{id}/data
# (it returns only header / top-performers), so there is no fox_mlb_pbp /
# fox_mlb_boxscore -- only the team/league endpoints + game odds.
# Reverse-engineering notes + an OpenAPI 3.1 spec live in the sdv-internal-refs repo.

.FOX_MLB_KEY <- "jE7yBJVRNAwdDesMgTzTXUUSx1It41Fq"
.fox_or <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' @keywords internal
#' @importFrom httr2 request req_url_query req_headers req_retry req_perform resp_body_string
#' @importFrom jsonlite fromJSON
.fox_mlb_get <- function(path, query = list()) {
  query[["apikey"]] <- .fox_or(query[["apikey"]], getOption("baseballr.fox_key", .FOX_MLB_KEY))
  query[["api-version"]] <- .fox_or(query[["api-version"]], "1.1")
  res <- httr2::request(paste0("https://api.foxsports.com/bifrost/v1/", path)) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_headers(Origin = "https://www.foxsports.com",
                       Referer = "https://www.foxsports.com/") |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2) |>
    httr2::req_perform()
  res |>
    httr2::resp_body_string(encoding = "UTF-8") |>
    jsonlite::fromJSON(simplifyDataFrame = FALSE, simplifyVector = FALSE, simplifyMatrix = FALSE)
}

.fox_cells <- function(cols) {
  vapply(cols, function(c) {
    v <- if (is.list(c)) c[["text"]] else c
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1]
  }, character(1))
}
.fox_uri_id <- function(uri) {
  if (is.null(uri)) return(NA_character_)
  m <- regmatches(uri, regexpr("[0-9]+$", uri)); if (length(m)) m else NA_character_
}
#' @importFrom janitor make_clean_names
.fox_table_df <- function(tbl, extra = list()) {
  if (is.null(tbl)) return(NULL)
  hdr <- .fox_cells(tbl[["headers"]][[1]][["columns"]])
  nm <- janitor::make_clean_names(ifelse(is.na(hdr) | hdr == "", paste0("v", seq_along(hdr)), hdr))
  rws <- .fox_or(tbl[["rows"]], list()); if (!length(rws)) return(NULL)
  recs <- lapply(rws, function(r) {
    cells <- .fox_cells(r[["columns"]]); vals <- as.list(cells); names(vals) <- nm[seq_along(vals)]
    eid <- .fox_uri_id(.fox_or(r[["entityLink"]][["contentUri"]], NULL))
    as.data.frame(c(extra, vals, list(entity_id = eid)), stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(recs)
}

# ---- parsers (return data.frame) ------------------------------------------
.fox_mlb_roster <- function(raw, team_id) {
  rows <- list()
  for (g in .fox_or(raw[["groups"]], list())) {
    hdr <- .fox_cells(g[["headers"]][[1]][["columns"]])
    group_label <- .fox_or(g[["title"]], .fox_or(hdr[1], NA_character_))
    col_names <- c("player", tolower(.fox_or(hdr[-1], character(0))))
    for (r in .fox_or(g[["rows"]], list())) {
      uri <- .fox_or(r[["entityLink"]][["contentUri"]], NULL)
      if (is.null(uri) || !grepl("athletes/", uri)) next
      cells <- .fox_cells(r[["columns"]]); vals <- as.list(cells); names(vals) <- col_names[seq_along(vals)]
      rows[[length(rows) + 1]] <- data.frame(
        team_id = as.character(team_id), position_group = group_label,
        as.data.frame(vals, stringsAsFactors = FALSE), athlete_id = .fox_uri_id(uri), stringsAsFactors = FALSE)
    }
  }
  if (length(rows)) dplyr::bind_rows(rows) else data.frame()
}
.fox_mlb_team_stats <- function(raw, team_id) {
  rows <- list()
  for (sec in .fox_or(raw[["leadersSections"]], list())) {
    for (ld in .fox_or(sec[["leaders"]], list())) {
      rows[[length(rows) + 1]] <- data.frame(
        team_id = as.character(team_id), category = .fox_or(sec[["title"]], NA_character_),
        stat = .fox_or(ld[["title"]], NA_character_),
        stat_abbreviation = .fox_or(ld[["statAbbreviation"]], NA_character_),
        player = .fox_or(ld[["name"]], NA_character_), value = .fox_or(ld[["statValue"]], NA_character_),
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}
.fox_mlb_gamelog <- function(raw, team_id) {
  rows <- list()
  for (sec in .fox_or(raw[["sectionList"]], list())) {
    category <- .fox_or(sec[["id"]], NA_character_)
    for (tbl in .fox_or(sec[["tables"]], list())) {
      hdr <- .fox_cells(tbl[["headers"]][[1]][["columns"]])
      season_type <- .fox_or(hdr[1], NA_character_)
      stat_names <- janitor::make_clean_names(.fox_or(hdr[-(1:2)], character(0)))
      for (r in .fox_or(tbl[["rows"]], list())) {
        cells <- .fox_cells(r[["columns"]]); gid <- .fox_uri_id(.fox_or(r[["entityLink"]][["contentUri"]], NULL))
        vals <- cells[-(1:2)]
        for (j in seq_along(vals)) {
          rows[[length(rows) + 1]] <- data.frame(
            team_id = as.character(team_id), season_type = season_type, category = category, game_id = gid,
            game_date = .fox_or(cells[1], NA_character_), opponent = .fox_or(cells[2], NA_character_),
            stat = .fox_or(stat_names[j], paste0("v", j)), value = vals[j], stringsAsFactors = FALSE)
        }
      }
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}
.fox_mlb_standings <- function(raw, team_id) {
  parts <- list()
  for (s in .fox_or(raw[["standingsSections"]], list())) {
    for (tbl in .fox_or(s[["standings"]], list())) {
      parts[[length(parts) + 1]] <- .fox_table_df(
        tbl, extra = list(team_id = as.character(team_id), section = .fox_or(s[["title"]], NA_character_)))
    }
  }
  dplyr::bind_rows(parts)
}
.fox_mlb_leaders <- function(raw) {
  parts <- lapply(.fox_or(raw[["sectionList"]], list()), function(s) .fox_table_df(s[["table"]]))
  dplyr::bind_rows(parts)
}
.fox_mlb_odds <- function(raw, game_id) {
  sp <- raw[["sixPack"]]; o <- if (is.null(sp)) NULL else sp[["odds"]]; rows <- list()
  if (!is.null(o)) {
    hdr <- janitor::make_clean_names(.fox_cells(o[["columnHeaders"]]))
    for (r in .fox_or(o[["rows"]], list())) {
      vals <- vapply(.fox_or(r[["values"]], list()),
                     function(v) as.character(.fox_or(v[["odds"]], NA_character_)), character(1))
      rec <- as.list(vals); names(rec) <- hdr[seq_along(rec)]
      rows[[length(rows) + 1]] <- data.frame(
        game_id = as.character(game_id), team = .fox_or(r[["fullText"]], .fox_or(r[["text"]], NA_character_)),
        as.data.frame(rec, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
    }
  }
  if (length(rows)) dplyr::bind_rows(rows) else data.frame()
}

#' @keywords internal
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble bind_rows
.fox_mlb_resource <- function(resource, game_id = NULL, team_id = NULL,
                              category = "batting", who = "player", page = 0) {
  out <- data.frame()
  tryCatch(
    expr = {
      raw <- switch(
        resource,
        odds = .fox_mlb_get(paste0("mlb/event/", game_id, "/odds")),
        league_leaders = .fox_mlb_get(paste0("mlb/league/stats-con/", who, "/", category, "/", page)),
        roster = .fox_mlb_get(paste0("mlb/team/", team_id, "/roster")),
        team_stats = .fox_mlb_get(paste0("mlb/team/", team_id, "/stats")),
        gamelog = .fox_mlb_get(paste0("mlb/team/", team_id, "/gamelog")),
        standings = .fox_mlb_get(paste0("mlb/team/", team_id, "/standings")))
      df <- switch(
        resource,
        odds = .fox_mlb_odds(raw, game_id), roster = .fox_mlb_roster(raw, team_id),
        team_stats = .fox_mlb_team_stats(raw, team_id), gamelog = .fox_mlb_gamelog(raw, team_id),
        standings = .fox_mlb_standings(raw, team_id), league_leaders = .fox_mlb_leaders(raw))
      out <- df |>
        dplyr::as_tibble() |>
        janitor::clean_names() |>
        make_baseballr_data(paste0("Fox Sports MLB ", resource), Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: invalid arguments or no Fox MLB {resource} data available!"))
    }
  )
  out
}

# ---- public wrappers (MLB; generic only) -----------------------------------
#' **Get Fox Sports MLB team roster**
#' @name fox_mlb_team_roster
#' @param team_id Fox Bifrost team id (e.g. `"1"`). Discover via the league team directory.
#' @return A `baseballr_data` tibble, one row per player: `team_id`,
#'   `position_group`, `player`, position/age/etc. columns, `athlete_id`.
#' @export
#' @examples \donttest{ try(fox_mlb_team_roster("1")) }
fox_mlb_team_roster <- function(team_id) .fox_mlb_resource("roster", team_id = team_id)

#' **Get Fox Sports MLB team stat leaders**
#' @name fox_mlb_team_stats
#' @param team_id Fox Bifrost team id.
#' @return A `baseballr_data` tibble: `team_id`, `category`, `stat`,
#'   `stat_abbreviation`, `player`, `value`.
#' @export
#' @examples \donttest{ try(fox_mlb_team_stats("1")) }
fox_mlb_team_stats <- function(team_id) .fox_mlb_resource("team_stats", team_id = team_id)

#' **Get Fox Sports MLB team game log**
#' @name fox_mlb_team_gamelog
#' @param team_id Fox Bifrost team id.
#' @return A `baseballr_data` tibble (long): `team_id`, `season_type`,
#'   `category`, `game_id`, `game_date`, `opponent`, `stat`, `value`.
#' @export
#' @examples \donttest{ try(fox_mlb_team_gamelog("1")) }
fox_mlb_team_gamelog <- function(team_id) .fox_mlb_resource("gamelog", team_id = team_id)

#' **Get Fox Sports MLB standings**
#' @name fox_mlb_standings
#' @param team_id Fox Bifrost team id (standings of that team's division/league).
#' @return A `baseballr_data` tibble of standings rows (`team_id`, `section`,
#'   the standings columns, `entity_id`).
#' @export
#' @examples \donttest{ try(fox_mlb_standings("1")) }
fox_mlb_standings <- function(team_id) .fox_mlb_resource("standings", team_id = team_id)

#' **Get Fox Sports MLB statistical leaders**
#' @name fox_mlb_league_leaders
#' @param category Stat category (default `"batting"`).
#' @param who `"player"` or `"team"` (default `"player"`).
#' @param page 0-based page index (default `0`).
#' @return A `baseballr_data` tibble of leaderboard rows (`entity_id` + stat columns).
#' @export
#' @examples \donttest{ try(fox_mlb_league_leaders("batting")) }
fox_mlb_league_leaders <- function(category = "batting", who = "player", page = 0) {
  .fox_mlb_resource("league_leaders", category = category, who = who, page = page)
}

#' **Get Fox Sports MLB game odds**
#' @name fox_mlb_odds
#' @param game_id Fox Bifrost event id.
#' @return A `baseballr_data` tibble, one row per team (`game_id`, `team`, plus
#'   six-pack odds columns: run line / to-win / total). Empty when no market.
#' @export
#' @examples \donttest{ try(fox_mlb_odds("95687")) }
fox_mlb_odds <- function(game_id) .fox_mlb_resource("odds", game_id = game_id)
