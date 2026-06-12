#' @rdname ncaa_game_logs
#' @title **Get NCAA Baseball Game Logs**
#' @param player_id A player's unique id. Can be found using the
#' get_ncaa_baseball_roster function.
#' @param year The year of interest.
#' @param type The kind of statistics you want to return. Current options
#' are 'batting', 'pitching', or 'fielding'.
#' @param span The span of time; can either be 'game' for game logs in a season, or 'career' which
#' returns seasonal stats for a player's career.
#' @param ... Additional arguments passed to an underlying function like httr.
#' @return A data frame containing player and school information as well as
#' game-by-game statistics. The exact stat columns vary by `type`
#' (batting / pitching / fielding) and `span` (game / career); the table
#' below shows the pitching (`type = "pitching"`, `span = "game"`) columns.
#'
#'  |col_name      |types     |description                                    |
#'  |:-------------|:---------|:----------------------------------------------|
#'  |player_id     |numeric   |stats.ncaa.org player identifier.              |
#'  |player_name   |character |Player name.                                   |
#'  |Date          |character |Game date.                                     |
#'  |Opponent      |character |Opponent name.                                 |
#'  |Result        |character |Game result (W/L and score).                   |
#'  |App           |numeric   |Appearances.                                   |
#'  |G             |numeric   |Games.                                         |
#'  |GS            |numeric   |Games started.                                 |
#'  |IP            |numeric   |Innings pitched.                               |
#'  |CG            |numeric   |Complete games.                                |
#'  |H             |numeric   |Hits allowed.                                  |
#'  |R             |numeric   |Runs allowed.                                  |
#'  |ER            |numeric   |Earned runs allowed.                           |
#'  |BB            |numeric   |Walks (bases on balls) allowed.                |
#'  |SO            |numeric   |Strikeouts.                                    |
#'  |SHO           |numeric   |Shutouts.                                      |
#'  |BF            |numeric   |Batters faced.                                 |
#'  |P-OAB         |numeric   |Opponent at-bats.                              |
#'  |2B-A          |numeric   |Doubles allowed.                               |
#'  |3B-A          |numeric   |Triples allowed.                               |
#'  |Bk            |numeric   |Balks.                                         |
#'  |HR-A          |numeric   |Home runs allowed.                             |
#'  |WP            |numeric   |Wild pitches.                                  |
#'  |HB            |numeric   |Hit batters.                                   |
#'  |IBB           |numeric   |Intentional walks allowed.                     |
#'  |Inh Run       |numeric   |Inherited runners.                             |
#'  |Inh Run Score |numeric   |Inherited runners who scored.                  |
#'  |SHA           |numeric   |Sacrifice hits allowed.                        |
#'  |SFA           |numeric   |Sacrifice flies allowed.                       |
#'  |Pitches       |numeric   |Pitch count.                                   |
#'  |GO            |numeric   |Ground outs induced.                           |
#'  |FO            |numeric   |Fly outs induced.                              |
#'  |W             |numeric   |Wins.                                          |
#'  |L             |numeric   |Losses.                                        |
#'  |SV            |numeric   |Saves.                                         |
#'  |OrdAppeared   |numeric   |Order in which the pitcher appeared.           |
#'  |KL            |numeric   |Strikeouts looking (called third strike).      |
#'  |pickoffs      |character |Pickoffs.                                      |
#'
#' @importFrom tibble tibble
#' @importFrom tidyr extract_numeric
#' @import rvest
#' @details
#' Live usage (reads `stats.ncaa.org`, which is behind Akamai bot protection and
#' needs the optional `chromote` + Google Chrome browser fallback, so it is shown
#' here rather than as a runnable example):
#'
#' ```r
#' ncaa_game_logs(player_id = 2649785, year = 2023, type = "pitching", span = "game")
#' ncaa_game_logs(player_id = 2477974, year = 2023, type = "pitching", span = "career")
#' ncaa_game_logs(player_id = 2680961, year = 2023, type = "batting", span = "game")
#' ncaa_game_logs(player_id = 2486588, year = 2023, type = "batting", span = "career")
#' ```
#' @export

ncaa_game_logs <- function(player_id, year, type = "batting", span = 'game', ...) {
  if (is.null(player_id)) {
    cli::cli_abort("Enter valid player_id")
  }
  if (is.null(year)) {
    cli::cli_abort("Enter valid year as a number (YYYY)")
  }
  if (is.null(type) | !(type %in% c("batting","pitching", "fielding"))) {
    cli::cli_abort("Enter valid type: 'batting', 'pitching', 'fielding'")
  }
  if (is.null(span) | !(span %in% c("game","career"))) {
    cli::cli_abort("Enter valid span: 'game', 'career'")
  }
  
  
  season_ids <- load_ncaa_baseball_season_ids() %>%
    dplyr::filter(.data$season == year)
  type_id <- season_ids %>%
    dplyr::pull(switch(type,
      pitching = "pitching_id",
      fielding = "fielding_id",
      "batting_id"
    ))

  payload_df <- data.frame()

  tryCatch(
    expr = {
      # stats.ncaa.org migrated player pages from
      # /player/index?id=...&stats_player_seq={id} to /players/{id}. The page
      # carries a season "career totals" grid and a per-game "game log" grid,
      # each id-keyed; pick the one `span` asks for.
      url <- paste0("https://stats.ncaa.org/players/", player_id,
                    "?year_stat_category_id=", type_id)
      resp <- request_with_proxy(url = url, ...)
      check_status(resp)
      payload <- resp %>%
        httr2::resp_body_string() %>%
        xml2::read_html()

      player_name <- payload %>%
        rvest::html_element("h2, .card-header, title") %>%
        rvest::html_text() %>%
        stringr::str_squish()

      grid_id <- if (span == "career") {
        paste0("#career_totals_", player_id, "_player")
      } else {
        paste0("#game_log_", player_id, "_player")
      }
      grid <- payload %>% rvest::html_element(grid_id)
      if (inherits(grid, "xml_missing") || length(grid) == 0) {
        cli::cli_abort("{Sys.time()}: No {span} table found for player {player_id}.")
      }

      payload_df <- grid %>%
        rvest::html_table() %>%
        as.data.frame(check.names = FALSE) %>%
        dplyr::rename(dplyr::any_of(c(DP = "OPP DP")))

      # The game-log grid spans every season the player has played; keep only
      # the requested year (its 4-digit value appears in the Date column).
      if (span == "game" && "Date" %in% names(payload_df)) {
        payload_df <- payload_df %>%
          dplyr::filter(stringr::str_detect(.data$Date, as.character(year)))
      }

      # Identifiers / text stay character; every other column (the stats) numeric.
      text_cols <- c("Date", "Opponent", "Result", "Year", "Team")
      stat_cols <- setdiff(names(payload_df), text_cols)
      suppressWarnings(
        payload_df <- payload_df %>%
          dplyr::mutate(dplyr::across(dplyr::any_of(stat_cols),
                                      ~ as.numeric(as.character(.x))))
      )

      payload_df <- payload_df %>%
        dplyr::mutate(
          player_id = player_id,
          player_name = player_name,
          year = year,
          type = type
        ) %>%
        dplyr::select("player_id", "player_name", "year", "type",
                      tidyr::everything()) %>%
        make_baseballr_data(
          "NCAA Baseball Game Logs data from stats.ncaa.org", Sys.time()
        )
    },
    error = function(e) {
      cli::cli_alert_danger(
        paste0("{Sys.time()}: Could not retrieve or parse NCAA game logs for ",
               "player {player_id} ({year}, {type}). The stats.ncaa.org page ",
               "layout may have changed or the request was challenged.")
      )
      cli::cli_alert_danger("Error: {conditionMessage(e)}")
    },
    finally = {
    }
  )
  return(payload_df)
  
}
#' @rdname get_ncaa_game_logs
#' @title **(legacy) Get NCAA Baseball Game Logs**
#' @inheritParams ncaa_game_logs
#' @inherit ncaa_game_logs return
#' @keywords legacy
#' @export
get_ncaa_game_logs <- ncaa_game_logs