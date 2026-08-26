#' @title
#' **Scrape FanGraphs player projections**
#'
#' @description Retrieves FanGraphs' published player projections (Steamer,
#'   ZiPS, ATC, THE BAT, and their variants) from the FanGraphs projections
#'   API -- one row per player with the projected stat line.
#' @param type Projection system. One of `"steamer"`, `"zips"`, `"zipsdc"`,
#'   `"atc"`, `"thebat"`, `"thebatx"` (batters only), or the rest-of-season
#'   variants `"rsteamer"`, `"rzips"`, `"ratc"`, `"rthebat"`, `"rthebatx"`.
#'   Defaults to `"steamer"`.
#' @param stats Either `"bat"` (batting projections) or `"pit"` (pitching
#'   projections). Defaults to `"bat"`.
#' @param team MLB team filter as FanGraphs' numeric team id; `0` (default)
#'   returns all teams.
#' @param league League filter: `"all"` (default), `"al"`, or `"nl"`.
#' @param position Position filter (e.g. `"all"`, `"c"`, `"ss"`, `"of"`).
#'   Defaults to `"all"`.
#' @return A `baseballr_data` tibble with one row per player and the
#'   projection system's stat columns (counting stats, rates, and projected
#'   WAR; column set varies by system and by `stats`).
#' @export
#' @examples
#' \donttest{
#'   try(fg_projections(type = "steamer", stats = "bat"))
#' }
fg_projections <- function(type = "steamer", stats = "bat", team = 0,
                           league = "all", position = "all") {

  valid_types <- c("steamer", "zips", "zipsdc", "atc", "thebat", "thebatx",
                   "rsteamer", "rzips", "ratc", "rthebat", "rthebatx")
  if (!type %in% valid_types) {
    cli::cli_abort("{.arg type} must be one of {.val {valid_types}}.")
  }
  if (!stats %in% c("bat", "pit")) {
    cli::cli_abort('{.arg stats} must be "bat" or "pit".')
  }

  url <- paste0(
    "https://www.fangraphs.com/api/projections",
    "?type=", type,
    "&stats=", stats,
    "&pos=", position,
    "&team=", team,
    "&players=0",
    "&lg=", league
  )

  df <- NULL
  tryCatch(
    expr = {
      payload <- fg_api_call(url)
      df <- payload |>
        dplyr::as_tibble() |>
        janitor::clean_names() |>
        make_baseballr_data(
          paste0("FanGraphs ", type, " ", stats, " projections data from FanGraphs.com"),
          Sys.time()
        )
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no projections data available!")
      cli::cli_alert_info("Original error: {conditionMessage(e)}")
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}
