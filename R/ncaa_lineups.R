#' @rdname ncaa_lineups
#' @title **Retrieve lineups for a given NCAA game via its `game_info_url`**
#' @param game_info_url The unique game info url
#' @param ... Additional arguments passed to an underlying function like httr.
#' @return Returns a tibble of each school's starting lineup and starting pitcher
#' 
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |year          |numeric   |
#'  |player_name   |character |
#'  |position      |character |
#'  |slug          |character |
#'  |batting_order |character |
#'  |team_name     |character |
#'  |sub           |numeric   |
#'  |attendance    |character |
#'  |game_date     |character |
#'  |location      |character |
#'  |player_id     |integer   |
#'  |team_id       |numeric   |
#'  |team_url      |character |
#'  |conference_id |numeric   |
#'  |conference    |character |
#'  |division      |numeric   |
#'  |season_id     |numeric   |
#' 
#' @importFrom stringr str_detect str_squish str_starts str_remove_all str_split_fixed
#' @import rvest
#' @export
#' @examples 
#' \donttest{
#'   try(ncaa_lineups(game_info_url="https://stats.ncaa.org/contests/2167178/box_score"))
#'   try(ncaa_lineups(game_info_url="https://stats.ncaa.org/game/index/4587474?org_id=528"))
#' }
ncaa_lineups <- function(game_info_url = NULL, ...) {
  
  if (is.null(game_info_url)) {
    cli::cli_abort("Enter valid game_info_url (e.g. https://stats.ncaa.org/contests/2167178/box_score")
  }
  # The redesigned box-score page no longer carries a standalone lineup table;
  # the batting lineup lives on the /contests/{id}/individual_stats tab (one
  # batting box score per team, with a "P" position column and the batting order
  # given by row order).
  url <- sub("box_score|play_by_play|team_stats|situational_stats",
             "individual_stats", game_info_url)

  lineup_table <- data.frame()

  tryCatch(
    expr = {
      resp <- request_with_proxy(url = url, ...)
      check_status(resp)
      payload <- resp %>%
        httr2::resp_body_string() %>%
        xml2::read_html()

      all_tables <- payload %>% rvest::html_elements("table")
      # The batting box scores carry "Name", "P" (position) and "AB" columns;
      # there is one per team (away then home). Pitching / fielding grids on the
      # same page lack the "AB" column, so this selects only the batting ones.
      is_batting <- vapply(all_tables, function(t) {
        nm <- tryCatch(names(rvest::html_table(t)), error = function(e) character(0))
        all(c("Name", "P", "AB") %in% nm)
      }, logical(1))
      batting_tables <- all_tables[is_batting]
      if (length(batting_tables) == 0) {
        cli::cli_abort("{Sys.time()}: No batting lineup tables found at {url}")
      }

      parse_team_lineup <- function(tnode) {
        tbl <- tnode %>% rvest::html_table()
        is_total <- tbl[["#"]] == "" | is.na(tbl[["#"]])
        team_name <- if (any(is_total)) tbl[["Name"]][which(is_total)[1]] else NA_character_
        rows <- tnode %>% rvest::html_elements("tbody tr")
        slug <- vapply(rows, function(r) {
          a <- rvest::html_element(r, "a")
          if (inherits(a, "xml_missing")) NA_character_ else rvest::html_attr(a, "href")
        }, character(1))
        if (length(slug) != nrow(tbl)) slug <- rep(NA_character_, nrow(tbl))
        keep <- !is_total
        data.frame(
          player_name = stringr::str_squish(tbl[["Name"]][keep]),
          position = tbl[["P"]][keep],
          batting_order = seq_len(sum(keep)),
          team_name = stringr::str_squish(team_name),
          slug = slug[keep],
          stringsAsFactors = FALSE
        )
      }

      lineup_table <- purrr::map(batting_tables, parse_team_lineup) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(
          player_id = as.integer(stringr::str_extract(.data$slug, "players/(\\d+)", group = 1)),
          player_url = ifelse(is.na(.data$slug), NA_character_,
                              paste0("https://stats.ncaa.org", .data$slug))
        ) %>%
        dplyr::select(
          "player_name", "position", "batting_order", "team_name",
          "player_id", "player_url", "slug"
        ) %>%
        make_baseballr_data("NCAA Baseball Lineups data from stats.ncaa.org", Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger(
        paste0("{Sys.time()}: Could not retrieve or parse NCAA lineups from ",
               "{game_info_url}. The stats.ncaa.org page layout may have changed ",
               "or the request was challenged.")
      )
      cli::cli_alert_danger("Error: {conditionMessage(e)}")
    },
    finally = {
    }
  )
  return(lineup_table)
}

#' @rdname get_ncaa_lineups
#' @title **(legacy) Retrieve lineups for a given NCAA game via its `game_info_url`**
#' @inheritParams ncaa_lineups
#' @return Returns a tibble of each school's starting lineup and starting pitcher
#' @keywords legacy
#' @export
get_ncaa_lineups <- ncaa_lineups
