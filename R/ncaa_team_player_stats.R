#' @rdname ncaa_team_player_stats
#' @title **Scrape NCAA baseball Team Player Stats (Division I, II, and III)**
#' @description This function allows the user to obtain batting, pitching, or fielding statistics for any school affiliated with the NCAA at the division I, II, or III levels. The function acquires data from the NCAA's website (stats.ncaa.org) and returns a tibble.
#' @param team_id The numerical ID that the NCAA website uses to identify a team
#' @param year The season for which data should be returned, in the form of "YYYY". Years currently available: 2013-2017.
#' @param type A string indicating whether to return "batting", "pitching", or "fielding" statistics
#' @param ... Additional arguments passed to an underlying function like httr.
#' @return A data frame with the following variables
#'  
#'    |col_name      |types     |
#'    |:-------------|:---------|
#'    |year          |integer   |
#'    |team_name     |character |
#'    |team_id       |numeric   |
#'    |conference_id |integer   |
#'    |conference    |character |
#'    |division      |numeric   |
#'    |player_id     |integer   |
#'    |player_url    |character |
#'    |player_name   |character |
#'    |Yr            |character |
#'    |Pos           |character |
#'    |Jersey        |character |
#'    |GP            |numeric   |
#'    |GS            |numeric   |
#'    |BA            |numeric   |
#'    |OBPct         |numeric   |
#'    |SlgPct        |numeric   |
#'    |R             |numeric   |
#'    |AB            |numeric   |
#'    |H             |numeric   |
#'    |2B            |numeric   |
#'    |3B            |numeric   |
#'    |TB            |numeric   |
#'    |HR            |numeric   |
#'    |RBI           |numeric   |
#'    |BB            |numeric   |
#'    |HBP           |numeric   |
#'    |SF            |numeric   |
#'    |SH            |numeric   |
#'    |K             |numeric   |
#'    |DP            |numeric   |
#'    |CS            |numeric   |
#'    |Picked        |numeric   |
#'    |SB            |numeric   |
#'    |RBI2out       |numeric   |
#'  
#' @import dplyr
#' @import rvest
#' @importFrom stringr str_split
#' @export
#' @examples
#' \donttest{
#'   try(ncaa_team_player_stats(team_id = 234, year = 2023, type = "batting"))
#' }

ncaa_team_player_stats <- function(team_id, year = most_recent_ncaa_baseball_season(), type = 'batting', ...) {
  
  if (is.null(team_id)) {
    cli::cli_abort("Enter valid team_id")
  }
  if (is.null(year)) {
    cli::cli_abort("Enter valid year as a number (YYYY)")
  }
  if (is.null(type) | !(type %in% c("batting","pitching", "fielding"))) {
    cli::cli_abort("Enter valid type: 'batting', 'pitching', 'fielding'")
  }
  
  if (year < 2013) {
    stop('you must provide a year that is greater than or equal to 2013')
  }

  df <- data.frame()

  tryCatch(
    expr = {
      # Team stats live at /team/{team_id}/stats, keyed by the season id and the
      # per-type stat-category id (batting_id / pitching_id) from the season-id
      # lookup. The previous batting branch passed `id` twice and never sent the
      # category id; both branches now use year_stat_category_id correctly.
      # NOTE: this endpoint is currently gated behind Akamai's bm-verify
      # interstitial, which a static request cannot solve (it works in a real
      # browser that runs the challenge JS). When challenged the function
      # degrades gracefully via .ncaa_is_interstitial() below.
      season_ids <- load_ncaa_baseball_season_ids() %>%
        dplyr::filter(.data$season == year)
      season_id <- season_ids %>% dplyr::pull("id")
      type_id <- season_ids %>%
        dplyr::pull(switch(type,
          pitching = "pitching_id",
          fielding = "fielding_id",
          "batting_id"
        ))

      # stats.ncaa.org migrated team stats from the franchise-centric
      # /team/{team_id}/stats form to a per-season resource at
      # /teams/{season_team_id}/season_to_date_stats. Resolve the season-team id
      # (read off the still-working roster page) and request the category grid.
      season_team_id <- .ncaa_resolve_season_team_id(team_id, season_id, ...)
      if (is.na(season_team_id)) {
        cli::cli_alert_warning(
          paste0("{Sys.time()}: Could not resolve the stats.ncaa.org season-team ",
                 "id for team {team_id} ({year}); the roster page was unavailable ",
                 "or challenged. Install {{chromote}} + Google Chrome to enable ",
                 "the browser fallback.")
        )
        return(df)
      }
      url <- paste0("https://stats.ncaa.org/teams/", season_team_id,
                    "/season_to_date_stats?year_stat_category_id=", type_id)

      team_stats_resp <- request_with_proxy(url = url, ...)
      payload_txt <- httr2::resp_body_string(team_stats_resp)

      if (httr2::resp_status(team_stats_resp) != 200 ||
          .ncaa_is_interstitial(payload_txt)) {
        cli::cli_alert_warning(
          paste0("{Sys.time()}: stats.ncaa.org returned an Akamai bot-challenge ",
                 "for the team {type} stats endpoint; no data could be retrieved. ",
                 "Install {{chromote}} + Google Chrome to enable the browser fallback.")
        )
        return(df)
      }

      data_read <- xml2::read_html(payload_txt)
      stat_grid <- data_read %>% rvest::html_element("#stat_grid")
      if (inherits(stat_grid, "xml_missing") || length(stat_grid) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No #stat_grid table found for team {team_id} ({year})."
        )
        return(df)
      }

      # One generic parse for batting / pitching / fielding. The redesigned grid
      # labels the jersey column "#" (callers expect "Jersey") and batting uses
      # "OPP DP" (callers expect "DP"). Identifier/text columns stay character;
      # every other column (the per-category stats) is coerced to numeric --
      # tolerant of the differing column sets across the three categories.
      df <- stat_grid %>%
        rvest::html_table() %>%
        as.data.frame(check.names = FALSE) %>%
        dplyr::rename(dplyr::any_of(c(Jersey = "#", DP = "OPP DP")))
      df$Player <- gsub("x ", "", df$Player)
      df$year <- year
      df$team_id <- team_id
      df <- df %>%
        dplyr::left_join(load_ncaa_baseball_teams(),
                         by = c("team_id" = "team_id", "year" = "year"))
      text_cols <- c("team_name", "conference", "Jersey", "Player", "Yr", "Pos",
                     "Ht", "B/T")
      stat_cols <- setdiff(names(df), text_cols)
      suppressWarnings(
        df <- df %>%
          dplyr::mutate(dplyr::across(dplyr::any_of(stat_cols),
                                      ~ as.numeric(as.character(.x))))
      )

      player_url <- data_read %>%
        html_elements('#stat_grid a') %>%
        html_attr('href') %>%
        as.data.frame() %>%
        dplyr::rename("player_url" = ".") %>%
        dplyr::mutate(player_url = paste0('https://stats.ncaa.org', .data$player_url))
      
      player_names_join <- data_read %>%
        html_elements('#stat_grid a') %>%
        html_text() %>%
        as.data.frame() %>%
        dplyr::rename("player_names_join" = ".")
      
      # The redesigned grid links to /players/{id}?... (was ...&stats_player_seq={id}).
      player_id <- player_url$player_url %>%
        stringr::str_extract("players/(\\d+)", group = 1) %>%
        as.data.frame() %>%
        dplyr::rename("player_id" = ".")
      
      player_url_comb <- dplyr::bind_cols(player_names_join, player_id, player_url)
      
      df <- df %>% 
        dplyr::left_join(player_url_comb, by = c('Player' = 'player_names_join'))
      df <- df %>% 
        dplyr::rename("player_name" = "Player")
      
      df <- df %>%
        dplyr::mutate_at(vars(player_url), as.character) %>%
        dplyr::mutate_at(c("conference_id", "player_id", "year"), as.integer) %>%
        dplyr::select(
          "year",
          "team_name",
          "team_id",
          "conference_id",
          "conference",
          "division",
          "player_id",
          "player_url",
          "player_name",
          "Yr",
          "Pos",
          "Jersey",
          tidyr::everything()) %>% 
      make_baseballr_data(glue::glue("NCAA Baseball Team {stringr::str_to_title(type)} Stats data from stats.ncaa.org"),Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger(
        paste0("{Sys.time()}: Could not retrieve or parse NCAA team {type} stats ",
               "for team {team_id} ({year}). The stats.ncaa.org page layout may ",
               "have changed or the request was challenged.")
      )
      cli::cli_alert_danger("Error: {conditionMessage(e)}")
    },
    finally = {
    }
  )
  return(df)
}


#' @rdname ncaa_scrape
#' @title **(legacy) Scrape NCAA baseball Team Player Stats (Division I, II, and III)**
#' @inheritParams ncaa_team_player_stats
#' @return A data frame with the following variables
#'  
#'    |col_name      |types     |
#'    |:-------------|:---------|
#'    |year          |integer   |
#'    |team_name     |character |
#'    |team_id       |numeric   |
#'    |conference_id |integer   |
#'    |conference    |character |
#'    |division      |numeric   |
#'    |player_id     |integer   |
#'    |player_url    |character |
#'    |player_name   |character |
#'    |Yr            |character |
#'    |Pos           |character |
#'    |Jersey        |character |
#'    |GP            |numeric   |
#'    |GS            |numeric   |
#'    |BA            |numeric   |
#'    |OBPct         |numeric   |
#'    |SlgPct        |numeric   |
#'    |R             |numeric   |
#'    |AB            |numeric   |
#'    |H             |numeric   |
#'    |2B            |numeric   |
#'    |3B            |numeric   |
#'    |TB            |numeric   |
#'    |HR            |numeric   |
#'    |RBI           |numeric   |
#'    |BB            |numeric   |
#'    |HBP           |numeric   |
#'    |SF            |numeric   |
#'    |SH            |numeric   |
#'    |K             |numeric   |
#'    |DP            |numeric   |
#'    |CS            |numeric   |
#'    |Picked        |numeric   |
#'    |SB            |numeric   |
#'    |RBI2out       |numeric   |
#'    
#' @keywords legacy
#' @export
ncaa_scrape <- ncaa_team_player_stats