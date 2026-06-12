# College-baseball twins of the espn_mlb_data.R game functions.
# ESPN serves the identical summary / core JSON for college baseball, so the
# parser bodies are copied verbatim from R/espn_mlb_data.R; only the league
# slug in the URL (baseball/mlb -> baseball/college-baseball,
# leagues/mlb -> leagues/college-baseball) and the human-readable
# "ESPN MLB" labels differ.

#' **Get ESPN College Baseball game data (Pbp, Team and Player Box)**
#' @title **Get ESPN College Baseball Game All**
#' @rdname espn_college_baseball_game_all
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_all
#' @inherit espn_mlb_game_all return
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @import rvest
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#' try(espn_college_baseball_game_all(game_id = "401778093"))
#' }
#'
espn_college_baseball_game_all <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  summary_url <- "http://site.api.espn.com/apis/site/v2/sports/baseball/college-baseball/summary?"

  ## Inputs
  ## game_id
  full_url <- paste0(
    summary_url,
    "event=",
    game_id
  )

  pbp <- list(Plays = NULL, Team = NULL, Player = NULL)
  resp <- NULL
  plays_df <- NULL
  team_box_score <- NULL
  player_box_score <- NULL

  #---- Fetch the summary endpoint (single outer tryCatch) -------------------
  tryCatch(
    expr = {
      res <- .retry_request(full_url)
      check_status(res)
      resp <- res %>%
        .resp_text()
    },
    error = function(e) .report_api_error(
      e,
      hint = "Could not fetch game summary for game_id = {game_id}",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )

  if (is.null(resp)) {
    return(pbp)
  }

  #---- Play-by-Play ------
  tryCatch(
    expr = {
      plays_df <- helper_espn_mlb_pbp(resp)

      if (is.null(plays_df)) {
        message(sprintf("%s: No play-by-play data for %s available!", Sys.time(), game_id))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no play-by-play data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  #---- Team Box ------
  tryCatch(
    expr = {
      team_box_score <- helper_espn_mlb_team_box(resp)

      if (is.null(team_box_score)) {
        message(sprintf("%s: No team box score data for %s available!", Sys.time(), game_id))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no team box score data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  #---- Player Box ------
  tryCatch(
    expr = {
      player_box_score <- helper_espn_mlb_player_box(resp)

      if (is.null(player_box_score)) {
        message(sprintf("%s: No player box score data for %s available!", Sys.time(), game_id))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no player box score data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )

  pbp <- c(list(plays_df), list(team_box_score), list(player_box_score))
  names(pbp) <- c("Plays", "Team", "Player")
  return(pbp)
}


#' **Get ESPN College Baseball PBP data**
#' @title **Get ESPN College Baseball Play-by-Play**
#' @rdname espn_college_baseball_pbp
#' @author Saiem Gilani
#' @inheritParams espn_mlb_pbp
#' @inherit espn_mlb_pbp return
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @import rvest
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#' try(espn_college_baseball_pbp(game_id = "401778093"))
#' }
#'
espn_college_baseball_pbp <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  summary_url <-
    "http://site.api.espn.com/apis/site/v2/sports/baseball/college-baseball/summary?"

  ## Inputs
  ## game_id
  full_url <- paste0(
    summary_url,
    "event=",
    game_id
  )

  plays_df <- NULL

  #---- Play-by-Play ------
  tryCatch(
    expr = {
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      plays_df <- helper_espn_mlb_pbp(resp)

      if (is.null(plays_df)) {
        return(message(sprintf("%s: No play-by-play data for %s available!", Sys.time(), game_id)))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no play-by-play data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )

  return(plays_df)
}


#' **Get ESPN College Baseball team box scores**
#' @title **Get ESPN College Baseball Team Box**
#' @rdname espn_college_baseball_team_box
#' @author Saiem Gilani
#' @inheritParams espn_mlb_team_box
#' @inherit espn_mlb_team_box return
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @import rvest
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#' try(espn_college_baseball_team_box(game_id = "401778093"))
#' }
#'
espn_college_baseball_team_box <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  summary_url <-
    "http://site.api.espn.com/apis/site/v2/sports/baseball/college-baseball/summary?"

  ## Inputs
  ## game_id
  full_url <- paste0(
    summary_url,
    "event=",
    game_id
  )

  #---- Team Box ------
  team_box_score <- NULL

  tryCatch(
    expr = {
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      team_box_score <- helper_espn_mlb_team_box(resp)

      if (is.null(team_box_score)) {
        return(message(sprintf("%s: No team box score data for %s available!", Sys.time(), game_id)))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no team box score data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(team_box_score)
}


#' **Get ESPN College Baseball player box scores**
#' @title **Get ESPN College Baseball Player Box**
#' @rdname espn_college_baseball_player_box
#' @author Saiem Gilani
#' @inheritParams espn_mlb_player_box
#' @inherit espn_mlb_player_box return
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @import rvest
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#' try(espn_college_baseball_player_box(game_id = "401778093"))
#' }
#'
espn_college_baseball_player_box <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  summary_url <-
    "http://site.api.espn.com/apis/site/v2/sports/baseball/college-baseball/summary?"

  ## Inputs
  ## game_id
  full_url <- paste0(
    summary_url,
    "event=",
    game_id
  )

  #---- Player Box ------
  player_box_score <- NULL

  tryCatch(
    expr = {
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      player_box_score <- helper_espn_mlb_player_box(resp)

      if (is.null(player_box_score)) {
        return(message(sprintf("%s: No player box score data for %s available!", Sys.time(), game_id)))
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no player box score data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(player_box_score)
}


#' **Get ESPN College Baseball game rosters**
#' @title **Get ESPN College Baseball Game Rosters**
#' @rdname espn_college_baseball_game_rosters
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_rosters
#' @inherit espn_mlb_game_rosters return
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @import rvest
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#' try(espn_college_baseball_game_rosters(game_id = "401778093"))
#' }
espn_college_baseball_game_rosters <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  athlete_roster_df <- .empty_baseballr_data("ESPN College Baseball Game Roster Information from ESPN.com")

  tryCatch(
    expr = {
      play_base_url <- paste0(
        "https://sports.core.api.espn.com/v2/sports/baseball/leagues/college-baseball/events/",
        game_id,
        "/competitions/",
        game_id,
        "/competitors/"
      )
      game_res <- .retry_request(play_base_url)
      # Check the result
      check_status(game_res)

      game_resp <- game_res %>%
        .resp_text()
      game_df <- jsonlite::fromJSON(game_resp)[["items"]] %>%
        jsonlite::toJSON() %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        dplyr::rename("team_statistics_href" = "statistics.$ref")

      colnames(game_df) <- gsub(".\\$ref", "_href", colnames(game_df))

      game_df <- game_df %>%
        dplyr::rename(
          "team_id" = "id",
          "team_uid" = "uid"
        )

      game_df$game_id <- game_id

      teams_df <- purrr::map_dfr(game_df$team_href, function(x) {
        res <- .retry_request(x)
        # Check the result
        check_status(res)

        team_df <- res %>%
          .resp_text() %>%
          jsonlite::fromJSON(
            simplifyDataFrame = FALSE,
            simplifyVector = FALSE,
            simplifyMatrix = FALSE
          )

        team_df[["links"]] <- NULL
        team_df[["injuries"]] <- NULL
        team_df[["record"]] <- NULL
        team_df[["athletes"]] <- NULL
        team_df[["venue"]] <- NULL
        team_df[["groups"]] <- NULL
        team_df[["ranks"]] <- NULL
        team_df[["statistics"]] <- NULL
        team_df[["leaders"]] <- NULL
        team_df[["links"]] <- NULL
        team_df[["notes"]] <- NULL
        team_df[["franchise"]] <- NULL
        team_df[["againstTheSpreadRecords"]] <- NULL
        team_df[["oddsRecords"]] <- NULL
        team_df[["college"]] <- NULL
        team_df[["transactions"]] <- NULL
        team_df[["leaders"]] <- NULL
        team_df[["depthCharts"]] <- NULL
        team_df[["awards"]] <- NULL
        team_df[["events"]] <- NULL

        team_df <- team_df %>%
          purrr::map_if(is.list, as.data.frame) %>%
          as.data.frame() %>%
          dplyr::select(
            -dplyr::any_of(
              c(
                "logos.width",
                "logos.height",
                "logos.alt",
                "logos.rel..full.",
                "logos.rel..default.",
                "logos.rel..scoreboard.",
                "logos.rel..scoreboard..1",
                "logos.rel..scoreboard.2",
                "logos.lastUpdated",
                "logos.width.1",
                "logos.height.1",
                "logos.alt.1",
                "logos.rel..full..1",
                "logos.rel..dark.",
                "logos.rel..dark..1",
                "logos.lastUpdated.1",
                "logos.width.2",
                "logos.height.2",
                "logos.alt.2",
                "logos.rel..full..2",
                "logos.rel..scoreboard.",
                "logos.lastUpdated.2",
                "logos.width.3",
                "logos.height.3",
                "logos.alt.3",
                "logos.rel..full..3",
                "logos.lastUpdated.3",
                "X.ref",
                "X.ref.1",
                "X.ref.2"
              )
            )
          ) %>%
          janitor::clean_names()

        colnames(team_df)[1:13] <- paste0("team_", colnames(team_df)[1:13])

        team_df <- team_df %>%
          dplyr::rename(
            "logo_href" = "logos_href",
            "logo_dark_href" = "logos_href_1"
          ) %>%
          dplyr::left_join(
            game_df %>%
              dplyr::select(
                "game_id",
                "team_id",
                "team_uid",
                "order",
                "homeAway",
                "winner",
                "roster_href"
              ),
            by = c(
              "team_id" = "team_id",
              "team_uid" = "team_uid"
            )
          )
      })

      ## Inputs
      ## game_id
      team_roster_df <- purrr::map_dfr(teams_df$team_id, function(x) {
        res <- .retry_request(paste0(play_base_url, x, "/roster"))

        # Check the result
        check_status(res)

        resp <- res %>%
          .resp_text()

        raw_play_df <- jsonlite::fromJSON(resp)[["entries"]]

        raw_play_df <- raw_play_df %>%
          jsonlite::toJSON() %>%
          jsonlite::fromJSON(flatten = TRUE) %>%
          dplyr::mutate(team_id = x) %>%
          dplyr::select(-dplyr::any_of(c("period", "forPlayerId", "active")))

        raw_play_df <- raw_play_df %>%
          dplyr::left_join(teams_df, by = c("team_id" = "team_id"))
      })

      colnames(team_roster_df) <- gsub(
        ".\\$ref",
        "_href",
        colnames(team_roster_df)
      )

      athlete_roster_df <- purrr::map_dfr(
        team_roster_df$athlete_href,
        function(x) {
          res <- .retry_request(x)

          # Check the result
          check_status(res)

          resp <- res %>%
            .resp_text()

          raw_play_df <- jsonlite::fromJSON(resp, flatten = TRUE)
          raw_play_df[["links"]] <- NULL
          raw_play_df[["injuries"]] <- NULL
          raw_play_df[["teams"]] <- NULL
          raw_play_df[["team"]] <- NULL
          raw_play_df[["college"]] <- NULL
          raw_play_df[["proAthlete"]] <- NULL
          raw_play_df[["statistics"]] <- NULL
          raw_play_df[["notes"]] <- NULL
          raw_play_df[["eventLog"]] <- NULL
          raw_play_df[["$ref"]] <- NULL
          raw_play_df[["position"]][["$ref"]] <- NULL

          raw_play_df2 <- raw_play_df %>%
            jsonlite::toJSON() %>%
            jsonlite::fromJSON(flatten = TRUE) %>%
            as.data.frame() %>%
            dplyr::mutate(id = as.integer(.data$id)) %>%
            dplyr::rename(
              dplyr::any_of(c(
                "athlete_id" = "id",
                "athlete_uid" = "uid",
                "athlete_guid" = "guid",
                "athlete_type" = "type",
                "athlete_display_name" = "displayName",
                "athlete_jersey_number" = "jersey"
              ))
            )

          raw_play_df2 <- raw_play_df2 %>%
            dplyr::left_join(team_roster_df, by = c("athlete_id" = "playerId"))
        }
      )

      colnames(athlete_roster_df) <- gsub(
        ".\\$ref",
        "_href",
        colnames(athlete_roster_df)
      )

      athlete_roster_df <- athlete_roster_df %>%
        janitor::clean_names() %>%
        dplyr::select(
          -dplyr::any_of(c(
            "x_ref",
            "x_ref_1",
            "contract_ref",
            "contract_ref_1",
            "contract_ref_2",
            "draft_ref",
            "draft_ref_1",
            "athlete_href",
            "position_ref",
            "position_href",
            "roster_href",
            "statistics_href"
          ))
        ) %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::any_of(c(
              "game_id",
              "athlete_id",
              "team_id",
              "position_id",
              "status_id",
              "sdr"
            )),
            as.integer
          )
        ) %>%
        make_baseballr_data(
          "ESPN College Baseball Game Roster Information from ESPN.com",
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no game roster data for {game_id} available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(athlete_roster_df)
}
