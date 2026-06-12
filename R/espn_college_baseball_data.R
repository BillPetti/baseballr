# NCAA college-baseball twins of the "old-style" ESPN aggregator functions in
# R/espn_mlb_data.R. ESPN serves the identical JSON structure for college
# baseball at /baseball/college-baseball/ as it does for MLB at /baseball/mlb/,
# so the parser bodies are copied verbatim and only the URL slug and the
# human-readable source labels change. These are full self-contained functions
# (not thin shims) that build an ESPN URL and parse the JSON.

#' **Get ESPN College Baseball team names and IDs**
#' @title **Get ESPN College Baseball Teams**
#' @rdname espn_college_baseball_teams
#' @author Saiem Gilani
#' @inherit espn_mlb_teams return
#'
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows row_number group_by mutate as_tibble ungroup
#' @importFrom tidyr unnest unnest_wider everything pivot_wider
#' @import rvest
#' @export
#' @family ESPN College Baseball Functions
#'
#' @examples
#' \donttest{
#' try(espn_college_baseball_teams())
#' }
#'
espn_college_baseball_teams <- function() {
  .args <- .capture_args()
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  teams_url <- "http://site.api.espn.com/apis/site/v2/sports/baseball/college-baseball/teams?limit=1000"

  teams <- .empty_baseballr_data("ESPN College Baseball Teams Information from ESPN.com")

  tryCatch(
    expr = {
      res <- .retry_request(teams_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      leagues <- jsonlite::fromJSON(resp)[["sports"]][["leagues"]][[1]][[
        "teams"
      ]][[1]][["team"]] %>%
        dplyr::group_by(.data$id) %>%
        tidyr::unnest_wider("logos", names_sep = "_") %>%
        tidyr::unnest_wider("logos_href", names_sep = "_") %>%
        dplyr::select(
          -"logos_width",
          -"logos_height",
          -"logos_alt",
          -"logos_rel"
        ) %>%
        dplyr::ungroup()

      if ("records" %in% colnames(leagues)) {
        records <- leagues$record
        records <- records %>%
          tidyr::unnest_wider("items") %>%
          tidyr::unnest_wider("stats", names_sep = "_") %>%
          dplyr::mutate(row = dplyr::row_number())
        stat <- records %>%
          dplyr::group_by(.data$row) %>%
          purrr::map_if(is.data.frame, list)
        stat <- lapply(stat$stats_1, function(x) {
          x %>%
            purrr::map_if(is.data.frame, list) %>%
            dplyr::as_tibble()
        })

        s <- lapply(stat, function(x) {
          tidyr::pivot_wider(x)
        })

        s <- tibble::tibble(g = s)
        stats <- s %>%
          tidyr::unnest_wider("g")

        records <- dplyr::bind_cols(records %>% dplyr::select("summary"), stats)
        leagues <- leagues %>%
          dplyr::select(-"record")
      }
      leagues <- leagues %>%
        dplyr::select(
          -"links",
          -"isActive",
          -"isAllStar",
          -"uid",
          -"slug"
        )
      teams <- leagues %>%
        # any_of() guards: the college-baseball payload can omit columns the
        # MLB payload carries (e.g. alternateColor), so a bare rename would
        # error. Renames no-op when the source column is absent.
        dplyr::rename(dplyr::any_of(c(
          "logo" = "logos_href_1",
          "logo_dark" = "logos_href_2",
          "mascot" = "name",
          "team" = "location",
          "team_id" = "id",
          "short_name" = "shortDisplayName",
          "alternate_color" = "alternateColor",
          "display_name" = "displayName"
        ))) %>%
        janitor::clean_names() %>%
        dplyr::mutate(team_id = as.integer(.data$team_id)) %>%
        make_baseballr_data("ESPN College Baseball Teams Information from ESPN.com", Sys.time())
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no teams data available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(teams)
}


#' **Get ESPN College Baseball schedule for a specific year**
#' @title **Get ESPN College Baseball Scoreboard**
#' @rdname espn_college_baseball_scoreboard
#' @author Saiem Gilani
#' @inheritParams espn_mlb_scoreboard
#' @inherit espn_mlb_scoreboard return
#'
#' @import utils
#' @importFrom dplyr select rename any_of mutate
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest_wider unchop hoist
#' @importFrom lubridate with_tz ymd_hm
#' @import rvest
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#'
#' # Get scoreboard from a College World Series date (2025-06-15).
#' \donttest{
#' try(espn_college_baseball_scoreboard(season = "20250615"))
#' }
espn_college_baseball_scoreboard <- function(season) {
  .args <- mget(setdiff(names(formals()), "..."))
  max_year <- substr(Sys.Date(), 1, 4)

  if (!(as.integer(substr(season, 1, 4)) > 2001)) {
    message(paste("Error: Season must be between 2001 and", max_year + 1))
  }

  # year > 2000
  season <- as.character(season)

  season_dates <- season

  schedule_api <- sprintf(
    "http://site.api.espn.com/apis/site/v2/sports/baseball/college-baseball/scoreboard?limit=1000&dates=%s",
    season_dates
  )

  tryCatch(
    expr = {
      res <- .retry_request(schedule_api)

      # Check the result
      check_status(res)

      raw_sched <- res %>%
        .resp_text() %>%
        jsonlite::fromJSON(
          simplifyDataFrame = FALSE,
          simplifyVector = FALSE,
          simplifyMatrix = FALSE
        )

      mlb_data <- raw_sched[["events"]] %>%
        tibble::tibble(data = .data$.) %>%
        tidyr::unnest_wider("data") %>%
        tidyr::unchop("competitions") %>%
        dplyr::select(
          -"id",
          -"uid",
          -"date",
          -"status"
        ) %>%
        tidyr::unnest_wider("competitions") %>%
        dplyr::rename(
          "matchup" = "name",
          "matchup_short" = "shortName",
          "game_id" = "id",
          "game_uid" = "uid",
          "game_date" = "date"
        ) %>%
        tidyr::hoist("status", status_name = list("type", "name")) %>%
        dplyr::select(
          !dplyr::any_of(
            c(
              "timeValid",
              "neutralSite",
              "conferenceCompetition",
              "recent",
              "venue",
              "type"
            )
          )
        ) %>%
        tidyr::unnest_wider("season", names_sep = "_") %>%
        dplyr::rename("season" = "season_year") %>%
        dplyr::select(-dplyr::any_of("status"))

      mlb_data <- mlb_data %>%
        dplyr::mutate(
          game_date_time = lubridate::ymd_hm(substr(
            .data$game_date,
            1,
            nchar(.data$game_date) - 1
          )) %>%
            lubridate::with_tz(tzone = "America/New_York"),
          game_date = as.Date(substr(.data$game_date_time, 1, 10))
        )

      mlb_data <- mlb_data %>%
        tidyr::hoist(
          "competitors",
          homeAway = list(1, "homeAway")
        )
      mlb_data <- mlb_data %>%
        tidyr::hoist(
          "competitors",
          team1_team_name = list(1, "team", "name"),
          team1_team_logo = list(1, "team", "logo"),
          team1_team_abb = list(1, "team", "abbreviation"),
          team1_team_id = list(1, "team", "id"),
          team1_team_location = list(1, "team", "location"),
          team1_team_full = list(1, "team", "displayName"),
          team1_team_color = list(1, "team", "color"),
          team1_score = list(1, "score"),
          team1_win = list(1, "winner"),
          team1_record = list(1, "records", 1, "summary"),
          # away team
          team2_team_name = list(2, "team", "name"),
          team2_team_logo = list(2, "team", "logo"),
          team2_team_abb = list(2, "team", "abbreviation"),
          team2_team_id = list(2, "team", "id"),
          team2_team_location = list(2, "team", "location"),
          team2_team_full = list(2, "team", "displayName"),
          team2_team_color = list(2, "team", "color"),
          team2_score = list(2, "score"),
          team2_win = list(2, "winner"),
          team2_record = list(2, "records", 1, "summary")
        )

      mlb_data <- mlb_data %>%
        dplyr::mutate(
          home_team_name = ifelse(
            .data$homeAway == "home",
            .data$team1_team_name,
            .data$team2_team_name
          ),
          home_team_logo = ifelse(
            .data$homeAway == "home",
            .data$team1_team_logo,
            .data$team2_team_logo
          ),
          home_team_abb = ifelse(
            .data$homeAway == "home",
            .data$team1_team_abb,
            .data$team2_team_abb
          ),
          home_team_id = ifelse(
            .data$homeAway == "home",
            .data$team1_team_id,
            .data$team2_team_id
          ),
          home_team_location = ifelse(
            .data$homeAway == "home",
            .data$team1_team_location,
            .data$team2_team_location
          ),
          home_team_full_name = ifelse(
            .data$homeAway == "home",
            .data$team1_team_full,
            .data$team2_team_full
          ),
          home_team_color = ifelse(
            .data$homeAway == "home",
            .data$team1_team_color,
            .data$team2_team_color
          ),
          home_score = ifelse(
            .data$homeAway == "home",
            .data$team1_score,
            .data$team2_score
          ),
          home_win = ifelse(
            .data$homeAway == "home",
            .data$team1_win,
            .data$team2_win
          ),
          home_record = ifelse(
            .data$homeAway == "home",
            .data$team1_record,
            .data$team2_record
          ),
          away_team_name = ifelse(
            .data$homeAway == "away",
            .data$team1_team_name,
            .data$team2_team_name
          ),
          away_team_logo = ifelse(
            .data$homeAway == "away",
            .data$team1_team_logo,
            .data$team2_team_logo
          ),
          away_team_abb = ifelse(
            .data$homeAway == "away",
            .data$team1_team_abb,
            .data$team2_team_abb
          ),
          away_team_id = ifelse(
            .data$homeAway == "away",
            .data$team1_team_id,
            .data$team2_team_id
          ),
          away_team_location = ifelse(
            .data$homeAway == "away",
            .data$team1_team_location,
            .data$team2_team_location
          ),
          away_team_full_name = ifelse(
            .data$homeAway == "away",
            .data$team1_team_full,
            .data$team2_team_full
          ),
          away_team_color = ifelse(
            .data$homeAway == "away",
            .data$team1_team_color,
            .data$team2_team_color
          ),
          away_score = ifelse(
            .data$homeAway == "away",
            .data$team1_score,
            .data$team2_score
          ),
          away_win = ifelse(
            .data$homeAway == "away",
            .data$team1_win,
            .data$team2_win
          ),
          away_record = ifelse(
            .data$homeAway == "away",
            .data$team1_record,
            .data$team2_record
          )
        )

      mlb_data <- mlb_data %>%
        dplyr::mutate_at(
          c(
            "game_id",
            "home_team_id",
            "home_win",
            "away_team_id",
            "away_win",
            "home_score",
            "away_score"
          ),
          as.integer
        )
      mlb_data <- mlb_data %>%
        dplyr::select(
          -dplyr::any_of(dplyr::starts_with("team1")),
          -dplyr::any_of(dplyr::starts_with("team2")),
          -dplyr::any_of(c("homeAway"))
        )

      if ("leaders" %in% names(mlb_data)) {
        schedule_out <- mlb_data %>%
          tidyr::hoist(
            "leaders",
            # batting-average leader (category 1)
            batting_leader_value = list(1, "leaders", 1, "value"),
            batting_leader_stat = list(1, "leaders", 1, "displayValue"),
            batting_leader_name = list(
              1,
              "leaders",
              1,
              "athlete",
              "displayName"
            ),
            batting_leader_shortname = list(
              1,
              "leaders",
              1,
              "athlete",
              "shortName"
            ),
            batting_leader_headshot = list(
              1,
              "leaders",
              1,
              "athlete",
              "headshot"
            ),
            batting_leader_team_id = list(1, "leaders", 1, "team", "id"),
            batting_leader_pos = list(
              1,
              "leaders",
              1,
              "athlete",
              "position",
              "abbreviation"
            ),
            # home-runs leader (category 2)
            home_run_leader_value = list(2, "leaders", 1, "value"),
            home_run_leader_stat = list(2, "leaders", 1, "displayValue"),
            home_run_leader_name = list(
              2,
              "leaders",
              1,
              "athlete",
              "displayName"
            ),
            home_run_leader_shortname = list(
              2,
              "leaders",
              1,
              "athlete",
              "shortName"
            ),
            home_run_leader_headshot = list(
              2,
              "leaders",
              1,
              "athlete",
              "headshot"
            ),
            home_run_leader_team_id = list(2, "leaders", 1, "team", "id"),
            home_run_leader_pos = list(
              2,
              "leaders",
              1,
              "athlete",
              "position",
              "abbreviation"
            ),
            # RBIs leader (category 3)
            rbi_leader_value = list(3, "leaders", 1, "value"),
            rbi_leader_stat = list(3, "leaders", 1, "displayValue"),
            rbi_leader_name = list(
              3,
              "leaders",
              1,
              "athlete",
              "displayName"
            ),
            rbi_leader_shortname = list(
              3,
              "leaders",
              1,
              "athlete",
              "shortName"
            ),
            rbi_leader_headshot = list(
              3,
              "leaders",
              1,
              "athlete",
              "headshot"
            ),
            rbi_leader_team_id = list(3, "leaders", 1, "team", "id"),
            rbi_leader_pos = list(
              3,
              "leaders",
              1,
              "athlete",
              "position",
              "abbreviation"
            ),
          )

        if (
          "broadcasts" %in%
            names(schedule_out) &&
            !any(is.na(schedule_out[["broadcasts"]]))
        ) {
          schedule_out %>%
            tidyr::hoist(
              "broadcasts",
              broadcast_market = list(1, "market"),
              broadcast_name = list(1, "names", 1)
            ) %>%
            dplyr::select(!where(is.list)) %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              "ESPN College Baseball Scoreboard Information from ESPN.com",
              Sys.time()
            )
        } else {
          schedule_out %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              "ESPN College Baseball Scoreboard Information from ESPN.com",
              Sys.time()
            )
        }
      } else {
        if (
          "broadcasts" %in%
            names(mlb_data) &&
            !any(is.na(mlb_data[["broadcasts"]]))
        ) {
          mlb_data %>%
            tidyr::hoist(
              "broadcasts",
              broadcast_market = list(1, "market"),
              broadcast_name = list(1, "names", 1)
            ) %>%
            dplyr::select(!where(is.list)) %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              "ESPN College Baseball Scoreboard Information from ESPN.com",
              Sys.time()
            )
        } else {
          mlb_data %>%
            dplyr::select(!where(is.list)) %>%
            janitor::clean_names() %>%
            make_baseballr_data(
              "ESPN College Baseball Scoreboard Information from ESPN.com",
              Sys.time()
            )
        }
      }
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no scoreboard data available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
}


#' **Get ESPN College Baseball's Standings**
#' @title **Get ESPN College Baseball Standings**
#' @rdname espn_college_baseball_standings
#' @author Saiem Gilani
#' @inheritParams espn_mlb_standings
#' @inherit espn_mlb_standings return
#'
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr select rename
#' @importFrom tidyr pivot_wider
#' @importFrom data.table rbindlist
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#' try(espn_college_baseball_standings(year = 2025))
#' }
espn_college_baseball_standings <- function(year) {
  .args <- mget(setdiff(names(formals()), "..."))
  standings_url <- "https://site.web.api.espn.com/apis/v2/sports/baseball/college-baseball/standings?region=us&lang=en&contentorigin=espn&type=0&level=1&sort=winpercent%3Adesc%2Cwins%3Adesc%2Cgamesbehind%3Aasc&"

  ## Inputs
  ## year
  full_url <- paste0(
    standings_url,
    "season=",
    year
  )

  standings <- .empty_baseballr_data("ESPN College Baseball Standings Information from ESPN.com")

  tryCatch(
    expr = {
      res <- .retry_request(full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        .resp_text()

      raw_standings <- jsonlite::fromJSON(resp)[["standings"]]

      # Create a dataframe of all teams by extracting from the raw_standings file

      teams <- raw_standings[["entries"]][["team"]]

      teams <- teams %>%
        dplyr::select("id", "displayName") %>%
        dplyr::rename(
          "team_id" = "id",
          "team" = "displayName"
        )

      # creating a dataframe of the raw standings table from ESPN

      standings_df <- raw_standings[["entries"]][["stats"]]

      standings_data <- data.table::rbindlist(
        standings_df,
        fill = TRUE,
        idcol = T
      )

      # Use the following code to replace NA's in the dataframe with the correct corresponding values and removing all unnecessary columns

      standings_data$value <- ifelse(
        is.na(standings_data$value) & !is.na(standings_data$summary),
        standings_data$summary,
        standings_data$value
      )

      standings_data <- standings_data %>%
        dplyr::select(
          ".id",
          "type",
          "value"
        )

      # Use pivot_wider to transpose the dataframe so that we now have a standings row for each team

      standings_data <- standings_data %>%
        tidyr::pivot_wider(names_from = "type", values_from = "value")

      standings_data <- standings_data %>%
        dplyr::select(-".id")

      # joining the 2 dataframes together to create a standings table

      standings <- cbind(teams, standings_data) %>%
        dplyr::mutate(team_id = as.integer(.data$team_id)) %>%
        # any_of() guard: the college-baseball standings payload omits some
        # stat types the MLB payload carries (e.g. clincher, playoffseed), so
        # the bare column vector would error. Coerce only the columns present.
        dplyr::mutate_at(
          dplyr::vars(dplyr::any_of(c(
            "avgpointsagainst",
            "avgpointsfor",
            "clincher",
            "differential",
            "divisionwinpercent",
            "gamesbehind",
            "leaguewinpercent",
            "losses",
            "playoffseed",
            "streak",
            "winpercent",
            "wins"
          ))),
          as.numeric
        )
      standings <- standings %>%
        make_baseballr_data(
          "ESPN College Baseball Standings Information from ESPN.com",
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no standings data available!",
      args = .args
    ),
    warning = function(w) {},
    finally = {}
  )
  return(standings)
}
