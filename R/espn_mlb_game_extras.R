# Baseball-specific extractors from the ESPN MLB game-summary endpoint that have
# no basketball analogue: probable starting pitchers and the game-info block
# (venue, attendance, duration, and the umpire crew).

#' **Get ESPN MLB Probable Starting Pitchers**
#' @name espn_mlb_game_probables
NULL
#' @title
#' **Get ESPN MLB Probable Starting Pitchers**
#' @rdname espn_mlb_game_probables
#' @author Saiem Gilani
#' @description
#' Returns the probable / announced starting pitchers for a game, one row per
#' team, parsed from the ESPN game-summary `header.competitions.competitors`
#' `probables` block. Available for upcoming and completed games.
#' @param game_id Game ID -- ESPN event identifier (character or numeric).
#' @return A `baseballr_data` tibble with one row per team's probable starter.
#'
#'    |col_name             |types     |description                                  |
#'    |:--------------------|:---------|:--------------------------------------------|
#'    |game_id              |integer   |Unique ESPN game/event identifier.           |
#'    |season               |integer   |Season (4-digit year).                       |
#'    |season_type          |integer   |ESPN season type (1=pre, 2=regular, 3=post). |
#'    |team_id              |integer   |Unique ESPN team identifier.                 |
#'    |team_abbreviation    |character |Short team abbreviation (e.g. 'NYY').        |
#'    |team_display_name    |character |Full team display name.                      |
#'    |home_away            |character |Venue label for the team ('home' or 'away'). |
#'    |athlete_id           |integer   |Unique ESPN athlete identifier (pitcher).    |
#'    |athlete_full_name    |character |Pitcher full name.                           |
#'    |athlete_display_name |character |Pitcher display name.                        |
#'    |athlete_short_name   |character |Pitcher short display name.                  |
#'    |jersey               |character |Jersey number.                               |
#'    |position             |character |Position abbreviation (SP / RP / P).         |
#'    |throws               |character |Throwing handedness (L / R).                 |
#'    |headshot_href        |character |Headshot image URL.                          |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   try(espn_mlb_game_probables(game_id = "401570000"))
#' }
espn_mlb_game_probables <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  url <- paste0(
    "http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?event=",
    game_id
  )
  out <- .empty_baseballr_data("ESPN MLB Probable Pitchers from ESPN.com")
  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      j <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyVector = FALSE)

      header <- j[["header"]]
      comp <- header[["competitions"]][[1]]
      gid <- as.integer(header[["id"]])
      season <- header[["season"]][["year"]]
      season_type <- header[["season"]][["type"]]

      rows <- list()
      i <- 1L
      for (c in comp[["competitors"]]) {
        tm <- c[["team"]]
        probs <- c[["probables"]]
        if (is.null(probs) || length(probs) == 0) next
        for (p in probs) {
          a <- p[["athlete"]]
          rows[[i]] <- data.frame(
            game_id              = gid,
            season               = season,
            season_type          = season_type,
            team_id              = as.integer(tm[["id"]]),
            team_abbreviation    = tm[["abbreviation"]] %||% NA_character_,
            team_display_name    = tm[["displayName"]] %||% NA_character_,
            home_away            = c[["homeAway"]] %||% NA_character_,
            athlete_id           = suppressWarnings(as.integer(a[["id"]])),
            athlete_full_name    = a[["fullName"]] %||% NA_character_,
            athlete_display_name = a[["displayName"]] %||% NA_character_,
            athlete_short_name   = a[["shortName"]] %||% NA_character_,
            jersey               = a[["jersey"]] %||% NA_character_,
            position             = a[["position"]][["abbreviation"]] %||% NA_character_,
            throws               = a[["throws"]][["abbreviation"]] %||%
                                     (if (is.character(a[["throws"]])) a[["throws"]] else NA_character_),
            headshot_href        = a[["headshot"]][["href"]] %||% NA_character_,
            stringsAsFactors = FALSE
          )
          i <- i + 1L
        }
      }
      if (length(rows) > 0) {
        out <- dplyr::bind_rows(rows) %>%
          janitor::clean_names() %>%
          make_baseballr_data("ESPN MLB Probable Pitchers from ESPN.com", Sys.time())
      }
    },
    error = function(e) .report_api_error(
      e, hint = "No probable-pitcher data for {game_id} available!", args = .args
    ),
    warning = function(w) .report_api_warning(
      w, hint = "Warning retrieving probable pitchers for {game_id}", args = .args
    ),
    finally = {}
  )
  return(out)
}


#' **Get ESPN MLB Game Info (venue, attendance, duration, umpires)**
#' @name espn_mlb_game_info
NULL
#' @title
#' **Get ESPN MLB Game Info (venue, attendance, duration, umpires)**
#' @rdname espn_mlb_game_info
#' @author Saiem Gilani
#' @description
#' Returns one wide row of game metadata from the ESPN game-summary `gameInfo`
#' block: venue, attendance, game duration, and the umpire crew (home-plate,
#' first-, second-, and third-base umpires).
#' @param game_id Game ID -- ESPN event identifier (character or numeric).
#' @return A single-row `baseballr_data` tibble.
#'
#'    |col_name             |types     |description                               |
#'    |:--------------------|:---------|:-----------------------------------------|
#'    |game_id              |integer   |Unique ESPN game/event identifier.        |
#'    |venue_id             |character |Unique ESPN venue identifier.             |
#'    |venue_name           |character |Venue (ballpark) full name.               |
#'    |venue_city           |character |Venue city.                               |
#'    |venue_state          |character |Venue state / province.                   |
#'    |attendance           |integer   |Announced attendance.                     |
#'    |game_duration        |character |Elapsed game time (H:MM).                 |
#'    |home_plate_umpire    |character |Home-plate umpire.                        |
#'    |first_base_umpire    |character |First-base umpire.                        |
#'    |second_base_umpire   |character |Second-base umpire.                       |
#'    |third_base_umpire    |character |Third-base umpire.                        |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   try(espn_mlb_game_info(game_id = "401570000"))
#' }
espn_mlb_game_info <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  url <- paste0(
    "http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?event=",
    game_id
  )
  out <- .empty_baseballr_data("ESPN MLB Game Info from ESPN.com")
  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)
      j <- res %>% .resp_text() %>% jsonlite::fromJSON(simplifyVector = FALSE)

      gi <- j[["gameInfo"]]
      venue <- gi[["venue"]]
      addr <- venue[["address"]]

      ump <- c(home_plate = NA_character_, first_base = NA_character_,
               second_base = NA_character_, third_base = NA_character_)
      for (o in gi[["officials"]] %||% list()) {
        pos <- tolower(o[["position"]][["displayName"]] %||% o[["position"]][["name"]] %||% "")
        nm <- trimws(o[["displayName"]] %||% o[["fullName"]] %||% NA_character_)
        if (grepl("home", pos)) ump["home_plate"] <- nm
        else if (grepl("first", pos)) ump["first_base"] <- nm
        else if (grepl("second", pos)) ump["second_base"] <- nm
        else if (grepl("third", pos)) ump["third_base"] <- nm
      }

      df <- data.frame(
        game_id            = as.integer(j[["header"]][["id"]]),
        venue_id           = venue[["id"]] %||% NA_character_,
        venue_name         = venue[["fullName"]] %||% NA_character_,
        venue_city         = addr[["city"]] %||% NA_character_,
        venue_state        = addr[["state"]] %||% NA_character_,
        attendance         = suppressWarnings(as.integer(gi[["attendance"]] %||% NA)),
        game_duration      = gi[["gameDuration"]] %||% NA_character_,
        home_plate_umpire  = unname(ump["home_plate"]),
        first_base_umpire  = unname(ump["first_base"]),
        second_base_umpire = unname(ump["second_base"]),
        third_base_umpire  = unname(ump["third_base"]),
        stringsAsFactors = FALSE
      )
      out <- df %>%
        janitor::clean_names() %>%
        make_baseballr_data("ESPN MLB Game Info from ESPN.com", Sys.time())
    },
    error = function(e) .report_api_error(
      e, hint = "No game-info data for {game_id} available!", args = .args
    ),
    warning = function(w) .report_api_warning(
      w, hint = "Warning retrieving game info for {game_id}", args = .args
    ),
    finally = {}
  )
  return(out)
}
