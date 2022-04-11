#' @rdname mlb_probables 
#' @title **Retrieve probable starters for a given MLB game**
#' @param game_pk The unique game_pk identifier for the game
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @return Returns a data frame that includes probable starting pitchers and
#' the home plate umpire for the `game_pk` requested
#'  |col_name             |types     |
#'  |:--------------------|:---------|
#'  |game_pk              |integer   |
#'  |game_date            |character |
#'  |fullName             |character |
#'  |id                   |integer   |
#'  |team                 |character |
#'  |team_id              |integer   |
#'  |home_plate_full_name |character |
#'  |home_plate_id        |integer   |
#' @export
#' @examples \donttest{
#'   try(mlb_probables(566001))
#' }

mlb_probables <- function(game_pk) {
  oldw <- getOption("warn")
  options(warn = -1)
  api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/", game_pk,"/feed/live")
  
  tryCatch(
    expr={
      payload <- jsonlite::fromJSON(api_call, flatten = TRUE)
      away_probable <- if(is.null(payload$gameData$probablePitchers$away)) {
        return_table <- tibble(id = NA,
                               fullName = NA,
                               link = NA)
      } else {
        return_table <- payload$gameData$probablePitchers$away %>%
          as.data.frame()
      }
      home_probable <- if(is.null(payload$gameData$probablePitchers$home)) {
        return_table <- tibble(id = NA,
                               fullName = NA,
                               link = NA)
      } else {
        return_table <- payload$gameData$probablePitchers$home %>%
          as.data.frame()
      }
      parse_teams <- function(payload_teams) {
        return_table <- tibble(team = payload_teams$name,
                               team_id = payload_teams$id)
        return(return_table)
      }
      
      away_team <- parse_teams(payload$gameData$teams$away)
      home_team <- parse_teams(payload$gameData$teams$home)
      
      teams <- dplyr::bind_rows(away_team,
                                home_team)
      
      if(length(payload$liveData$boxscore$officials) > 0) {
        
        umpires <- payload$liveData$boxscore$officials %>%
          dplyr::filter(.data$officialType == "Home Plate") %>%
          dplyr::rename(
            home_plate_type = .data$officialType,
            home_plate_id = .data$official.id,
            home_plate_full_name = .data$official.fullName,
            home_plate_link = .data$official.link) %>%
          dplyr::select(.data$home_plate_id, .data$home_plate_full_name)
        
      } else {
        
        umpires <- tibble(home_plate_id  = NA,
                          home_plate_full_name = NA)
      }
      
      probs <- dplyr::bind_rows(away_probable,
                                home_probable)
      
      table <- dplyr::bind_cols(probs,
                                teams)
      
      table <- table %>%
        dplyr::mutate(
          home_plate_id = umpires$home_plate_id,
          home_plate_full_name = umpires$home_plate_full_name)
      
      table <- table %>%
        dplyr::mutate(
          game_pk = payload$gamePk,
          game_date = stringr::str_sub(payload$gameData$game$calendarEventID, -10, -1)) %>%
        dplyr::select(.data$game_pk, .data$game_date, .data$fullName, .data$id, .data$team, .data$team_id,
                      .data$home_plate_full_name, .data$home_plate_id) %>%
        make_baseballr_data("MLB Probables data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  options(warn = oldw)
  
  return(table)
}

#' @rdname get_probables_mlb
#' @title **(legacy) Retrieve probable starters for a given MLB game**
#' @inheritParams mlb_probables
#' @return Returns a data frame that includes probable starting pitchers and
#' the home plate umpire for the `game_pk` requested
#' @keywords legacy
#' @export
get_probables_mlb <- mlb_probables