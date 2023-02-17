#' @rdname ncaa_schedule_info
#' @title **Get Schedule and Results for NCAA Baseball Teams**
#' @param teamid The team's unique NCAA id.
#' @param year The season (i.e. use 2016 for the 2015-2016 season,
#' etc.)
#' @return A data frame with the following fields: date, opponent,
#' result, score, innings (if more than regulation), and the url
#' for the game itself.
#' 
#'   |col_name                |types     |
#'   |:-----------------------|:---------|
#'   |year                    |integer   |
#'   |season_id               |integer   |
#'   |date                    |character |
#'   |home_team               |character |
#'   |home_team_id            |integer   |
#'   |home_team_conference    |character |
#'   |home_team_conference_id |integer   |
#'   |home_team_slug          |character |
#'   |home_team_division      |integer   |
#'   |away_team               |character |
#'   |away_team_id            |integer   |
#'   |away_team_conference    |character |
#'   |away_team_conference_id |integer   |
#'   |away_team_slug          |character |
#'   |away_team_division      |integer   |
#'   |neutral_site            |character |
#'   |result                  |character |
#'   |score                   |character |
#'   |innings                 |character |
#'   |slug                    |character |
#'   |game_info_url           |character |
#'    
#' @importFrom tibble tibble rownames_to_column
#' @importFrom tidyr separate
#' @importFrom stringr str_trim str_extract
#' @import rvest 
#' @export
#' @examples \donttest{
#'  try(ncaa_schedule_info(teamid = 698, year = 2023))
#' }

ncaa_schedule_info <- function(teamid = NULL, year = NULL){
  season_ids <- load_ncaa_baseball_season_ids()
  id <- subset(season_ids, season_ids$season == year, select = id)
  year2 <- year
  ncaa_baseball_teams <- load_ncaa_baseball_teams()
  school_info <- ncaa_baseball_teams %>% 
    dplyr::filter(.data$team_id == teamid, as.integer(.data$year) == as.integer(year2)) %>%
    dplyr::distinct()
  
  url <- paste0("https://stats.ncaa.org/team/", teamid, "/", id)
  
  tryCatch(
    expr = {
      
      payload <- url %>% xml2::read_html()
      
      if (year > 2018) {
        sched_html <- payload %>%
          rvest::html_elements("fieldset") %>%
          rvest::html_elements("table") 
        sched_1 <- (payload %>%
                      rvest::html_elements("fieldset") %>%
                      rvest::html_elements("table")) [[1]] %>%
          rvest::html_elements("tr")
        if (length(sched_1) > 1) {
          sched_1 <- sched_1[2:length(sched_1)]
          sched_1 <- sched_1[c(seq(1,length(sched_1),2))]
          sched <- sched_html %>%
            rvest::html_table() %>%
            as.data.frame() 
          sched <- sched %>%
            dplyr::filter(.data$Date != "") %>% 
            dplyr::select(-dplyr::any_of("Attendance"))
        } else {
          sched <- data.frame()
          cli::cli_warn(glue::glue("No NCAA Schedule Information found for params (team_id: {teamid}, year = {year})"))
          return(NULL)
        }
      } else {
        sched_html <- payload %>% 
          rvest::html_element("td:nth-child(1) > table") 
        sched_1 <- (payload  %>% 
                      rvest::html_element("td:nth-child(1) > table")) %>%
          rvest::html_elements("tr")
        sched_1 <- sched_1[3:length(sched_1)]
        sched <- sched_html %>%
          rvest::html_table() %>%
          as.data.frame() 
        colnames(sched) <- c("Date", "Opponent", "Result") 
        sched <- sched[3:nrow(sched),]   
      }
      
      
      sched <- sched %>%
        dplyr::filter(.data$Date != "")
      
      game_extractor <- function(x){
        data.frame(slug = ifelse(
          is.null(
            (x %>%
               rvest::html_elements("td:nth-child(3)") %>% 
               rvest::html_elements("a.skipMask"))), 
          NA_character_,
          (x %>%
             rvest::html_elements("td:nth-child(3)") %>% 
             rvest::html_elements("a.skipMask")) %>% 
            html_attr("href")
        ))
      }
      slugs <- lapply(sched_1, game_extractor) %>% 
        dplyr::bind_rows() 
      
      sched$opponent_slug <- sched_html %>%
        rvest::html_elements("td:nth-child(2)") %>%
        rvest::html_element("a") %>%
        rvest::html_attr("href")
      
      sched <- dplyr::bind_cols(sched, slugs)
      sched <- sched %>%
        dplyr::filter(!(.data$Result %in% c("Canceled","Ppd")))
      
      sched <- sched %>%
        dplyr::mutate(
          Date = substr(.data$Date,1,10),
          game_info_url = ifelse(!is.na(.data$slug), paste0("https://stats.ncaa.org", .data$slug), NA_character_))
      
      suppressWarnings(
        sched <- sched %>%
          tidyr::separate("Result", into = c("Result", "Score", "Innings"),
                          sep = " ") %>%
          # dplyr::filter(.data$Result != "RPI") %>% 
          dplyr::mutate(
            Innings = stringr::str_extract(.data$Innings,"\\d+")
          )
        
      )
      sched <- school_info %>% 
        dplyr::bind_cols(sched)
      
      sched <- sched %>% 
        dplyr::mutate(
          away = stringr::str_extract(string = .data$Opponent,"^@"),
          Opponent = stringr::str_trim(stringr::str_remove(.data$Opponent,"^@")),
          game_number = dplyr::row_number()
        )
      
      sched <- sched %>% 
        tidyr::separate("Opponent", sep = "@", into = c("OpponentName","NeutralSite"), fill = "right")
      
      sched <- sched %>% 
        dplyr::group_by(.data$game_number) %>% 
        dplyr::mutate(
          home_team = dplyr::case_when(
            !is.na(.data$NeutralSite) ~ sort(c(.data$team_name, .data$OpponentName))[[1]],
            .data$away == "@" ~ .data$OpponentName,
            TRUE ~ .data$team_name),
          away_team = dplyr::case_when(
            !is.na(.data$NeutralSite) ~ sort(c(.data$team_name, .data$OpponentName))[[2]],
            .data$away == "@" ~ .data$team_name,
            TRUE ~ .data$OpponentName),
          OpponentName = stringr::str_trim(.data$OpponentName),
          NeutralSite = stringr::str_trim(.data$NeutralSite)) %>% 
        dplyr::ungroup() %>%  
        dplyr::select(-"away") %>% 
        dplyr::left_join(ncaa_baseball_teams %>% 
                           dplyr::filter(as.integer(.data$year) == as.integer(year2)) %>%
                           dplyr::distinct() %>% 
                           dplyr::select(
                             "OpponentName" = "team_name",
                             "OpponentTeamId" = "team_id",
                             "OpponentConference" = "conference",
                             "OpponentConferenceId" = "conference_id",
                             "OpponentTeamSlug" = "team_url",
                             "OpponentDivision" = "division"), by = c("OpponentName" = "OpponentName")) %>%
        janitor::clean_names() 
      sched <- sched %>% 
        dplyr::mutate(
          home_team_id = ifelse(.data$home_team == .data$team_name, .data$team_id, .data$opponent_team_id),
          home_team_conference = ifelse(.data$home_team == .data$team_name, .data$conference, .data$opponent_conference),
          home_team_conference_id = ifelse(.data$home_team == .data$team_name, .data$conference_id, .data$opponent_conference_id),
          home_team_slug = ifelse(.data$home_team == .data$team_name, .data$team_url, .data$opponent_team_slug),
          home_team_division = ifelse(.data$home_team == .data$team_name, .data$division, .data$opponent_division),
          
          away_team_id = ifelse(.data$home_team == .data$team_name, .data$opponent_team_id, .data$team_id),
          away_team_conference = ifelse(.data$home_team == .data$team_name, .data$opponent_conference, .data$conference),
          away_team_conference_id = ifelse(.data$home_team == .data$team_name, .data$opponent_conference_id, .data$conference_id),
          away_team_slug = ifelse(.data$home_team == .data$team_name, .data$opponent_team_slug, .data$team_url),
          away_team_division = ifelse(.data$home_team == .data$team_name, .data$opponent_division, .data$division)) %>% 
        dplyr::select(
          "year",
          "season_id",
          "date",
          "home_team",
          "home_team_id",
          "home_team_conference",
          "home_team_conference_id",
          "home_team_slug",
          "home_team_division",
          "away_team",
          "away_team_id",
          "away_team_conference",
          "away_team_conference_id",
          "away_team_slug",
          "away_team_division",
          "neutral_site",
          "result",
          "score",
          "innings",
          "slug",
          "game_info_url"
        )
      
      sched <- sched %>%
        make_baseballr_data("NCAA Baseball Schedule Info data from stats.ncaa.org", Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided (team_id: {teamid}, year = {year})"))
    },
    finally = {
    }
  )
  return(sched)
}

#' @rdname get_ncaa_schedule_info
#' @title **(legacy) Get Schedule and Results for NCAA Baseball Teams**
#' @inheritParams ncaa_schedule_info
#' @return A data frame with the following fields: date, opponent,
#' result, score, innings (if more than regulation), and the url
#' for the game itself.
#' @keywords legacy
#' @export
get_ncaa_schedule_info <- ncaa_schedule_info
