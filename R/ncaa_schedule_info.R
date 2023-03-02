#' @rdname ncaa_schedule_info
#' @title **Get Schedule and Results for NCAA Baseball Teams**
#' @param team_id The team's unique NCAA id.
#' @param year The season (i.e. use 2016 for the 2015-2016 season,
#' etc.)
#' @param ... Additional arguments passed to an underlying function like httr.
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
#' @details 
#' ```r
#'  try(ncaa_schedule_info(team_id = 736, year = 2019))
#' ````

ncaa_schedule_info <- function(team_id = NULL, year = NULL, ...){

  season_ids <- load_ncaa_baseball_season_ids()
  id <- subset(season_ids, season_ids$season == year, select = id)
  year2 <- year
  ncaa_baseball_teams <- load_ncaa_baseball_teams()
  school_info <- ncaa_baseball_teams %>% 
    dplyr::filter(.data$team_id == {{team_id}}, as.integer(.data$year) == as.integer(year2)) %>%
    dplyr::distinct() %>% 
    dplyr::first()
  
  url <- paste0("https://stats.ncaa.org/team/", team_id, "/", id)
  
  headers <- httr::add_headers(.headers = .ncaa_headers())
  tryCatch(
    expr = {
      content <- request_with_proxy(url = url, ..., headers)
      
      check_status(content)
      
      payload <- content %>% 
        httr::content(as = "text", encoding = "UTF-8") %>% 
        xml2::read_html()
      
      if (year > 2018) {
        sched_html <- payload %>%
          rvest::html_elements("fieldset") %>%
          rvest::html_elements("table")
        if (length(sched_html) == 0){
          sched <- data.frame()          
          cli::cli_warn(glue::glue("No NCAA Schedule Information found for params (team_id: {team_id}, year = {year})"))
          return(sched)
        }
        grey_heading <- sched_html %>%
          rvest::html_elements(".grey_heading")
        xml2::xml_remove(grey_heading)
        
        sched_1 <- (payload %>%
                      rvest::html_elements("fieldset") %>%
                      rvest::html_elements("table")) [[1]] %>%
          rvest::html_elements("tr")
        sched_2 <- (payload %>%
                      rvest::html_elements("fieldset") %>%
                      rvest::html_elements("table") %>% 
                      rvest::html_table())[[1]]
        if (nrow(sched_2) > 1) {
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
          cli::cli_warn(glue::glue("No NCAA Schedule Information found for params (team_id: {team_id}, year = {year})"))
          return(sched)
        }
      } else {
        sched_html <- payload %>% 
          rvest::html_element("td:nth-child(1) > table") 
        if (length(sched_html) == 0){
          sched <- data.frame()          
          cli::cli_warn(glue::glue("No NCAA Schedule Information found for params (team_id: {team_id}, year = {year})"))
          return(sched)
        }
        sched_1 <- (payload  %>% 
                      rvest::html_element("td:nth-child(1) > table")) %>%
          rvest::html_elements("tr")
        sched_2 <- (payload  %>% 
                      rvest::html_element("td:nth-child(1) > table") %>% 
                      rvest::html_table())
        if (nrow(sched_2) > 2){
          sched_1 <- sched_1[3:length(sched_1)]
          sched <- sched_html %>%
            rvest::html_table() %>%
            as.data.frame() 
          colnames(sched) <- c("Date", "Opponent", "Result") 
          sched <- sched[3:nrow(sched),]
        } else {
          sched <- data.frame()
          cli::cli_warn(glue::glue("No NCAA Schedule Information found for params (team_id: {team_id}, year = {year})"))
          return(sched)
        }
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
      
      if (nrow(sched) == 0) {
        cli::cli_warn(glue::glue("No NCAA Schedule Information found for params (team_id: {team_id}, year = {year})"))
        return(sched)
      }
      
      sched <- sched %>%
        dplyr::mutate(
          Date = substr(.data$Date,1,10),
          game_info_url = ifelse(!is.na(.data$slug), paste0("https://stats.ncaa.org", .data$slug), NA_character_))
      
      sched_ids_links <- purrr::map(sched$game_info_url, function(x){
        if (!is.na(x)) {
          contest_id <- as.integer(stringr::str_extract(x, "\\d+"))
          
          content <- request_with_proxy(url = x, ..., headers)
          
          check_status(content)
          
          init_payload <- content %>% 
            httr::content(as = "text", encoding = "UTF-8") %>%
            xml2::read_html() 
          
          payload <- init_payload %>% 
            rvest::html_elements("#root li:nth-child(3) a") %>%
            rvest::html_attr("href") %>%
            as.data.frame() %>%
            dplyr::rename(pbp_url_slug = ".") %>%
            dplyr::mutate(game_pbp_url = paste0("https://stats.ncaa.org", .data$pbp_url_slug)) %>%
            dplyr::pull(.data$game_pbp_url) 
          
          payload_df <- data.frame(
            game_pbp_url = payload,
            contest_id = contest_id,
            game_pbp_id = as.integer(stringr::str_extract(payload, "\\d+"))
          )
          Sys.sleep(0.2)
          return(payload_df)
        } else {
          
          payload_df <- data.frame(
            game_pbp_url = NA_character_,
            contest_id = NA_integer_,
            game_pbp_id = NA_integer_
          )
          return(payload_df)
        }
      }) %>% 
        rbindlist_with_attrs()
      sched <- sched %>% 
        dplyr::bind_cols(sched_ids_links)
      
      suppressWarnings(
        sched <- sched %>% 
          dplyr::mutate(
            game_result = stringr::str_extract(.data$Result, "\\w+"),
            Result = stringr::str_remove(.data$Result, "\\w+"),
            Innings = stringr::str_extract(.data$Result, "\\(\\d+\\)"),
            Innings = stringr::str_extract(.data$Innings, "\\d+"),
            Score = stringr::str_trim(stringr::str_remove(.data$Result, "\\(\\d+\\)"))
          ) %>% 
          dplyr::select(-"Result")
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
      suppressWarnings(
        sched <- sched %>% 
          tidyr::separate("score", sep = "-", into = c("team_score", "opponent_score")) %>% 
          dplyr::mutate(
            team_score = as.integer(stringr::str_trim(.data$team_score)),
            opponent_score = as.integer(stringr::str_trim(.data$opponent_score))
          )
      )
      sched <- sched %>% 
        dplyr::mutate(
          home_team_id = ifelse(.data$home_team == .data$team_name, .data$team_id, .data$opponent_team_id),
          home_team_score = ifelse(.data$home_team == .data$team_name, .data$team_score, .data$opponent_score),
          home_team_conference = ifelse(.data$home_team == .data$team_name, .data$conference, .data$opponent_conference),
          home_team_conference_id = ifelse(.data$home_team == .data$team_name, .data$conference_id, .data$opponent_conference_id),
          home_team_slug = ifelse(.data$home_team == .data$team_name, .data$team_url, .data$opponent_team_slug),
          home_team_division = ifelse(.data$home_team == .data$team_name, .data$division, .data$opponent_division),
          
          away_team_id = ifelse(.data$home_team == .data$team_name, .data$opponent_team_id, .data$team_id),
          away_team_score = ifelse(.data$home_team == .data$team_name, .data$opponent_score, .data$team_score),
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
          "home_team_score",
          "home_team_conference",
          "home_team_conference_id",
          "home_team_slug",
          "home_team_division",
          "away_team",
          "away_team_id",
          "away_team_score",
          "away_team_conference",
          "away_team_conference_id",
          "away_team_slug",
          "away_team_division",
          "neutral_site",
          "innings",
          "slug",
          "game_info_url",
          "game_pbp_url",
          "contest_id",
          "game_pbp_id"
        )
      
      sched <- sched %>%
        make_baseballr_data("NCAA Baseball Schedule Info data from stats.ncaa.org", Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided (team_id: {team_id}, year = {year})"))
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
