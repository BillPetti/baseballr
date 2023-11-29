#' @rdname ncaa_park_factor
#' @title **Get Park Effects for NCAA Baseball Teams**
#'
#' @param team_id The team's unique NCAA id.
#' @param years The season or seasons (i.e. use 2016 for the 2015-2016 season,
#' etc., limited to just 2013-2023 seasons).
#' @param type default is conference. the conference parameter adjusts for the conference
#' the school plays in, the division parameter calculates based on the division the school plays in 1,2,or 3.
#' Defaults to 'conference'.
#' @param ... Additional arguments passed to an underlying function like httr.
#' @return A data frame with the following fields: school, home_game,
#' away_game, runs_scored_home, runs_allowed_home, run_scored_away,
#' runs_allowed_away, base_pf (base park factor), home_game_adj (an adjustment for the percentage of home games played) final_pf (park factor after adjustments)
#' 
#'  |col_name          |types     |
#'  |:-----------------|:---------|
#'  |school            |character |
#'  |home_game         |numeric   |
#'  |away_game         |numeric   |
#'  |runs_scored_home  |numeric   |
#'  |runs_allowed_home |numeric   |
#'  |runs_scored_away  |numeric   |
#'  |runs_allowed_away |numeric   |
#'  |base_pf           |numeric   |
#'  |home_game_adj     |numeric   |
#'  |final_pf          |numeric   |
#'
#' @importFrom janitor adorn_totals
#' @export
#' @details 
#' ```r
#'   try(ncaa_park_factor(team_id = 736, years = c(2018:2019), type = "conference"))
#' ```

ncaa_park_factor <- function(team_id, years, type = "conference", ...) {
  if (is.null(team_id)) {
    cli::cli_abort("Enter valid team_id")
  }
  if (is.null(years)) {
    cli::cli_abort("Enter valid years as a number (YYYY) or vector")
  }
  ncaa_team_lookup <- load_ncaa_baseball_teams() %>% 
    dplyr::mutate(
      team_id = as.integer(.data$team_id),
      conference_id = as.integer(.data$conference_id),
      year = as.integer(.data$year),
      division = as.integer(.data$division)
    )
  conference_pull <-  ncaa_team_lookup %>% 
    dplyr::filter(.data$team_id == {{team_id}}) %>%
    dplyr::select(
      "team_id",
      "conference_id",
      "year",
      "division") %>%
    dplyr::distinct() %>% 
    dplyr::filter(.data$year %in% years)
  
  teams <- ncaa_team_lookup %>% 
    dplyr::filter(.data$conference_id %in% conference_pull$conference_id,
                  .data$year %in% years) %>% 
    dplyr::group_by(.data$conference_id, .data$year) %>% 
    dplyr::count(.data$conference) %>% 
    dplyr::ungroup()
  conference_pull <- conference_pull %>% 
    dplyr::right_join(teams, by = c("conference_id","year")) %>% 
    dplyr::pull("n")
  
  
  
  school_name <- ncaa_team_lookup %>% 
    dplyr::filter(.data$team_id == {{team_id}}) %>%
    dplyr::slice(1) %>%
    dplyr::pull("team_name")
  
  y = length(years)
  y_all = vector(mode = "list", length = length(years))
  if (sum(years < 2013) > 0 || sum(years > 2023) > 0) {
    return("Please select only years between 2013 and 2023")
  }
  
  
  if (y == 1) {
    df = suppressWarnings(baseballr::ncaa_schedule_info(team_id = {{team_id}}, year = years, ...))
    
    df = df %>%
      dplyr::mutate(
        home_game = as.numeric(ifelse(.data$home_team_id == {{team_id}} & is.na(.data$neutral_site), 1, 0)),
        away_game = as.numeric(ifelse(.data$home_game == 1, 0, 1)),
        runs_scored_home = as.numeric(ifelse(.data$home_game == 1, .data$home_team_score,.data$away_team_score)),
        runs_allowed_home = as.numeric(ifelse(.data$home_game == 1, .data$away_team_score,.data$home_team_score)),
        runs_scored_away = as.numeric(ifelse(.data$home_game == 1, .data$away_team_score,.data$home_team_score)),
        runs_allowed_away = as.numeric(ifelse(.data$home_game == 1, .data$home_team_score,.data$away_team_score)),
        school = school_name) %>%
      dplyr::select("school","home_game","away_game","runs_scored_home","runs_allowed_home",
                    "runs_scored_away","runs_allowed_away")
    
    dfa = janitor::adorn_totals(df, where = "row", name = "school") 
    dfa <- dfa %>% dplyr::slice(nrow(dfa))
    
    if (type == "division") {
      dfa = dfa %>%
        dplyr::mutate(
          base_pf = ((.data$runs_scored_home + .data$runs_allowed_home)/(.data$home_game))/((.data$runs_scored_away + .data$runs_allowed_away)/(.data$away_game)),
          home_game_adj = ifelse(.data$base_pf > 1,
                                 .data$base_pf - (abs(.data$base_pf - 1)*(.data$home_game/(.data$home_game + .data$away_game))),
                                 .data$base_pf + (abs(.data$base_pf - 1)*(.data$home_game/(.data$home_game + .data$away_game)))),
          final_pf = (1 - (1 - .data$home_game_adj)*.6),
          base_pf = round(.data$base_pf, 3),
          home_game_adj = round(.data$home_game_adj, 3),
          final_pf = round(.data$final_pf, 3)) %>%
        # dplyr::rename("school" = "score") %>%
          dplyr::mutate(school = school_name)
    } else {
      dfa = dfa %>%
        dplyr::mutate(
          RPGH = (.data$runs_scored_home + .data$runs_allowed_home)/(.data$home_game),
          RPGR = (.data$runs_scored_away + .data$runs_allowed_away)/(.data$away_game),
          TM = round(mean(teams$n),0),
          base_pf = (.data$RPGH*.data$TM)/((.data$TM-1)*.data$RPGR+.data$RPGH),
          home_game_adj = ifelse(.data$base_pf > 1, .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.6),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>%
        dplyr::select(-c("RPGH","RPGR","TM")) %>%
        # dplyr::rename("school" = "score") %>%
        dplyr::mutate(school = school_name)
    }
    
    return(dfa)
    
  } else if (y == 2) {
    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::ncaa_schedule_info(team_id = {{team_id}}, year = years[[j]], ...))
    }
    
    df = do.call(rbind,y_all)
    
    df = df %>%
      dplyr::mutate(
        home_game = as.numeric(ifelse(.data$home_team_id == {{team_id}} & is.na(.data$neutral_site), 1, 0)),
        away_game = as.numeric(ifelse(.data$home_game == 1, 0, 1)),
        runs_scored_home = as.numeric(ifelse(.data$home_game == 1, .data$home_team_score,.data$away_team_score)),
        runs_allowed_home = as.numeric(ifelse(.data$home_game == 1, .data$away_team_score,.data$home_team_score)),
        runs_scored_away = as.numeric(ifelse(.data$home_game == 1, .data$away_team_score,.data$home_team_score)),
        runs_allowed_away = as.numeric(ifelse(.data$home_game == 1, .data$home_team_score,.data$away_team_score)),
        school = school_name) %>%
      dplyr::select("school","home_game","away_game","runs_scored_home","runs_allowed_home",
                    "runs_scored_away","runs_allowed_away")
    
    dfa = janitor::adorn_totals(df, where = "row", name = "school") %>% tail(1)
    
    
    if (type == "division") {
      dfa = dfa %>%
        dplyr::mutate(
          base_pf = ((.data$runs_scored_home+ dfa$runs_allowed_home)/(.data$home_game))/((.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game)),
          home_game_adj = ifelse(.data$base_pf > 1,
                                 .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))),
                                 .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.7),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>%
        dplyr::mutate(school = school_name)
    } else {
      dfa = dfa %>%
        dplyr::mutate(
          RPGH = (.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game),
          RPGR = (.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game),
          TM = round(mean(teams$n),0),
          base_pf = (.data$RPGH*.data$TM)/((.data$TM-1)*.data$RPGR+.data$RPGH),
          home_game_adj = ifelse(.data$base_pf > 1,
                                 .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))),
                                 .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.7),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>%
        dplyr::select(-c("RPGH","RPGR","TM")) %>%
        dplyr::mutate(school = school_name)
    }
    return(dfa)
  } else if (y == 3) {
    
    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::ncaa_schedule_info(team_id = {{team_id}}, year = years[[j]], ...))
    }
    
    df = do.call(rbind,y_all)
    
    df = df %>%
      dplyr::mutate(
        home_game = as.numeric(ifelse(.data$home_team_id == {{team_id}} & is.na(.data$neutral_site), 1, 0)),
        away_game = as.numeric(ifelse(.data$home_game == 1, 0, 1)),
        runs_scored_home = as.numeric(ifelse(.data$home_game == 1, .data$home_team_score,.data$away_team_score)),
        runs_allowed_home = as.numeric(ifelse(.data$home_game == 1, .data$away_team_score,.data$home_team_score)),
        runs_scored_away = as.numeric(ifelse(.data$home_game == 1, .data$away_team_score,.data$home_team_score)),
        runs_allowed_away = as.numeric(ifelse(.data$home_game == 1, .data$home_team_score,.data$away_team_score)),
        school = school_name) %>%
      dplyr::select("school","home_game","away_game","runs_scored_home","runs_allowed_home",
                    "runs_scored_away","runs_allowed_away")
    
    dfa = janitor::adorn_totals(df, where = "row", name = "school") %>% tail(1)
    
    if (type == "division") {
      dfa = dfa %>% 
        dplyr::mutate(
          base_pf = ((.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game))/((.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game)),
          home_game_adj = ifelse(.data$base_pf > 1, 
                                 .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), 
                                 .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.8),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>% 
        dplyr::mutate(school = school_name)
    } else {
      dfa = dfa %>%  
        dplyr::mutate(
          RPGH = (.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game),
          RPGR = (.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game),
          TM = round(mean(teams$n),0),
          base_pf = (.data$RPGH*.data$TM)/((.data$TM-1)*.data$RPGR+.data$RPGH),
          home_game_adj = ifelse(.data$base_pf > 1, 
                                 .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.8),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>% 
        dplyr::select(-c("RPGH", "RPGR", "TM")) %>% 
        dplyr::mutate(school = school_name)
    }
    return(dfa)
  } else if(y == 4) {
    
    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::ncaa_schedule_info(team_id = {{team_id}}, year = years[[j]], ...))
    }
    
    df = do.call(rbind, y_all)
    
    df = df %>%
      dplyr::mutate(
        home_game = as.numeric(ifelse(.data$home_team_id == {{team_id}} & is.na(.data$neutral_site), 1, 0)),
        away_game = as.numeric(ifelse(.data$home_game == 1, 0, 1)),
        runs_scored_home = as.numeric(ifelse(.data$home_game == 1, .data$home_team_score,.data$away_team_score)),
        runs_allowed_home = as.numeric(ifelse(.data$home_game == 1, .data$away_team_score,.data$home_team_score)),
        runs_scored_away = as.numeric(ifelse(.data$home_game == 1, .data$away_team_score,.data$home_team_score)),
        runs_allowed_away = as.numeric(ifelse(.data$home_game == 1, .data$home_team_score,.data$away_team_score)),
        school = school_name) %>%
      dplyr::select("school","home_game","away_game","runs_scored_home","runs_allowed_home",
                    "runs_scored_away","runs_allowed_away")
    
    dfa = janitor::adorn_totals(df, where = "row", name = "school") %>% tail(1)
    
    if (type == "division") {
      dfa = dfa %>% 
        dplyr::mutate(
          base_pf = ((.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game))/((.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game)),
          home_game_adj = ifelse(.data$base_pf > 1, 
                                 .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), 
                                 .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.9),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>% 
        dplyr::mutate(school = school_name)
    } else {
      dfa = dfa %>% 
        dplyr::mutate(
          RPGH = (.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game),
          RPGR = (.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game),
          TM = round(mean(teams$n),0),
          base_pf = (.data$RPGH*.data$TM)/((.data$TM-1)*.data$RPGR+.data$RPGH),
          home_game_adj = ifelse(.data$base_pf > 1, 
                                 .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), 
                                 .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.9),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>% 
        dplyr::select(-c("RPGH", "RPGR", "TM")) %>% 
        dplyr::mutate(school = school_name)
    }
    
    return(dfa)
  } else if (y >= 5) {
    
    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::ncaa_schedule_info(team_id = {{team_id}}, year = years[[j]], ...))
    }
    
    df = do.call(rbind,y_all)
    
    df = df %>%
      dplyr::mutate(
        home_game = as.numeric(ifelse(.data$home_team_id == {{team_id}} & is.na(.data$neutral_site), 1, 0)),
        away_game = as.numeric(ifelse(.data$home_game == 1, 0, 1)),
        runs_scored_home = as.numeric(ifelse(.data$home_game == 1, .data$home_team_score,.data$away_team_score)),
        runs_allowed_home = as.numeric(ifelse(.data$home_game == 1, .data$away_team_score,.data$home_team_score)),
        runs_scored_away = as.numeric(ifelse(.data$home_game == 1, .data$away_team_score,.data$home_team_score)),
        runs_allowed_away = as.numeric(ifelse(.data$home_game == 1, .data$home_team_score,.data$away_team_score)),
        school = school_name) %>%
      dplyr::select("school","home_game","away_game","runs_scored_home","runs_allowed_home",
                    "runs_scored_away","runs_allowed_away")
    
    dfa = janitor::adorn_totals(df, where = "row", name = "school") %>% tail(1)
    
    if (type == "division") {
      
      dfa = dfa %>% 
        dplyr::mutate(
          base_pf = (((.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game))*(.data$home_game/(.data$home_game+.data$away_game)))/(((.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game))*(.data$away_game/(.data$home_game+.data$away_game))),
          home_game_adj = ifelse(.data$base_pf > 1, .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.95),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>% 
        dplyr::mutate(school = school_name)
    } else {
      dfa = dfa %>% 
        dplyr::mutate(
          RPGH = (.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game),
          RPGR = (.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game),
          TM = round(mean(teams$n),0),
          base_pf = (.data$RPGH*.data$TM)/((.data$TM-1)*.data$RPGR+.data$RPGH),
          home_game_adj = ifelse(.data$base_pf > 1, 
                                 .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), 
                                 .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.95),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>% 
        dplyr::select(-c("RPGH", "RPGR", "TM")) %>% 
        dplyr::mutate(school = school_name)
    }
    return(dfa)
  }
}

#' @rdname get_ncaa_park_factor
#' @title **(legacy) Get Park Effects for NCAA Baseball Teams**
#' @inheritParams ncaa_park_factor
#' @return A data frame with the following fields: school, home_game,
#' away_game, runs_scored_home, runs_allowed_home, run_scored_away,
#' runs_allowed_away, base_pf (base park factor), home_game_adj (an adjustment for the percentage of home games played) final_pf (park factor after adjustments)
#' @keywords legacy
#' @export
get_ncaa_park_factor <- ncaa_park_factor
