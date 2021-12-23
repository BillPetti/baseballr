#' @rdname ncaa_park_factor
#' @title Get Park Effects for NCAA Baseball Teams
#'
#' @param teamid The team's unique NCAA id.
#' @param years The season or seasons (i.e. use 2016 for the 2015-2016 season,
#' etc., limited to just 2013-2020 seasons).
#' @param type default is conference. the conference parameter adjusts for the conference
#' the school plays in, the division parameter calculates based on the division the school plays in 1,2,or 3.
#' Defaults to 'conference'.
#' @return A dataframe with the following fields: school, home_game,
#' away_game, runs_scored_home, runs_allowed_home, run_scored_away,
#' runs_allowed_away, base_pf (base park factor), home_game_adj (an adjustment for the percentage of home games played) final_pf (park factor after adjustments)
#' @importFrom stringr str_detect str_squish str_remove_all str_split_fixed
#' @importFrom janitor adorn_totals
#' @export
#' @examples \donttest{
#'   ncaa_park_factor(teamid = 736, years = c(2017:2019), type = "conference")
#' }

ncaa_park_factor <- function(teamid, years, type = "conference") {
  
  conference_pull <-  baseballr::master_ncaa_team_lu %>% 
    dplyr::filter(.data$school_id == teamid & .data$year == year) %>%
    dplyr::select(.data$school_id,.data$conference_id,.data$year,.data$division) %>%
    dplyr::distinct() %>% 
    dplyr::filter(.data$year %in% years)
  
  teams <- baseballr::master_ncaa_team_lu %>% 
    dplyr::filter(.data$conference_id %in% conference_pull$conference_id,
                  .data$year %in% years) %>% 
    dplyr::group_by(.data$conference_id,.data$year) %>% 
    dplyr::count(.data$conference) %>% 
    dplyr::ungroup()
  conference_pull <- conference_pull %>% 
    dplyr::right_join(teams,by=c("conference_id","year")) %>% 
    dplyr::pull(.data$n)
  
  
  
  school_name <- baseballr::master_ncaa_team_lu %>% 
    dplyr::filter(.data$school_id == teamid) %>%
    dplyr::select(.data$school) %>%
    dplyr::distinct() %>% 
    dplyr::pull(.data$school)
  
  y = length(years)
  y_all = vector(mode = "list", length = length(years))
  if (sum(years < 2013) > 0 || sum(years > 2020) > 0) {
    return("Please select only years between 2013 and 2020")
  }
  
  message('Acquiring game data and calculating park factors. Please be patient...')
  
  if (y == 1) {
    df = suppressWarnings(baseballr::get_ncaa_schedule_info(teamid = teamid, year = years))
    
    df = df %>%
      dplyr::mutate(
        home_game = ifelse(stringr::str_detect(.data$opponent,"\\@")==TRUE,0,1),
        away_game = ifelse(.data$home_game==1,0,1),
        score = stringr::str_squish(.data$score),
        score = stringr::str_remove_all(.data$score," "),
        runs_scored_home = as.numeric(ifelse(.data$home_game == 1, 
                                             stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 1],0)),
        runs_allowed_home = as.numeric(ifelse(.data$home_game == 1, 
                                              stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 2],0)),
        runs_scored_away = as.numeric(ifelse(.data$away_game == 1, 
                                             stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 1],0)),
        runs_allowed_away = as.numeric(ifelse(.data$away_game == 1, 
                                              stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 2],0)))
    
    dfa = janitor::adorn_totals(df %>% 
                                  dplyr::select(.data$score,.data$home_game,.data$away_game,.data$runs_scored_home,
                                                .data$runs_allowed_home,.data$runs_scored_away,
                                                .data$runs_allowed_away), where = "row", name = .data$school_name) %>% tail(1)
    
    if (type == "division") {
      dfa = dfa %>% 
        dplyr::mutate(
          base_pf = ((.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game))/((.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game)),
          home_game_adj = ifelse(.data$base_pf > 1, 
                                 .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), 
                                 .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.6),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>% 
        dplyr::rename(school = .data$score)
    } else {
      dfa = dfa %>% 
        dplyr::mutate(
          RPGH = (.data$runs_scored_home +.data$runs_allowed_home)/(.data$home_game),
          RPGR = (.data$runs_scored_away + .data$runs_allowed_away)/(.data$away_game),
          TM = round(mean(teams$n),0),
          base_pf = (.data$RPGH*.data$TM)/((.data$TM-1)*.data$RPGR+.data$RPGH),
          home_game_adj = ifelse(.data$base_pf > 1, .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.6),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>% 
        dplyr::select(-.data$RPGH,-.data$RPGR,-.data$TM) %>% 
        dplyr::rename(school = .data$score)
    }
    
    return(dfa)
    
  } else if (y == 2) {
    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::get_ncaa_schedule_info(teamid = teamid, year = years[[j]]))
    }
    
    df = do.call(rbind,y_all)
    
    df = df %>%
      dplyr::mutate(
        home_game = ifelse(stringr::str_detect(.data$opponent,"\\@")==TRUE,0,1),
        away_game = ifelse(.data$home_game==1,0,1),
        score = stringr::str_squish(.data$score),
        score = stringr::str_remove_all(.data$score," "),
        runs_scored_home = as.numeric(ifelse(.data$home_game == 1, 
                                             stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 1],0)),
        runs_allowed_home = as.numeric(ifelse(.data$home_game == 1, 
                                              stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 2],0)),
        runs_scored_away = as.numeric(ifelse(.data$away_game == 1, 
                                             stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 1],0)),
        runs_allowed_away = as.numeric(ifelse(.data$away_game == 1, 
                                              stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 2],0)))
    
    dfa = janitor::adorn_totals(df %>% 
                                  dplyr::select(.data$score,.data$home_game,
                                                .data$away_game,.data$runs_scored_home,
                                                .data$runs_allowed_home,.data$runs_scored_away,.data$runs_allowed_away), where = "row", name = school_name) %>% tail(1)
    if (type == "division") {
      dfa = dfa %>% 
        dplyr::mutate(
          base_pf = ((.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game))/((.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game)),
          home_game_adj = ifelse(.data$base_pf > 1, 
                                 .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), 
                                 .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
          final_pf = (1-(1-.data$home_game_adj)*.7),
          base_pf = round(.data$base_pf,3),
          home_game_adj = round(.data$home_game_adj,3),
          final_pf = round(.data$final_pf,3)) %>% 
        dplyr::rename(school = .data$score)
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
        dplyr::select(-.data$RPGH,-.data$RPGR,-.data$TM) %>% 
        dplyr::rename(school = .data$score)
    }
    return(dfa)
  } else if (y == 3) {
    
    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::get_ncaa_schedule_info(teamid = teamid, year = years[[j]]))
    }
    
    df = do.call(rbind,y_all)
    
    df = df %>%
      dplyr::mutate(
        home_game = ifelse(stringr::str_detect(.data$opponent,"\\@")==TRUE,0,1),
        away_game = ifelse(.data$home_game==1,0,1),
        score = stringr::str_squish(.data$score),
        score = stringr::str_remove_all(.data$score," "),
        runs_scored_home = as.numeric(ifelse(.data$home_game == 1, 
                                             stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 1],0)),
        runs_allowed_home = as.numeric(ifelse(.data$home_game == 1, 
                                              stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 2],0)),
        runs_scored_away = as.numeric(ifelse(.data$away_game == 1, 
                                             stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 1],0)),
        runs_allowed_away = as.numeric(ifelse(.data$away_game == 1, 
                                              stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 2],0)))
    
    dfa = janitor::adorn_totals(df %>% 
                                  dplyr::select(
                                    .data$score,.data$home_game,.data$away_game,.data$runs_scored_home,
                                    .data$runs_allowed_home,.data$runs_scored_away,.data$runs_allowed_away), where = "row", name = school_name) %>% 
      tail(1)
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
        dplyr::rename(school = .data$score)
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
        dplyr::select(-.data$RPGH,-.data$RPGR,-.data$TM) %>% 
        dplyr::rename(school = .data$score)
    }
    return(dfa)
  } else if(y == 4) {
    
    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::get_ncaa_schedule_info(teamid = teamid, year = years[[j]]))
    }
    
    df = do.call(rbind,y_all)
    
    df = df %>%
      dplyr::mutate(
        home_game = ifelse(stringr::str_detect(.data$opponent,"\\@")==TRUE,0,1),
        away_game = ifelse(.data$home_game==1,0,1),
        score = stringr::str_squish(.data$score),
        score = stringr::str_remove_all(.data$score," "),
        runs_scored_home = as.numeric(ifelse(.data$home_game == 1,
                                             stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 1],0)),
        runs_allowed_home = as.numeric(ifelse(.data$home_game == 1, 
                                              stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 2],0)),
        runs_scored_away = as.numeric(ifelse(.data$away_game == 1, 
                                             stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 1],0)),
        runs_allowed_away = as.numeric(ifelse(.data$away_game == 1, stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 2],0)))
    
    dfa = janitor::adorn_totals(df %>% 
                                  dplyr::select(.data$score,.data$home_game,.data$away_game,.data$runs_scored_home,
                                                .data$runs_allowed_home,.data$runs_scored_away,.data$runs_allowed_away), where = "row", name = school_name) %>% 
      tail(1)
    if (type == "division") {
      dfa = dfa %>% dplyr::mutate(
        base_pf = ((.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game))/((.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game)),
        home_game_adj = ifelse(.data$base_pf > 1, 
                               .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), 
                               .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
        final_pf = (1-(1-.data$home_game_adj)*.9),
        base_pf = round(.data$base_pf,3),
        home_game_adj = round(.data$home_game_adj,3),
        final_pf = round(.data$final_pf,3)) %>% 
        dplyr::rename(school = .data$score)
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
        dplyr::select(-.data$RPGH,-.data$RPGR,-.data$TM) %>% 
        dplyr::rename(school = .data$score)
    }
    
    return(dfa)
  } else if(y >= 5) {
    
    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::get_ncaa_schedule_info(teamid = teamid, year = years[[j]]))
    }
    
    df = do.call(rbind,y_all)
    
    df = df %>%
      dplyr::mutate(
        home_game = ifelse(stringr::str_detect(.data$opponent,"\\@")==TRUE,0,1),
        away_game = ifelse(.data$home_game==1,0,1),
        score = stringr::str_squish(.data$score),
        score = stringr::str_remove_all(.data$score," "),
        runs_scored_home = as.numeric(ifelse(.data$home_game == 1, 
                                             stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 1],0)),
        runs_allowed_home = as.numeric(ifelse(.data$home_game == 1, 
                                              stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 2],0)),
        runs_scored_away = as.numeric(ifelse(.data$away_game == 1, 
                                             stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 1],0)),
        runs_allowed_away = as.numeric(ifelse(.data$away_game == 1, 
                                              stringr::str_split_fixed(.data$score, stringr::fixed("-"), 2)[, 2],0)))
    
    dfa = janitor::adorn_totals(df %>% 
                                  dplyr::select(.data$score,.data$home_game,.data$away_game,.data$runs_scored_home,
                                                .data$runs_allowed_home,.data$runs_scored_away,.data$runs_allowed_away), where = "row", name = school_name) %>% tail(1)
    if (type == "division") {
      
      dfa = dfa %>% dplyr::mutate(
        base_pf = (((.data$runs_scored_home+.data$runs_allowed_home)/(.data$home_game))*(.data$home_game/(.data$home_game+.data$away_game)))/(((.data$runs_scored_away+.data$runs_allowed_away)/(.data$away_game))*(.data$away_game/(.data$home_game+.data$away_game))),
        home_game_adj = ifelse(.data$base_pf > 1, .data$base_pf-(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game))), .data$base_pf+(abs(.data$base_pf-1)*(.data$home_game/(.data$home_game+.data$away_game)))),
        final_pf = (1-(1-.data$home_game_adj)*.95),
        base_pf = round(.data$base_pf,3),
        home_game_adj = round(.data$home_game_adj,3),
        final_pf = round(.data$final_pf,3)) %>% 
        dplyr::rename(school = .data$score)
    } else {
      dfa = dfa %>% mutate(
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
        dplyr::select(-.data$RPGH,-.data$RPGR,-.data$TM) %>% 
        dplyr::rename(school = .data$score)
    }
    return(dfa)
  }
}

#' @rdname ncaa_park_factor
#' @title Get Park Effects for NCAA Baseball Teams
#' @param teamid The team's unique NCAA id.
#' @param years The season or seasons (i.e. use 2016 for the 2015-2016 season,
#' etc., limited to just 2013-2020 seasons).
#' @param type default is conference. the conference parameter adjusts for the conference
#' the school plays in, the division parameter calculates based on the division the school plays in 1,2,or 3.
#' Defaults to 'conference'.
#' @return A dataframe with the following fields: school, home_game,
#' away_game, runs_scored_home, runs_allowed_home, run_scored_away,
#' runs_allowed_away, base_pf (base park factor), home_game_adj (an adjustment for the percentage of home games played) final_pf (park factor after adjustments)
#' @export
get_ncaa_park_factor <- function(teamid, years, type = "conference") {
  ncaa_park_factor(teamid = teamid, years = years, type = "conference")
}