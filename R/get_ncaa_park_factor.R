
#' Get Park Effects for NCAA Baseball Teams
#'
#' @param teamid The team's unique NCAA id.
#' @param years The season or seasons (i.e. use 2016 for the 2015-2016 season,
#' etc., limited to just 2013-2020 seasons).
#' @param type default is conference. the conference parameter adjusts for the conference
#' the school plays in, the division parameter calculates based on the division the school plays in 1,2,or 3.
#' Defaults to 'conference'.
#'
#'@importFrom stringr str_detect str_squish str_remove_all str_split_fixed
#'@importFrom janitor adorn_totals
#'
#' @return A dataframe with the following fields: school, home_game,
#' away_game, runs_scored_home, runs_allowed_home, run_scored_away,
#' runs_allowed_away, base_pf (base park factor), home_game_adj (an adjustment for the percentage of home games played) final_pf (park factor after adjustments)
#' @export
#'
#'
#'@examples \dontrun{get_ncaa_park_factor(736, c(2017:2019),type = "conference")}
#'
#'
#'

get_ncaa_park_factor <- function(teamid, years, type = "conference") {

  conference_pull <- subset(master_ncaa_team_lu, school_id ==
                              teamid) %>%
    dplyr::select(school_id,conference_id,year,division) %>%
    dplyr::distinct() %>% dplyr::filter(year %in% years)

  teams <- dplyr::filter(master_ncaa_team_lu, conference_id %in% conference_pull$conference_id,
                         year %in% years) %>% group_by(conference_id,year) %>% count(conference) %>% right_join(conference_pull,conferences,by=c("conference_id","year")) %>% pull(n)



  school_name <- subset(master_ncaa_team_lu, school_id ==
                          teamid) %>%
    dplyr::select(school) %>%
    dplyr::distinct() %>% dplyr::pull(school)

  y = length(years)
  y_all = vector(mode = "list", length = length(years))
  if (sum(years < 2013) > 0 || sum(years > 2020) > 0) {
    return("Please select only years between 2013 and 2020")
  }

  message('Acquiring game data and calculating park factors. Please be patient...')

  if (y == 1) {
    df = suppressWarnings(baseballr::get_ncaa_schedule_info(teamid = teamid, year = years))

    df = df %>%
      dplyr::mutate(home_game = ifelse(stringr::str_detect(opponent,"\\@")==TRUE,0,1),
                    away_game = ifelse(home_game==1,0,1),
                    score = stringr::str_squish(score),
                    score = stringr::str_remove_all(score," "),
                    runs_scored_home = as.numeric(ifelse(home_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 1],0)),
                    runs_allowed_home = as.numeric(ifelse(home_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 2],0)),
                    runs_scored_away = as.numeric(ifelse(away_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 1],0)),
                    runs_allowed_away = as.numeric(ifelse(away_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 2],0)))

    dfa = janitor::adorn_totals(df %>% dplyr::select(score,home_game,away_game,runs_scored_home,
                                                     runs_allowed_home,runs_scored_away,runs_allowed_away), where = "row", name = school_name) %>% tail(1)

    if (type == "division") {
      dfa = dfa %>% dplyr::mutate(base_pf = ((runs_scored_home+runs_allowed_home)/(home_game))/((runs_scored_away+runs_allowed_away)/(away_game)),
                                  home_game_adj = ifelse(base_pf > 1, base_pf-(abs(base_pf-1)*(home_game/(home_game+away_game))), base_pf+(abs(base_pf-1)*(home_game/(home_game+away_game)))),
                                  final_pf = (1-(1-home_game_adj)*.6),
                                  base_pf = round(base_pf,3),
                                  home_game_adj = round(home_game_adj,3),
                                  final_pf = round(final_pf,3)) %>% dplyr::rename(school = score)
    } else {
      dfa = dfa %>% mutate(RPGH = (runs_scored_home+runs_allowed_home)/(home_game),
                           RPGR = (runs_scored_away+runs_allowed_away)/(away_game),
                           TM = round(mean(teams),0),
                           base_pf = (RPGH*TM)/((TM-1)*RPGR+RPGH),
                           home_game_adj = ifelse(base_pf > 1, base_pf-(abs(base_pf-1)*(home_game/(home_game+away_game))), base_pf+(abs(base_pf-1)*(home_game/(home_game+away_game)))),
                           final_pf = (1-(1-home_game_adj)*.6),
                           base_pf = round(base_pf,3),
                           home_game_adj = round(home_game_adj,3),
                           final_pf = round(final_pf,3)) %>% dplyr::select(-RPGH,-RPGR,-TM) %>% dplyr::rename(school = score)
    }

    return(dfa)

  } else if (y == 2) {
    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::get_ncaa_schedule_info(teamid = teamid, year = years[[j]]))
    }

    df = do.call(rbind,y_all)

    df = df %>%
      dplyr::mutate(home_game = ifelse(stringr::str_detect(opponent,"\\@")==TRUE,0,1),
                    away_game = ifelse(home_game==1,0,1),
                    score = stringr::str_squish(score),
                    score = stringr::str_remove_all(score," "),
                    runs_scored_home = as.numeric(ifelse(home_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 1],0)),
                    runs_allowed_home = as.numeric(ifelse(home_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 2],0)),
                    runs_scored_away = as.numeric(ifelse(away_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 1],0)),
                    runs_allowed_away = as.numeric(ifelse(away_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 2],0)))

    dfa = janitor::adorn_totals(df %>% dplyr::select(score,home_game,away_game,runs_scored_home,
                                                     runs_allowed_home,runs_scored_away,runs_allowed_away), where = "row", name = school_name) %>% tail(1)
    if (type == "division") {
      dfa = dfa %>% dplyr::mutate(base_pf = ((runs_scored_home+runs_allowed_home)/(home_game))/((runs_scored_away+runs_allowed_away)/(away_game)),
                                  home_game_adj = ifelse(base_pf > 1, base_pf-(abs(base_pf-1)*(home_game/(home_game+away_game))), base_pf+(abs(base_pf-1)*(home_game/(home_game+away_game)))),
                                  final_pf = (1-(1-home_game_adj)*.7),
                                  base_pf = round(base_pf,3),
                                  home_game_adj = round(home_game_adj,3),
                                  final_pf = round(final_pf,3)) %>% dplyr::rename(school = score)
    } else {
      dfa = dfa %>% mutate(RPGH = (runs_scored_home+runs_allowed_home)/(home_game),
                           RPGR = (runs_scored_away+runs_allowed_away)/(away_game),
                           TM = round(mean(teams),0),
                           base_pf = (RPGH*TM)/((TM-1)*RPGR+RPGH),
                           home_game_adj = ifelse(base_pf > 1, base_pf-(abs(base_pf-1)*(home_game/(home_game+away_game))), base_pf+(abs(base_pf-1)*(home_game/(home_game+away_game)))),
                           final_pf = (1-(1-home_game_adj)*.7),
                           base_pf = round(base_pf,3),
                           home_game_adj = round(home_game_adj,3),
                           final_pf = round(final_pf,3)) %>% select(-RPGH,-RPGR,-TM) %>% dplyr::rename(school = score)
    }
    return(dfa)
  } else if (y == 3) {

    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::get_ncaa_schedule_info(teamid = teamid, year = years[[j]]))
    }

    df = do.call(rbind,y_all)

    df = df %>%
      dplyr::mutate(home_game = ifelse(stringr::str_detect(opponent,"\\@")==TRUE,0,1),
                    away_game = ifelse(home_game==1,0,1),
                    score = stringr::str_squish(score),
                    score = stringr::str_remove_all(score," "),
                    runs_scored_home = as.numeric(ifelse(home_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 1],0)),
                    runs_allowed_home = as.numeric(ifelse(home_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 2],0)),
                    runs_scored_away = as.numeric(ifelse(away_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 1],0)),
                    runs_allowed_away = as.numeric(ifelse(away_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 2],0)))

    dfa = janitor::adorn_totals(df %>% dplyr::select(score,home_game,away_game,runs_scored_home,
                                                     runs_allowed_home,runs_scored_away,runs_allowed_away), where = "row", name = school_name) %>% tail(1)
    if (type == "division") {
      dfa = dfa %>% dplyr::mutate(base_pf = ((runs_scored_home+runs_allowed_home)/(home_game))/((runs_scored_away+runs_allowed_away)/(away_game)),
                                  home_game_adj = ifelse(base_pf > 1, base_pf-(abs(base_pf-1)*(home_game/(home_game+away_game))), base_pf+(abs(base_pf-1)*(home_game/(home_game+away_game)))),
                                  final_pf = (1-(1-home_game_adj)*.8),
                                  base_pf = round(base_pf,3),
                                  home_game_adj = round(home_game_adj,3),
                                  final_pf = round(final_pf,3)) %>% dplyr::rename(school = score)
    } else {
      dfa = dfa %>% mutate(RPGH = (runs_scored_home+runs_allowed_home)/(home_game),
                           RPGR = (runs_scored_away+runs_allowed_away)/(away_game),
                           TM = round(mean(teams),0),
                           base_pf = (RPGH*TM)/((TM-1)*RPGR+RPGH),
                           home_game_adj = ifelse(base_pf > 1, base_pf-(abs(base_pf-1)*(home_game/(home_game+away_game))), base_pf+(abs(base_pf-1)*(home_game/(home_game+away_game)))),
                           final_pf = (1-(1-home_game_adj)*.8),
                           base_pf = round(base_pf,3),
                           home_game_adj = round(home_game_adj,3),
                           final_pf = round(final_pf,3)) %>% select(-RPGH,-RPGR,-TM) %>% dplyr::rename(school = score)
    }
    return(dfa)
  } else if(y == 4) {

    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::get_ncaa_schedule_info(teamid = teamid, year = years[[j]]))
    }

    df = do.call(rbind,y_all)

    df = df %>%
      dplyr::mutate(home_game = ifelse(stringr::str_detect(opponent,"\\@")==TRUE,0,1),
                    away_game = ifelse(home_game==1,0,1),
                    score = stringr::str_squish(score),
                    score = stringr::str_remove_all(score," "),
                    runs_scored_home = as.numeric(ifelse(home_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 1],0)),
                    runs_allowed_home = as.numeric(ifelse(home_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 2],0)),
                    runs_scored_away = as.numeric(ifelse(away_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 1],0)),
                    runs_allowed_away = as.numeric(ifelse(away_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 2],0)))

    dfa = janitor::adorn_totals(df %>% dplyr::select(score,home_game,away_game,runs_scored_home,
                                                     runs_allowed_home,runs_scored_away,runs_allowed_away), where = "row", name = school_name) %>% tail(1)
    if (type == "division") {
      dfa = dfa %>% dplyr::mutate(base_pf = ((runs_scored_home+runs_allowed_home)/(home_game))/((runs_scored_away+runs_allowed_away)/(away_game)),
                                  home_game_adj = ifelse(base_pf > 1, base_pf-(abs(base_pf-1)*(home_game/(home_game+away_game))), base_pf+(abs(base_pf-1)*(home_game/(home_game+away_game)))),
                                  final_pf = (1-(1-home_game_adj)*.9),
                                  base_pf = round(base_pf,3),
                                  home_game_adj = round(home_game_adj,3),
                                  final_pf = round(final_pf,3)) %>% dplyr::rename(school = score)
    } else {
      dfa = dfa %>% mutate(RPGH = (runs_scored_home+runs_allowed_home)/(home_game),
                           RPGR = (runs_scored_away+runs_allowed_away)/(away_game),
                           TM = round(mean(teams),0),
                           base_pf = (RPGH*TM)/((TM-1)*RPGR+RPGH),
                           home_game_adj = ifelse(base_pf > 1, base_pf-(abs(base_pf-1)*(home_game/(home_game+away_game))), base_pf+(abs(base_pf-1)*(home_game/(home_game+away_game)))),
                           final_pf = (1-(1-home_game_adj)*.9),
                           base_pf = round(base_pf,3),
                           home_game_adj = round(home_game_adj,3),
                           final_pf = round(final_pf,3)) %>% select(-RPGH,-RPGR,-TM) %>% dplyr::rename(school = score)
    }

    return(dfa)
  } else if(y >= 5) {

    for (j in 1:length(years)) {
      y_all[[j]] = suppressWarnings(baseballr::get_ncaa_schedule_info(teamid = teamid, year = years[[j]]))
    }

    df = do.call(rbind,y_all)

    df = df %>%
      dplyr::mutate(home_game = ifelse(stringr::str_detect(opponent,"\\@")==TRUE,0,1),
                    away_game = ifelse(home_game==1,0,1),
                    score = stringr::str_squish(score),
                    score = stringr::str_remove_all(score," "),
                    runs_scored_home = as.numeric(ifelse(home_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 1],0)),
                    runs_allowed_home = as.numeric(ifelse(home_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 2],0)),
                    runs_scored_away = as.numeric(ifelse(away_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 1],0)),
                    runs_allowed_away = as.numeric(ifelse(away_game == 1, stringr::str_split_fixed(score, stringr::fixed("-"), 2)[, 2],0)))

    dfa = janitor::adorn_totals(df %>% dplyr::select(score,home_game,away_game,runs_scored_home,
                                                     runs_allowed_home,runs_scored_away,runs_allowed_away), where = "row", name = school_name) %>% tail(1)
    if (type == "division") {

      dfa = dfa %>% dplyr::mutate(base_pf = (((runs_scored_home+runs_allowed_home)/(home_game))*(home_game/(home_game+away_game)))/(((runs_scored_away+runs_allowed_away)/(away_game))*(away_game/(home_game+away_game))),
                                  home_game_adj = ifelse(base_pf > 1, base_pf-(abs(base_pf-1)*(home_game/(home_game+away_game))), base_pf+(abs(base_pf-1)*(home_game/(home_game+away_game)))),
                                  final_pf = (1-(1-home_game_adj)*.95),
                                  base_pf = round(base_pf,3),
                                  home_game_adj = round(home_game_adj,3),
                                  final_pf = round(final_pf,3)) %>% dplyr::rename(school = score)
    } else {
      dfa = dfa %>% mutate(RPGH = (runs_scored_home+runs_allowed_home)/(home_game),
                           RPGR = (runs_scored_away+runs_allowed_away)/(away_game),
                           TM = round(mean(teams),0),
                           base_pf = (RPGH*TM)/((TM-1)*RPGR+RPGH),
                           home_game_adj = ifelse(base_pf > 1, base_pf-(abs(base_pf-1)*(home_game/(home_game+away_game))), base_pf+(abs(base_pf-1)*(home_game/(home_game+away_game)))),
                           final_pf = (1-(1-home_game_adj)*.95),
                           base_pf = round(base_pf,3),
                           home_game_adj = round(home_game_adj,3),
                           final_pf = round(final_pf,3)) %>% select(-RPGH,-RPGR,-TM) %>% dplyr::rename(school = score)
    }
    return(dfa)
  }
}
