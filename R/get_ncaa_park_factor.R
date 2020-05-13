#' Get Park Effects for NCAA Baseball Teams
#'
#' @param teamid The team's unique NCAA id.
#' @param years The season or seasons (i.e. use 2016 for the 2015-2016 season,
#' etc., limited to just 2013-2020 seasons)
#'
#'@importFrom stringr str_detect str_squish str_remove_all str_split_fixed
#'@importFrom janitor adorn_totals
#'
#' @return A dataframe with the following fields: school, home_games,
#' away_games, runs_scored_home (the total runs scored in home games by the selected team over the selected period of time),
#' runs_allowed_home (the total runs allowed in home games by the selected team over the selected period of time),
#' run_scored_away (the total runs scored in road games by the selected team over the selected period of time),
#' runs_allowed_away (the total runs allowed in road games by the selected team over the selected period of time),
#' base_pf (base park factor, comparing the run scoring environment of home games v. non-home games),
#' final_pf (park factor after adjustments, like adjusting how regression should be applied for how many years are selected,
#' ideology taken from this article: https://library.fangraphs.com/park-factors-5-year-regressed/)
#' @export
#'
#'
#' @examples \dontrun{get_ncaa_park_factor(736, c(2017:2019)}

get_ncaa_park_factor <- function(teamid, years) {

  school_name <- subset(master_ncaa_team_lu, school_id ==
                          teamid) %>%
    dplyr::select(school) %>%
    dplyr::distinct() %>% dplyr::pull(school)

  y = length(years)
  y_all = vector(mode = "list", length = length(years))

if (sum(years < 2013) > 0 || sum(years > 2020) > 0) {
  return("Please select only years between 2013 and 2020")
}
else if (y == 1) {
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

  dfa = dfa %>% dplyr::mutate(base_pf = ((runs_scored_home+runs_allowed_home)/(home_game))/((runs_scored_away+runs_allowed_away)/(away_game)),
                       final_pf = (1-(1-base_pf)*.6),
                       base_pf = round(base_pf,3),
                       final_pf = round(final_pf,3)) %>% dplyr::rename(school = score)
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

  dfa = dfa %>% dplyr::mutate(base_pf = ((runs_scored_home+runs_allowed_home)/(home_game))/((runs_scored_away+runs_allowed_away)/(away_game)),
                       final_pf = (1-(1-base_pf)*.7),
                       base_pf = round(base_pf,3),
                       final_pf = round(final_pf,3)) %>% dplyr::rename(school = score)

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

  dfa = dfa %>% dplyr::mutate(base_pf = ((runs_scored_home+runs_allowed_home)/(home_game))/((runs_scored_away+runs_allowed_away)/(away_game)),
                       final_pf = (1-(1-base_pf)*.8),
                       base_pf = round(base_pf,3),
                       final_pf = round(final_pf,3)) %>% dplyr::rename(school = score)

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

  dfa = dfa %>% dplyr::mutate(base_pf = ((runs_scored_home+runs_allowed_home)/(home_game))/((runs_scored_away+runs_allowed_away)/(away_game)),
                       final_pf = (1-(1-base_pf)*.9),
                       base_pf = round(base_pf,3),
                       final_pf = round(final_pf,3)) %>% dplyr::rename(school = score)

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

   dfa = dfa %>% dplyr::mutate(base_pf = (((runs_scored_home+runs_allowed_home)/(home_game))*(home_game/(home_game+away_game)))/(((runs_scored_away+runs_allowed_away)/(away_game))*(away_game/(home_game+away_game))),
                        final_pf = (1-(1-base_pf)*.95),
                        base_pf = round(base_pf,3),
                        final_pf = round(final_pf,3)) %>% dplyr::rename(school = score)

   return(dfa)
 }
}
