#' Scrape NCAA baseball schedule for all teams (or just one conference) (Division I, II, and III)
#'
#' This function allows the user to obtain multiple team's schedule.
#'
#' @param year The season for which data should be returned, in the form of "YYYY". Years currently available: 2013-2017.
#' @param division Select what division of college baseball 
#' @param conf Select a conference, naming convention can be found in master_ncaa_team_lu
#' @keywords baseball, NCAA, college
#' @import dplyr
#' @import assertthat
#' @import purrr
#' @export ncaa_get_season_schedule
#' @examples
#' \dontrun{
#' ncaa_get_season_schedule(2017)
#' ncaa_get_season_schedule(2017,conf="ACC")
#' }
#' 
#' 

ncaa_get_season_schedule = function(yr, div=1,conf=NULL){
  
  assert_that(yr>=2013,msg='you must provide a year that is equal to or greater than 2013')
  if(!is.null(conf)){
    assert_that(conf %in% master_ncaa_team_lu$conference,
                msg='Conference name provided is not clear, please check master_ncaa_team_lu DF')
    teams_df <- master_ncaa_team_lu %>% filter(year==yr,division==div,conference==conf) %>% 
      select(school,conference,school_id,year) %>% 
      mutate(
        schedule = purrr::map2(school_id,year,ncaa_get_team_schedule,division=div,verbose=T)
      ) %>% drop_na()
    teams_clean <- teams_df %>% unnest() %>% select(-school,-year)
  }else{
    teams_df <- master_ncaa_team_lu %>% filter(year==yr & division==div) %>% 
      select(school,conference,school_id,year) %>% 
      mutate(
        schedule = purrr::map2(school_id,year,ncaa_get_team_schedule,division=div,verbose=T)
      ) %>% drop_na()
    teams_clean <- teams_df %>% unnest() %>% select(-school,-year)
  }
  
  return(teams_clean)  
}


