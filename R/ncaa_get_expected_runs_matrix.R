#' Calculate expected runs matrix for NCAA PBP
#'
#'
#' NCAA PBP can be obtained from `ncaa_get_pbp`
#'
#' @param season True or False, NCAA PBP by Season or Individual Teams.
#' @keywords baseball, NCAA, college
#' @import dplyr
#' @export ncaa_get_expected_runs_matrix
#' @examples 
#' \dontrun{
#' tamu_pbp_df <- ncaa_get_pbp(season=F,year=2017,division=1,teamid=697)
#' ncaa_get_expected_runs_matrix(tamu_pbp_df)
#' }

ncaa_get_expected_runs_matrix <- function(pbp_data_frame){
  base_cd = pbp_data_frame %>% pull(base_cd_before)
  outs = pbp_data_frame %>% pull(outs_before)
  runs_rest_of_inn = pbp_data_frame %>% pull(runs_roi)
  
  
  ER=data_frame(base_cd,outs,runs_rest_of_inn)%>%
    group_by(base_cd,outs)%>%
    summarize(ERV=round(mean(runs_rest_of_inn),3))%>%
    ungroup()%>%
    mutate(state=paste(base_cd, outs, sep=' '))%>%
    arrange(outs)
  
  ER2=matrix(ER$ERV, ncol=3)
  rownames(ER2)=c('_ _ _','X _ _','_ X _ ','X X _','_ _ X','X _ X','_ X X','X X X')
  colnames(ER2)=c('0','1','2')
  
  
  return(ER2)
}