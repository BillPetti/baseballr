#' Scrape NCAA baseball schedule for individual teams (Division I, II, and III)
#'
#' This function allows the user to obtain an individual team's schedule.
#'
#' @param teamid The numerical ID that the NCAA website uses to identify a team
#' @param year The season for which data should be returned, in the form of "YYYY". Years currently available: 2013-2017.
#' @param division Select what division of college baseball 
#' @keywords baseball, NCAA, college
#' @import dplyr
#' @import assertthat
#' @import snakecase
#' @importFrom xml2 "read_html"
#' @importFrom rvest "html_nodes"
#' @importFrom rvest "html_text"
#' @importFrom rvest "html_attr"
#' @export ncaa_get_team_schedule
#' @examples
#' \dontrun{
#' ncaa_get_team_schedule(697, 2017)
#' }
#' 
#' 
#' 
#' 
ncaa_get_team_schedule <- function(teamid, year, division=1,verbose=F){
### need a stop criteria here to make sure that the team id is correct
assert_that(year>=2013,msg='you must provide a year that is equal to or greater than 2013')

team <- master_ncaa_team_lu[(master_ncaa_team_lu$year == year) & 
                              (master_ncaa_team_lu$school_id == teamid) &
                              (master_ncaa_team_lu$division == division)
                            ,"school"] %>% pull()
base_url <- "http://stats.ncaa.org"
yearid = ncaa_season_id_lu[ncaa_season_id_lu$season == year,"id"] %>% pull()
team_url <- paste(base_url, "/team/index/", yearid, "?org_id=", teamid, sep="")

## sleep for 1 second
#Sys.sleep(1)
team_read <- try(team_url %>% read_html())
if (class(team_read)=='try-error'){
  print(paste('Cannot connect to server for', team, 'in', year))
  return(NULL)
}
else{
  x <- team_read %>% html_nodes(".smtext") %>% html_text()
  x_df <- apply(x %>% matrix(ncol=3,byrow=T),2,.stripwhite) %>% data.frame(stringsAsFactors = F)
  colnames(x_df) <- team_read %>% html_nodes("th") %>% html_text()
  
  game_ids <- .get_game_id(team_read %>% html_nodes(".smtext .skipMask") %>% html_attr("href"))
  
  if(verbose){
    print(paste0("Processing data for: ",team))
  }
  
  x_df <- x_df %>% mutate(
    game_id = game_ids,
    team = team,
    year = year,
    loc = case_when(
      grepl("^@", Opponent) ~ "A",
      grepl(" @ ", Opponent, fixed = TRUE) ~ "N",
      TRUE ~ "H"
    ),
    just_runs_result = gsub("\\([^()]*\\)", "", str_sub(Result,3,-1)),
    team_runs = as.numeric(gsub("\\D", "", gsub("-.*$", "", just_runs_result))),
    opp_runs = as.numeric(gsub(".*-", "", just_runs_result)),
    win = case_when(team_runs<opp_runs ~ "L",
                    team_runs>opp_runs ~ "W"),
    innings = as.numeric(ifelse(
      gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", Result, perl = T) == "",
      9,
      gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", Result, perl = T)
    )),
    opp = case_when(
      loc == "A" ~ gsub("@", "", Opponent),
      loc == "H" ~ Opponent,
      loc == "N" ~ substring(Opponent, 1, regexpr("@", Opponent) - 2)
    ),
    total_w = cumsum(ifelse(win == "W",1,0)),
    total_l = cumsum(ifelse(win == "L",1,0)),
    record = paste0(total_w,"-",total_l)
  )
  
  team_schedule <- x_df %>% select(year,Date,game_id,team,opp,loc,win,team_runs,opp_runs,record)
  ## proper capitlization because I'm OCD.
  colnames(team_schedule) <- to_upper_camel_case(colnames(team_schedule))
  
  return(team_schedule)
}

}

### Helper Functions
.stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

.get_game_id=function(x){
  # Get results
  game_id <- as.numeric(gsub("^.*game/index/([0-9]*).*$", "\\1", x))
  return(game_id)
} 
