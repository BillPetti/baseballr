#' Query Statcast and PITCHf/x Data for Batters from baseballsavant.mlb.com
#'
#' This function allows you to query Statcast and PITCHf/x data as provided on baseballsavant.mlb.com and have that data returned as a dataframe.
#' @param start_date Date of first game for which you want data. Format must be in YYYY-MM-DD format.
#' @param end_date Date of last game for which you want data. Format must be in YYYY-MM-DD format.
#' @param batterid The MLBAM ID for the batter who's data you want to query.
#' @keywords MLB, sabermetrics, Statcast
#' @importFrom utils read.csv
#' @export
#' @examples
#' \dontrun{
#' scrape_statcast_savant_batter(start_date = "2016-04-06", end_date = "2016-04-15", batterid = 621043)
#' }

scrape_statcast_savant_batter <- function(start_date, end_date, batterid) {
    # Check to make sure args are in the correct format.
    if(!is.character(start_date) | !is.character(end_date)) {
        warning("Please wrap your dates in quotations in 'yyyy-mm-dd' format.")
        return(NULL)
    }
    # Check for other user errors.
    if(as.Date(start_date)<="2015-03-01") { # March 1, 2015 was the first date of Spring Training.
        message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
    }
    if(as.Date(start_date)<="2008-03-25") { # March 25, 2008 was the first date of Spring Training.
        stop("The data are limited to the 2008 MLB season and after.")
        return(NULL)
    }
    if(as.Date(start_date)==Sys.Date()) {
        message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
    }
    if(as.Date(start_date)>as.Date(end_date)) {
        stop("The start date is later than the end date.")
        return(NULL)
    }

  # extract season from start_date

  year <- substr(start_date, 1,4)

    # Base URL.
    url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C&hfC=&hfSea=", year, "%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=",start_date,"&game_date_lt=",end_date,"&player_lookup%5B%5D=",batterid,"&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")

    # Do a try/catch to show errors that the user may encounter while downloading.
    tryCatch(
        {
            print("These data are from BaseballSevant and are property of MLB Advanced Media, L.P. All rights reserved.")
            print("Grabbing data, this may take a minute...")
            payload <- utils::read.csv(url)

        },
        error=function(cond) {
            message(paste("URL does not seem to exist, please check your Internet connection:"))
            message("Original error message:")
            message(cond)
            return(NA)
        },
        warning=function(cond) {
            message(paste("URL caused a warning. Make sure your batterid and date range are correct:"))
            message("Original warning message:")
            message(cond)
            return(NULL)
        }
    )
    # Clean up formatting.
    payload[payload=="null"] <- NA
    payload$game_date <- as.Date(payload$game_date, "%Y-%m-%d")
    payload$des <- as.character(payload$des)
    payload$game_pk <- as.character(payload$game_pk) %>% as.numeric()
    payload$on_1b <- as.character(payload$on_1b) %>% as.numeric()
    payload$on_2b <- as.character(payload$on_2b) %>% as.numeric()
    payload$on_3b <- as.character(payload$on_3b) %>% as.numeric()
    payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
    payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
    payload$hit_distance_sc <- as.character(payload$hit_distance_sc) %>% as.numeric()
    payload$launch_speed <- as.character(payload$launch_speed) %>% as.numeric()
    payload$launch_angle <- as.character(payload$launch_angle) %>% as.numeric()
    payload$effective_speed <- as.character(payload$effective_speed) %>% as.numeric()
    payload$release_spin_rate <- as.character(payload$release_spin_rate) %>% as.numeric()
    payload$release_extension <- as.character(payload$release_extension) %>% as.numeric()
    payload$pitch_name <- as.character(payload$pitch_name)
    payload$home_score <- as.character(payload$home_score) %>% as.numeric()
    payload$away_score <- as.character(payload$away_score) %>% as.numeric()
    payload$bat_score	<- as.character(payload$bat_score) %>% as.numeric()
    payload$fld_score <- as.character(payload$fld_score) %>% as.numeric()
    payload$post_away_score <- as.character(payload$post_away_score) %>% as.numeric()
    payload$post_home_score	<- as.character(payload$post_home_score) %>% as.numeric()
    payload$post_bat_score <- as.character(payload$post_bat_score) %>% as.numeric()
    payload$post_fld_score <- as.character(payload$post_fld_score) %>% as.numeric()
    payload$barrel <- with(payload, ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 11 & launch_speed + launch_angle >= 124, 1, 0))
    message("URL read and payload aquired successfully.")

    return(payload)

}
