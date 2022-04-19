#' @rdname retrosheet_data
#' @title **Get, Parse, and Format Retrosheet Event and Roster Files**
#' @details  
#' ```r
#' get_retrosheet_data(path_to_directory = "/Users/Documents/retrosheet/", 
#'     years_to_acquire = c(1957,1959), 
#'     sequence_years = F)
#' ```
#' @param path_to_directory A file path that either 1) creates a new directory
#' or 2) a path to an existing directory
#' @param years_to_acquire The seasons to collect. Single, multiple, and
#' sequential years can be passed. If passing multiple years, enclose in a
#' vector (i.e. c(2017,2018)). Defaults to ```most_recent_mlb_season()```.
#' @param sequence_years If the seasons passed in the years_to_acquire parameter
#' should be sequenced so that the function returns all years including and
#' between the vector passed, set the argument to TRUE. Defaults to FALSE.
#' @return Returns two csv files to the unzipped directory: 1) a combined csv
#' of the event data for a given year and 2) a combined csv of each team's
#' roster for each year
#' @importFrom dplyr mutate mutate_if
#' @importFrom janitor clean_names
#' @importFrom purrr map
#' @importFrom utils download.file tail unzip write.csv
#' @export

retrosheet_data <- function(path_to_directory,
                            years_to_acquire = most_recent_mlb_season(),
                            sequence_years = FALSE){
  # create a record for the starting working directory
  oldwd <- getwd()
  # reset to starting working directory
  on.exit(setwd(oldwd))
  if(dir.exists(path_to_directory) == FALSE) {

    dir.create(path_to_directory)
    
    # setwd
    setwd(path_to_directory)
    
    # create folders
    dir.create(paste0(path_to_directory, "/download.folder"))
    dir.create(paste0(path_to_directory, "/download.folder/unzipped"))
    dir.create(paste0(path_to_directory, "/download.folder/zipped"))
  } else {
    # setwd
    setwd(path_to_directory)
  }

  fields <- csv_from_url("https://raw.githubusercontent.com/maxtoki/baseball_R/master/data/fields.csv")

  data.table::fwrite(fields, file = paste0(path_to_directory, "/fields.csv"))

  if(sequence_years == FALSE) {

    years <- years_to_acquire
  } else {

    years <-seq(years_to_acquire[1], years_to_acquire[2],1)
  }

  purrr::map(.x = years,
             ~acquire_parse_restrosheet_event(season = .x,
                                              wd = path_to_directory))
  
}
#' @rdname get_retrosheet_data
#' @title **(legacy) Get, Parse, and Format Retrosheet Event and Roster Files**
#' @inheritParams retrosheet_data
#' @return Returns two csv files to the unzipped directory: 1) a combined csv
#' of the event data for a given year and 2) a combined csv of each team's
#' roster for each year
#' @keywords legacy
#' @export
get_retrosheet_data <- retrosheet_data 

acquire_parse_restrosheet_event <- function(season, wd){
  
  ## This code was adapted from Jim Albert
  ## source: https://gist.github.com/bayesball/8892981
  ## Asssumes you have downloaded and installed The Chadwick Files
  ## source: https://sourceforge.net/projects/chadwick/files/
  
  
  # ADJUSTED FOR MAC -- function will work for WINDOWS and MAC
  # download, unzip, append retrosheet data
  # assume current directory has a folder download.folder
  # download.folder has two subfolders unzipped and zipped
  # program cwevent.exe is in unzipped folder (for windows)
  # create a record for the starting working directory
  oldwd <- getwd()
  # reset to starting working directory
  on.exit(setwd(oldwd))
  setwd(wd)
  
  download.retrosheet(wd, season)
  unzip.retrosheet(season)
  create.csv.file(wd, season)
  create.csv.roster(wd, season)
  cleanup(wd)
}

download.retrosheet <- function(wd, season){
  # get zip file from retrosheet website
  download.file(
    url=paste("https://www.retrosheet.org/events/", season, "eve.zip", sep="")
    , destfile=paste(wd, "/download.folder", "/zipped/",
                     season, "eve.zip", sep="")
  )
}

unzip.retrosheet <- function(season){
  #unzip retrosheet files
  unzip(paste("download.folder", "/zipped/", season, "eve.zip", sep=""),
        exdir=paste("download.folder", "/unzipped", sep=""))
}

create.csv.file <- function(wd, year){
  # http://chadwick.sourceforge.net/doc/cwevent.html#cwtools-cwevent
  # shell("cwevent -y 2000 2000TOR.EVA > 2000TOR.bev")
  
  # create a record for the starting working directory
  oldwd <- getwd()
  # reset to starting working directory
  on.exit(setwd(oldwd))
  setwd(paste0(wd, "/download.folder/unzipped"))
  
  character_vars <- c('GAME_ID', 'AWAY_TEAM_ID', 'PITCH_SEQ_TX',
                      'BAT_ID', 'BAT_HAND_CD', 'RESP_BAT_ID',
                      'RESP_BAT_HAND_CD', 'PIT_ID', 'PIT_HAND_CD',
                      'RESP_PIT_ID', 'RESP_PIT_HAND_CD', 'POS2_FLD_ID',
                      'POS3_FLD_ID', 'POS4_FLD_ID', 'POS5_FLD_ID',
                      'POS6_FLD_ID', 'POS7_FLD_ID', 'POS8_FLD_ID',
                      'POS9_FLD_ID', 'BASE1_RUN_ID', 'BASE2_RUN_ID',
                      'BASE3_RUN_ID', 'EVENT_TX', 'BATTEDBALL_CD',
                      'BATTEDBALL_LOC_TX', 'ERR1_CD', 'ERR2_CD',
                      'ERR3_CD', 'RUN1_RESP_PIT_ID', 'RUN2_RESP_PIT_ID',
                      'RUN3_RESP_PIT_ID', 'REMOVED_FOR_PR_RUN1_ID',
                      'REMOVED_FOR_PR_RUN2_ID', 'REMOVED_FOR_PH_BAT_ID')
  
  numeric_vars <- c('INN_CT', 'BAT_HOME_ID', 'OUTS_CT',
                    'BALLS_CT', 'STRIKES_CT', 'AWAY_SCORE_CT',
                    'HOME_SCORE_CT', 'BAT_FLD_CD', 'BAT_LINEUP_ID',
                    'EVENT_CD', 'H_FL', 'EVENT_OUTS_CT',
                    'RBI_CT', 'FLD_CD', 'ERR_CT',
                    'ERR1_FLD_CD', 'ERR2_FLD_CD', 'ERR3_FLD_CD',
                    'BAT_DEST_ID', 'RUN1_DEST_ID', 'RUN2_DEST_ID',
                    'RUN3_DEST_ID', 'BAT_PLAY_TX', 'RUN1_PLAY_TX',
                    'RUN2_PLAY_TX', 'REMOVED_FOR_PH_BAT_FLD_CD',
                    'PO1_FLD_CD', 'PO2_FLD_CD', 'PO3_FLD_CD',
                    'ASS1_FLD_CD', 'ASS2_FLD_CD', 'ASS3_FLD_CD',
                    'ASS4_FLD_CD', 'ASS5_FLD_CD', 'EVENT_ID', 'year')
  
  logical_vars <- c('LEADOFF_FL', 'PH_FL', 'BAT_EVENT_FL',
                    'AB_FL', 'SH_FL', 'SF_FL', 'DP_FL',
                    'TP_FL', 'WP_FL', 'PB_FL', 'BUNT_FL',
                    'FOUL_FL', 'RUN3_PLAY_TX', 'RUN1_SB_FL',
                    'RUN2_SB_FL', 'RUN3_SB_FL', 'RUN1_CS_FL',
                    'RUN2_CS_FL', 'RUN3_CS_FL', 'RUN1_PK_FL',
                    'RUN2_PK_FL', 'RUN3_PK_FL', 'GAME_NEW_FL',
                    'GAME_END_FL', 'PR_RUN1_FL', 'PR_RUN2_FL',
                    'PR_RUN3_FL', 'REMOVED_FOR_PR_RUN3_ID')
  
  if (.Platform$OS.type == "unix"){
    system(paste(paste("cwevent -y", year, "-f 0-96"),
                 paste(year,"*.EV*",sep=""),
                 paste("> all", year, ".csv", sep="")))} else {
                   shell(paste(paste("cwevent -y", year, "-f 0-96"),
                               paste(year,"*.EV*",sep=""),
                               paste("> all", year, ".csv", sep="")))
                 }
  
  fields <- data.table::fread(paste0(wd, "/fields.csv"))
  
  payload <- data.table::fread(paste0("all", year, ".csv"), header = FALSE)
  
  names(payload) <- fields$Header
  
  payload <- payload %>%
    dplyr::mutate(year = year)
  
  payload <- payload %>%
    dplyr::mutate_if(names(payload) %in% character_vars, as.character) %>%
    dplyr::mutate_if(names(payload) %in% numeric_vars, as.numeric) %>%
    dplyr::mutate_if(names(payload) %in% logical_vars, as.logical)
  
  payload <- payload %>%
    janitor::clean_names()
  
  data.table::fwrite(payload, paste0(wd, "/download.folder/unzipped/all", year, ".csv"), header = TRUE)
}

create.csv.roster <- function(wd, year){
  # creates a csv file of the rosters
  filenames <- list.files(path = paste0(wd, "/download.folder/unzipped/"))
  
  filenames.roster <-
    subset(filenames, substr(filenames, 4, 11)==paste(year,".ROS",sep=""))
  
  R <- do.call("rbind", lapply(filenames.roster, function(x){
    read.csv2(wd = wd, file = x)
  }))
  
  names(R)[1:6] = c("Player.ID", "Last.Name", "First.Name",
                    "Bats", "Pitches", "Team")
  R <- R %>%
    dplyr::mutate(year = year) %>%
    janitor::clean_names()
  
  data.table::fwrite(R, file = paste0(wd, "/download.folder/unzipped/roster",
                                      year, ".csv"))
}

read.csv2 <- function(wd, file){
  data.table::fread(file = paste0(wd,
                                  "/download.folder/unzipped/", file),
                    header = FALSE)
}

cleanup <- function(wd){
  # create a record for the starting working directory
  oldwd <- getwd()
  # reset to starting working directory
  on.exit(setwd(oldwd))
  # removes retrosheet files not needed
  setwd(paste0(wd, "/download.folder/unzipped"))
  
  if (.Platform$OS.type == "unix") {
    system("rm *.EDN")
    system("rm *.EDA")
    system("rm *.EVN")
    system("rm *.EVA")
    system("rm *.ROS")
    system("rm TEAM*")} else {
      shell("del *.EDN")
      shell("del *.EDA")
      shell("del *.EVN")
      shell("del *.EVA")
      shell("del *.ROS")
      shell("del TEAM*")
    }
  
  setwd(paste0(wd, "/download.folder/zipped"))
  
  if (.Platform$OS.type == "unix") {
    system("rm *.zip")} else {
      shell("del *.zip")
    }
  
}
