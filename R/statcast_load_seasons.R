#' Function for stacking two columns with a third adjusting the color
#'
#' @description This function is used to load MLB data by year from the [baseballr-data repository](https://github.com/sportsdataverse/baseballr-data)
#'
#' @param seasons A numeric vector of 4-digit years associated with MLB seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2015.
#' @param file_type One of `c("rds", "parquet")`
#' @param remove_st boolean. Should Spring Training data be removed? Defaults to `TRUE`
#' @param remove_ex boolean. Should Exhibition data be removed? Defaults to `TRUE`
#'
#' @return Returns the complete statcast dataset from supplied years
#'
#' @importFrom dplyr arrange filter
#'
#' @export
statcast_load_seasons <- function(seasons = most_recent_mlb_season(), file_type = "rds", remove_st = TRUE, remove_ex = TRUE) {
  
  file_type <- rlang::arg_match0(file_type, c("rds", "parquet"))
  
  if(isTRUE(seasons)) seasons <- 2015:most_recent_mlb_season()
  
  stopifnot(is.numeric(seasons),
            seasons >= 2015,
            seasons <= most_recent_mlb_season())
  
  month_list <- c("March", "April", "May", "June", "July", "August", "September", "October", "November")
  years <- seasons
  
  combos <- expand.grid(years = years, months = month_list) %>% dplyr::arrange(.data$years)
  
  file_front <- paste(combos$years, combos$months, sep = "-")
  
  urls <- paste0("https://github.com/sportsdataverse/baseballr-data/raw/statcast-folder/statcast/",
                 file_front, "-StatcastData.", file_type)
  
  out <- load_from_url(urls, "Season-long pitch by pitch data from Baseball Savant", Sys.time(), baseballr = TRUE)
  
  if(remove_st){
    out <- out %>%
      dplyr::filter(.data$game_type != "S")
  }
  
  if(remove_ex){
    out <- out %>%
      dplyr::filter(.data$game_type != "E")
  }
  
  return(out)
}

