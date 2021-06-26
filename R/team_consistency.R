#' Calculate Team-level Consistency
#'
#' This function allows you to calculate team-level consistency in run scoring and run prevention over the course of an entire season.
#' @param year Season consistency should be run for.
#' @importFrom reldist gini
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @export
#' @examples \dontrun{team_consistency(year=2015)}

#load packages: currently requires XML to scrape, dplyr to tidy, reldist for Gini calculations. Will be switching to rvest for scraping

team_consistency <- function(year) {

  message('Data courtesy of Baseball-Reference.com. Please consider supporting Baseball-Reference by signing up for a Stathead account: https://stathead.com')

  url <- paste0("http://www.baseball-reference.com/leagues/MLB/",year,".shtml")

  

  teams <- (url %>% 
              xml2::read_html() %>% 
              rvest::html_nodes("table"))[1] %>%
    rvest::html_table() %>%
    as.data.frame()
  # teams <- teams %>% 
  #   dplyr::select(.data$Tm)
  teams <- teams %>% 
    dplyr::filter(.data$Tm!="LgAvg", .data$Tm!="Tm")
  teams <- teams[c(1:30),] %>%
    as.data.frame()
  teams$year <- year
  teams <- teams %>% 
    dplyr::mutate(
      Tm = gsub(" of Anaheim","",.data$Tm)
    )
  teams_data <- baseballr::teams_lu_table
  teams <- teams %>% 
    dplyr::left_join(teams_data %>% dplyr::select(.data$name,.data$bref_abbreviation), by=c("Tm"="name"))
  teams <- teams %>% 
    dplyr::mutate(
      bref_abbreviation =dplyr::case_when(
        .data$year<=2015 & .data$Tm == "Los Angeles Angels" ~ "LAA",
        .data$year<2012 & .data$Tm == "Florida Marlins" ~ "FLA",
         TRUE ~ .data$bref_abbreviation)
    )
  # names(teams) <- c("Tm", "year")
  tms <- unique(teams$bref_abbreviation)
  results <- purrr::map_df(tms, function(x){
      df <- team_results_bref(Tm=x, year)
      return(df)
    })
    

  results <- results %>% 
    dplyr::select(.data$Year, .data$Date, .data$Tm, .data$R, .data$RA)
  names(results) <- c("Year", "Date", "Team", "R", "RA")
  attr(results, "vars") <- NULL
  results$R <- as.numeric(results$R)
  results$RA <- as.numeric(results$RA)
  results <- results %>% 
    dplyr::filter(!is.na(.data$R))
  RGini <- results %>%
    dplyr::group_by(.data$Team) %>%
    dplyr::summarize(R = gini(.data$R))
  RAGini <- results %>%
    dplyr::group_by(.data$Team) %>%
    dplyr::summarize(RA = gini(.data$RA))
  VOL <- RGini %>% dplyr::left_join(RAGini, by = "Team")
  VOL$R <- round(VOL$R, 2)
  VOL$RA <- round(VOL$RA, 2)
  colnames(VOL)[1] <- "bref_t"
  VOL <- VOL %>%
    dplyr::mutate(percrank = rank(.data$R)/length(.data$R))
  colnames(VOL)[4] <- "R_Ptile"
  VOL <- VOL %>%
    dplyr::mutate(percrank = rank(.data$RA)/length(.data$RA))
  colnames(VOL)[5] <- "RA_Ptile"
  VOL$R_Ptile <- round(VOL$R_Ptile, 2)*100
  VOL$RA_Ptile <- round(VOL$RA_Ptile, 2)*100
  names(VOL) <- c("Team", "Con_R", "Con_RA", "Con_R_Ptile", "Con_RA_Ptile")
  
  
  return(VOL)
}

