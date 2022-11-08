#' @rdname team_consistency
#' @title **Calculate Team-level Consistency**
#' @description This function allows you to calculate team-level consistency in run scoring and run prevention over the course of an entire season.
#' @param year Season consistency should be run for.
#' @return Returns a tibble with the following columns
#'  |col_name     |types     |
#'  |:------------|:---------|
#'  |Team         |character |
#'  |Con_R        |numeric   |
#'  |Con_RA       |numeric   |
#'  |Con_R_Ptile  |numeric   |
#'  |Con_RA_Ptile |numeric   |
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(team_consistency(year=2021))
#' }

team_consistency <- function(year) {
  
  teams_data <- baseballr::teams_lu_table
  
  url <- paste0("http://www.baseball-reference.com/leagues/MLB/",year,".shtml")

  teams <- (url %>% 
              xml2::read_html() %>% 
              rvest::html_elements("table"))[1] %>%
    rvest::html_table() %>%
    as.data.frame()
  
  Sys.sleep(5)
  teams <- teams %>% 
    dplyr::filter(.data$Tm != "LgAvg", .data$Tm != "Tm")
  
  teams <- teams[c(1:30),] %>%
    as.data.frame()
  
  teams$year <- year
  
  teams <- teams %>% 
    dplyr::mutate(
      Tm = gsub(" of Anaheim","",.data$Tm))
  
  teams <- teams %>% 
    dplyr::left_join(teams_data %>% 
                       dplyr::select("name", "bref_abbreviation"), 
                     by = c("Tm" = "name"))
  teams <- teams %>% 
    dplyr::mutate(
      bref_abbreviation = dplyr::case_when(
        .data$year <= 2015 & .data$Tm == "Los Angeles Angels" ~ "LAA",
        .data$Tm == "Los Angeles Angels" ~ "LAA",
        .data$year < 2012 & .data$Tm == "Florida Marlins" ~ "FLA",
         TRUE ~ .data$bref_abbreviation))
  
  tms <- unique(teams$bref_abbreviation)
  results <- purrr::map_df(tms, function(x){
      df <- bref_team_results(Tm=x, year)
      return(df)
    })
    

  results <- results %>% 
    dplyr::select("Year", "Date", "Tm", "R", "RA")
  
  names(results) <- c("Year", "Date", "Team", "R", "RA")
  attr(results, "vars") <- NULL
  
  results$R <- as.numeric(results$R)
  results$RA <- as.numeric(results$RA)
  
  results <- results %>% 
    dplyr::filter(!is.na(.data$R))
  
  RGini <- results %>%
    dplyr::group_by(.data$Team) %>%
    dplyr::summarize(R = gini.wtd(.data$R))
  
  RAGini <- results %>%
    dplyr::group_by(.data$Team) %>%
    dplyr::summarize(RA = gini.wtd(.data$RA))
  
  VOL <- RGini %>% 
    dplyr::left_join(RAGini, by = "Team")
  
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

gini.wtd <- function (x, weights = NULL){
  if(is.null(weights)){
    weights <- rep(1, length(x))
  }
  
  missing <- !(is.na(x) | is.na(weights))
  x <- x[missing]
  weights <- weights[missing]
  if (!all(weights>=0)) stop("At least one weight is negative", call.=FALSE)
  if (all(weights == 0)) stop("All weights are zero", call.=FALSE)
  weights <- weights/sum(weights)
  
  order <- order(x)
  x <- x[order]
  weights <- weights[order]
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  gini <- sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
  return(gini)
}
