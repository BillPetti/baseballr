
#' @rdname fg_park
#' @title **Scrape Park Factors from FanGraphs**
#' @description This function allows you to scrape park factors for a given season from FanGraphs.com.
#' @param yr Season for which you want to scrape the park factors.
#' @return Returns a tibble of park factors.
#'  |col_name  |types     |
#'  |:---------|:---------|
#'  |season    |integer   |
#'  |home_team |character |
#'  |basic_5yr |integer   |
#'  |3yr       |integer   |
#'  |1yr       |integer   |
#'  |single    |integer   |
#'  |double    |integer   |
#'  |triple    |integer   |
#'  |hr        |integer   |
#'  |so        |integer   |
#'  |UIBB      |integer   |
#'  |GB        |integer   |
#'  |FB        |integer   |
#'  |LD        |integer   |
#'  |IFFB      |integer   |
#'  |FIP       |integer   |
#' @export
#' @examples \donttest{
#'   try(fg_park(2013))
#' }

fg_park <- function(yr) {
  
  tryCatch(
    expr={
      park_table <- paste0("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", yr) %>% 
        xml2::read_html() %>%
        rvest::html_element(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
        rvest::html_table() %>%
        setNames(c("season", "home_team", "basic_5yr", "3yr", "1yr", "single", "double", "triple", "hr",
                   "so", "UIBB", "GB", "FB", "LD", "IFFB", "FIP"))
      park_table <- park_table %>% 
        make_baseballr_data("Park Factors data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no park factors data available!"))
    },
    finally = {
    }
  )
  return(park_table)
}

#' @rdname fg_park
#' @title **Scrape Park Factors by handedness from FanGraphs**
#'
#' @description This function allows you to scrape park factors by handedness from FanGraphs.com for a given single year.
#' @param yr Season for which you want to scrape the park factors.
#' @return Returns a tibble of park factors by handedness.
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |season        |integer   |
#'  |home_team     |character |
#'  |single_as_LHH |integer   |
#'  |single_as_RHH |integer   |
#'  |double_as_LHH |integer   |
#'  |double_as_RHH |integer   |
#'  |triple_as_LHH |integer   |
#'  |triple_as_RHH |integer   |
#'  |hr_as_LHH     |integer   |
#'  |hr_as_RHH     |integer   |
#' @importFrom stats setNames
#' @export
#' @examples \donttest{
#'   try(fg_park_hand(2013))
#' }

fg_park_hand <- function(yr) {
  tryCatch(
    expr={
      park_table <- paste0("http://www.fangraphs.com/guts.aspx?type=pfh&teamid=0&season=", yr) %>% 
        xml2::read_html() %>%
        rvest::html_element(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
        rvest::html_table() %>%
        stats::setNames(c("season", "home_team", "single_as_LHH", "single_as_RHH",
                          "double_as_LHH", "double_as_RHH", "triple_as_LHH", "triple_as_RHH",
                          "hr_as_LHH", "hr_as_RHH"))
      park_table <- park_table %>% 
        make_baseballr_data("Park Factors by Handedness data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no park factors by handedness data available!"))
    },
    finally = {
    }
  )
  return(park_table)
}
