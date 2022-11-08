#' @rdname fg_guts
#' @title **Scrape FanGraphs.com Guts!**
#' @description Scrape historical FanGraphs Guts! table, wOBA, FIP coefficients and constants
#' @return Returns a tibble of seasonal constants from FanGraphs
#'  |col_name   |types   |
#'  |:----------|:-------|
#'  |season     |integer |
#'  |lg_woba    |numeric |
#'  |woba_scale |numeric |
#'  |wBB        |numeric |
#'  |wHBP       |numeric |
#'  |w1B        |numeric |
#'  |w2B        |numeric |
#'  |w3B        |numeric |
#'  |wHR        |numeric |
#'  |runSB      |numeric |
#'  |runCS      |numeric |
#'  |lg_r_pa    |numeric |
#'  |lg_r_w     |numeric |
#'  |cFIP       |numeric |
#' @import rvest
#' @export
#' @examples \donttest{
#'   try(fg_guts())
#' }
fg_guts <- function() {
  tryCatch(
    expr = {
      guts_table <- "http://www.fangraphs.com/guts.aspx?type=cn" %>% 
        xml2::read_html() %>%
        rvest::html_element(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
        rvest::html_table() %>%
        setNames(c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B",
                   "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
      
      guts_table <- guts_table %>%
        make_baseballr_data("GUTS data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no GUTS data available!"))
    },
    finally = {
    }
  )
  return(guts_table)
}  
