#' @rdname fg_guts
#' @title **Scrape FanGraphs.com Guts!**
#' @description Scrape historical FanGraphs Guts! table, wOBA, FIP coefficients and constants
#' @return Returns a tibble of seasonal constants from FanGraphs
#'
#'  |col_name   |types   |description                                          |
#'  |:----------|:-------|:----------------------------------------------------|
#'  |season     |integer |Season (YYYY).                                       |
#'  |lg_woba    |numeric |League-average wOBA for the season.                  |
#'  |woba_scale |numeric |wOBA scale factor (converts wOBA to runs).           |
#'  |wBB        |numeric |Linear weight (runs) for an unintentional walk.      |
#'  |wHBP       |numeric |Linear weight (runs) for a hit-by-pitch.             |
#'  |w1B        |numeric |Linear weight (runs) for a single.                   |
#'  |w2B        |numeric |Linear weight (runs) for a double.                   |
#'  |w3B        |numeric |Linear weight (runs) for a triple.                   |
#'  |wHR        |numeric |Linear weight (runs) for a home run.                 |
#'  |runSB      |numeric |Run value of a stolen base.                          |
#'  |runCS      |numeric |Run value of a caught stealing.                      |
#'  |lg_r_pa    |numeric |League runs per plate appearance.                    |
#'  |lg_r_w     |numeric |League runs per win.                                 |
#'  |cFIP       |numeric |FIP constant for the season.                         |
#'
#' @import rvest
#' @export
#' @examples \donttest{
#'   try(fg_guts())
#' }
fg_guts <- function() {
  guts_table <- NULL
  tryCatch(
    expr = {
      # FanGraphs replaced the legacy ASP.NET grid (id "GutsBoard1_dg1_ctl00")
      # with a modern ".table-scroll" data grid; select that table instead.
      guts_table <- "https://www.fangraphs.com/guts.aspx?type=cn" |>
        xml2::read_html() |>
        rvest::html_element(".table-scroll table") |>
        rvest::html_table() |>
        setNames(c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B",
                   "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
      
      guts_table <- guts_table |>
        make_baseballr_data("GUTS data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no GUTS data available!")
    },
    finally = {
    }
  )
  return(guts_table)
}  
