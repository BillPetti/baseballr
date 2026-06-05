
#' @rdname fg_park
#' @title **Scrape Park Factors from FanGraphs**
#' @description This function allows you to scrape park factors for a given season from FanGraphs.com.
#' @param yr Season for which you want to scrape the park factors.
#' @return Returns a tibble of park factors.
#'
#'  |col_name  |types     |description                                       |
#'  |:---------|:---------|:-------------------------------------------------|
#'  |season    |integer   |Season (YYYY).                                    |
#'  |home_team |character |Home team name.                                   |
#'  |basic_5yr |integer   |Basic 5-year park factor (100 = neutral).         |
#'  |3yr       |integer   |3-year park factor (100 = neutral).               |
#'  |1yr       |integer   |1-year park factor (100 = neutral).               |
#'  |single    |integer   |Park factor for singles.                          |
#'  |double    |integer   |Park factor for doubles.                          |
#'  |triple    |integer   |Park factor for triples.                          |
#'  |hr        |integer   |Park factor for home runs.                        |
#'  |so        |integer   |Park factor for strikeouts.                       |
#'  |UIBB      |integer   |Park factor for unintentional walks.              |
#'  |GB        |integer   |Park factor for ground balls.                     |
#'  |FB        |integer   |Park factor for fly balls.                        |
#'  |LD        |integer   |Park factor for line drives.                      |
#'  |IFFB      |integer   |Park factor for infield fly balls.                |
#'  |FIP       |integer   |Park factor applied to FIP.                       |
#'
#' @export
#' @examples \donttest{
#'   try(fg_park(2013))
#' }

fg_park <- function(yr) {
  
  park_table <- NULL
  tryCatch(
    expr = {
      park_table <- paste0("https://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", yr) |>
        httr2::request() |>
        httr2::req_user_agent("okhttp/4.12.0") |>
        httr2::req_perform() |>
        httr2::resp_body_string() |>
        xml2::read_html() |>
        rvest::html_element(".table-scroll table") |>
        rvest::html_table() |>
        setNames(c("season", "home_team", "basic_5yr", "3yr", "1yr", "single", "double", "triple", "hr",
                   "so", "UIBB", "GB", "FB", "LD", "IFFB", "FIP"))
      park_table <- park_table |> 
        make_baseballr_data("Park Factors data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no park factors data available!")
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
#'
#'  |col_name      |types     |description                                  |
#'  |:-------------|:---------|:--------------------------------------------|
#'  |season        |integer   |Season (YYYY).                               |
#'  |home_team     |character |Home team name.                              |
#'  |single_as_LHH |integer   |Singles park factor for left-handed hitters. |
#'  |single_as_RHH |integer   |Singles park factor for right-handed hitters.|
#'  |double_as_LHH |integer   |Doubles park factor for left-handed hitters. |
#'  |double_as_RHH |integer   |Doubles park factor for right-handed hitters.|
#'  |triple_as_LHH |integer   |Triples park factor for left-handed hitters. |
#'  |triple_as_RHH |integer   |Triples park factor for right-handed hitters.|
#'  |hr_as_LHH     |integer   |Home run park factor for left-handed hitters.|
#'  |hr_as_RHH     |integer   |Home run park factor for right-handed hitters.|
#'
#' @importFrom stats setNames
#' @export
#' @examples \donttest{
#'   try(fg_park_hand(2013))
#' }

fg_park_hand <- function(yr) {
  park_table <- NULL
  tryCatch(
    expr = {
      park_table <- paste0("https://www.fangraphs.com/guts.aspx?type=pfh&teamid=0&season=", yr) |>
        httr2::request() |>
        httr2::req_user_agent("okhttp/4.12.0") |>
        httr2::req_perform() |>
        httr2::resp_body_string() |>
        xml2::read_html() |>
        rvest::html_element(".table-scroll table") |>
        rvest::html_table() |>
        stats::setNames(c("season", "home_team", "single_as_LHH", "single_as_RHH",
                          "double_as_LHH", "double_as_RHH", "triple_as_LHH", "triple_as_RHH",
                          "hr_as_LHH", "hr_as_RHH"))
      park_table <- park_table |> 
        make_baseballr_data("Park Factors by Handedness data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no park factors by handedness data available!")
    },
    finally = {
    }
  )
  return(park_table)
}
