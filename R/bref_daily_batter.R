#' @rdname bref_daily_batter
#' @title **Scrape Batter Performance Data Over a Custom Time Frame**
#'
#' @description This function allows you to scrape basic batter statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param t1 First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param t2 Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @return Returns a tibble of batter performance over the requested date range, one row per player, with the following columns:
#'
#'  |col_name |types     |description                                              |
#'  |:--------|:---------|:--------------------------------------------------------|
#'  |season   |integer   |Season year.                                             |
#'  |Name     |character |Player name.                                             |
#'  |Age      |numeric   |Player age during the season.                            |
#'  |Level    |character |League level (e.g. Maj-AL, Maj-NL).                      |
#'  |Team     |character |Team name.                                               |
#'  |G        |numeric   |Games played.                                            |
#'  |PA       |numeric   |Plate appearances.                                       |
#'  |AB       |numeric   |At-bats.                                                 |
#'  |R        |numeric   |Runs scored.                                             |
#'  |H        |numeric   |Hits.                                                    |
#'  |X1B      |numeric   |Singles.                                                 |
#'  |X2B      |numeric   |Doubles.                                                 |
#'  |X3B      |numeric   |Triples.                                                 |
#'  |HR       |numeric   |Home runs.                                               |
#'  |RBI      |numeric   |Runs batted in.                                          |
#'  |BB       |numeric   |Walks (bases on balls).                                  |
#'  |IBB      |numeric   |Intentional walks.                                       |
#'  |uBB      |numeric   |Unintentional walks.                                     |
#'  |SO       |numeric   |Strikeouts.                                              |
#'  |HBP      |numeric   |Times hit by pitch.                                      |
#'  |SH       |numeric   |Sacrifice hits (bunts).                                  |
#'  |SF       |numeric   |Sacrifice flies.                                         |
#'  |GDP      |numeric   |Grounded into double plays.                              |
#'  |SB       |numeric   |Stolen bases.                                            |
#'  |CS       |numeric   |Times caught stealing.                                   |
#'  |BA       |numeric   |Batting average.                                         |
#'  |OBP      |numeric   |On-base percentage.                                      |
#'  |SLG      |numeric   |Slugging percentage.                                     |
#'  |OPS      |numeric   |On-base plus slugging.                                   |
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate arrange desc rename select
#' @importFrom tidyr everything
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(bref_daily_batter(t1="2015-05-10", t2="2015-06-20"))
#' }

bref_daily_batter <- function(t1, t2) {

  df <- NULL
  tryCatch(
    expr = {
      payload <- xml2::read_html(paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=b&lastndays=7&dates=fromandto&fromandto=", t1, ".", t2, "&level=mlb&franch=&stat=&stat_value=0"))
      df <- payload |>
        rvest::html_elements(xpath = '//*[@id="daily"]') |>
        rvest::html_table(fill = TRUE)
      
      df <- as.data.frame(df)[-c(1,3,5)]
      names(df)[1:4] <- c("Name", "Age", "Level", "Team")
      suppressWarnings(
        df[,c(2,5:26)] <- lapply(df[,c(2,5:26)],as.numeric)
      )
      df$X1B <- with(df, H-(X2B+X3B+HR))
      season <- substr(t1, 1, 4)
      df$season <- as.integer(season)
      df$uBB <- with(df, BB-IBB)
      df <- df[,c(28,1:9, 27, 10:15, 29, 16:26)]
      df$Team <- gsub(" $", "", df$Team, perl=T)
      df <- df |> 
        dplyr::filter(.data$Name != "Name")
      
      playerids <- payload |>
        rvest::html_elements("table") |>
        rvest::html_elements("a") |>
        rvest::html_attr("href") |>
        as.data.frame() |>
        dplyr::rename(slug = ".") |>
        dplyr::filter(grepl("players", .data$slug)) |>
        dplyr::mutate(playerid = gsub("/players/gl.fcgi\\?id=",
                                  "", .data$slug)) |>
        dplyr::mutate(playerid = gsub("&t.*","",.data$playerid))
      
      df <- df |>
        dplyr::mutate(bbref_id = playerids$playerid) |>
        dplyr::select("bbref_id", tidyr::everything())
      df <- df |> dplyr::arrange(desc(.data$PA), desc(.data$OPS)) |>
        make_baseballr_data("MLB Daily Batter data from baseball-reference.com",Sys.time())
      Sys.sleep(5)
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no daily batter data available!")
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(df)
}


#' @rdname daily_batter_bref
#' @title **(legacy) Scrape Batter Performance Data Over a Custom Time Frame**
#' @inheritParams bref_daily_batter
#' @return Returns a tibble of batter performance
#' @seealso ```bref_daily_batter()```
#' @keywords legacy
#' @export
daily_batter_bref <-  bref_daily_batter
