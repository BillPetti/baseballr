#' @rdname bref_daily_pitcher
#' @title **Scrape Pitcher Performance Data Over a Custom Time Frame**
#'
#' @description This function allows you to scrape basic pitcher statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param t1 First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param t2 Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @return Returns a tibble of pitcher performance over the requested date range, one row per player, with the following columns:
#'
#'  |col_name |types     |description                                              |
#'  |:--------|:---------|:--------------------------------------------------------|
#'  |bbref_id |character |Baseball-Reference player id (slug).                     |
#'  |season   |integer   |Season year.                                             |
#'  |Name     |character |Player name.                                             |
#'  |Age      |numeric   |Player age during the season.                            |
#'  |Level    |character |League level (e.g. Maj-AL, Maj-NL).                      |
#'  |Team     |character |Team name.                                               |
#'  |G        |numeric   |Games pitched.                                           |
#'  |GS       |numeric   |Games started.                                           |
#'  |W        |numeric   |Wins.                                                    |
#'  |L        |numeric   |Losses.                                                  |
#'  |SV       |numeric   |Saves.                                                   |
#'  |IP       |numeric   |Innings pitched.                                         |
#'  |H        |numeric   |Hits allowed.                                            |
#'  |R        |numeric   |Runs allowed.                                            |
#'  |ER       |numeric   |Earned runs allowed.                                     |
#'  |uBB      |numeric   |Unintentional walks allowed.                             |
#'  |BB       |numeric   |Walks allowed.                                           |
#'  |SO       |numeric   |Strikeouts.                                              |
#'  |HR       |numeric   |Home runs allowed.                                       |
#'  |HBP      |numeric   |Batters hit by pitch.                                    |
#'  |ERA      |numeric   |Earned run average (per 9 innings).                      |
#'  |AB       |numeric   |At-bats against.                                         |
#'  |X1B      |numeric   |Singles allowed.                                         |
#'  |X2B      |numeric   |Doubles allowed.                                         |
#'  |X3B      |numeric   |Triples allowed.                                         |
#'  |IBB      |numeric   |Intentional walks allowed.                               |
#'  |GDP      |numeric   |Double plays induced.                                    |
#'  |SF       |numeric   |Sacrifice flies allowed.                                 |
#'  |SB       |numeric   |Stolen bases allowed.                                    |
#'  |CS       |numeric   |Runners caught stealing.                                 |
#'  |PO       |numeric   |Pickoffs.                                                |
#'  |BF       |numeric   |Batters faced.                                           |
#'  |Pit      |numeric   |Total pitches thrown.                                    |
#'  |Str      |numeric   |Strikes thrown (as a share of pitches).                  |
#'  |StL      |numeric   |Looking (called) strikes share.                          |
#'  |StS      |numeric   |Swinging strikes share.                                  |
#'  |GB.FB    |numeric   |Ground ball to fly ball share.                           |
#'  |LD       |numeric   |Line drive share.                                        |
#'  |PU       |numeric   |Pop-up (infield fly) share.                              |
#'  |WHIP     |numeric   |Walks plus hits per inning pitched.                      |
#'  |BAbip    |numeric   |Batting average on balls in play allowed.                |
#'  |SO9      |numeric   |Strikeouts per 9 innings.                                |
#'  |SO.W     |numeric   |Strikeout-to-walk ratio.                                 |
#'  |SO_perc  |numeric   |Strikeout rate (per batter faced).                       |
#'  |uBB_perc |numeric   |Unintentional walk rate (per batter faced).              |
#'  |SO_uBB   |numeric   |Strikeouts minus unintentional walks.                    |
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate arrange desc rename select
#' @importFrom tidyr everything
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(bref_daily_pitcher("2015-05-10", "2015-06-20"))
#' }

bref_daily_pitcher <- function(t1, t2) {
  
  df <- NULL
  tryCatch(
    expr = {
      payload <- paste0("https://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=p&lastndays=7&dates=fromandto&fromandto=", t1, ".", t2, "&level=mlb&franch=&stat=&stat_value=0") |> 
        xml2::read_html()
      
      df <- payload |>
        rvest::html_elements(xpath = '//*[@id="daily"]') |>
        rvest::html_table(fill = TRUE)
      
      df <- as.data.frame(df)[-c(1,3,5)]
      names(df)[1:4] <- c("Name", "Age", "Level", "Team")
      suppressWarnings(
        df[,c(2,5:29, 36:39)] <- lapply(df[,c(2,5:29, 36:39)], as.numeric)
      )
      df$X1B <- with(df, H-(X2B+X3B+HR))
      season <- substr(t1, 1, 4)
      df$season <- as.integer(season)
      df$uBB <- with(df, BB-IBB)
      
      suppressWarnings(
        df[,30] <- as.numeric(sub("%", "", df[, 30]))
      )
      suppressWarnings(
        df[,31] <- as.numeric(sub("%", "", df[, 31]))
      )
      suppressWarnings(
        df[,32] <- as.numeric(sub("%", "", df[, 32]))
      )
      suppressWarnings(
        df[,33] <- as.numeric(sub("%", "", df[, 33]))
      )
      suppressWarnings(
        df[,34] <- as.numeric(sub("%", "", df[, 34]))
      )
      suppressWarnings(
        df[,35] <- as.numeric(sub("%", "", df[, 35]))
      )
      df[,c(30:35)] <- df[,c(30:35)]/100
      df <- df[,c(41,1:13,42,14:19,40,20:39)]
      df$SO_perc <- with(df, round(SO/BF,3))
      df$uBB_perc <- with(df, round(uBB/BF,3))
      df$SO_uBB <- with(df, round(SO_perc - uBB_perc))
      df$Team <- gsub(" $", "", df$Team, perl=T)
      df <- df |> 
        dplyr::filter(.data$Name != "Name")
      
      slugs <- payload |>
        rvest::html_elements(xpath = '//*[@id="daily"]') |>
        rvest::html_elements("a") |>
        rvest::html_attr("href")
      slugs <- slugs[grepl("players", slugs)]
      playerids <- data.frame(slug = slugs, stringsAsFactors = FALSE)
      playerids$playerid <- gsub("/players/gl.fcgi?id=", "", playerids$slug, fixed = TRUE)
      playerids$playerid <- gsub("&t.*", "", playerids$playerid)
      
      df <- df |>
        dplyr::mutate(bbref_id = playerids$playerid) |>
        dplyr::select("bbref_id", tidyr::everything()) |>
        dplyr::arrange(desc(.data$IP), desc(.data$WHIP))
      
      df <- df |>
        make_baseballr_data("MLB Daily Pitcher data from baseball-reference.com",Sys.time())
      Sys.sleep(5)
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no daily pitcher data available!")
    },
    finally = {
    }
  )
  return(df)
}


#' @rdname daily_pitcher_bref
#' @title **(legacy) Scrape Pitcher Performance Data Over a Custom Time Frame**
#' @inheritParams bref_daily_pitcher
#' @return Returns a tibble of pitcher performance 
#' @seealso ```bref_daily_pitcher()```
#' @keywords legacy
#' @export
daily_pitcher_bref <- bref_daily_pitcher
