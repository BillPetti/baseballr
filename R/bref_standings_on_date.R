#' @rdname bref_standings_on_date
#' @title **Scrape MLB Standings on a Given Date**
#' @description This function allows you to scrape the standings from MLB for any date you choose.
#' @param date a date object
#' @param division One of AL East, AL Central, AL West, AL Overall, NL East,
#' NL Central, NL West, and NL Overall. Which divisions exist depends on the
#' date: before 1969 only `AL Overall` / `NL Overall`; 1969-1993 adds
#' East/West; 1994 onward adds Central. Requesting a division that did not
#' exist for the date errors with the era's available options.
#' @param from a logical indicating whether you want standings up to and
#' including the date (FALSE, default) or rather standings for games played
#' after the date
#' @return Returns a tibble of MLB standings, one row per team in the requested division, with the following columns:
#'
#' |col_name |types     |description                                              |
#' |:--------|:---------|:--------------------------------------------------------|
#' |Tm       |character |Team abbreviation.                                       |
#' |W        |integer   |Wins as of the requested date.                           |
#' |L        |integer   |Losses as of the requested date.                         |
#' |W-L%     |numeric   |Winning percentage.                                      |
#' |GB       |character |Games behind the division leader (-- for the leader).    |
#' |RS       |integer   |Runs scored.                                             |
#' |RA       |integer   |Runs allowed.                                            |
#' |pythW-L% |numeric   |Pythagorean (expected) winning percentage from RS/RA.    |
#'
#' @import rvest 
#' @importFrom lubridate day month year
#' @export
#' @examples \donttest{
#'   try(bref_standings_on_date(date = "2015-08-04", division = "AL East"))
#' }

bref_standings_on_date <- function(date, division, from = FALSE) {
  yr <- lubridate::year(date)

  # Division structure by era (#253): before 1969 the leagues had no
  # divisions; 1969-1993 each league split East/West; 1994 onward
  # East/Central/West. The B-Ref page only publishes tables that existed,
  # so validate the request against the era instead of a fixed list.
  era_divisions <-
    if (yr < 1969) {
      c("AL Overall", "NL Overall")
    } else if (yr < 1994) {
      c("AL East", "AL West", "AL Overall",
        "NL East", "NL West", "NL Overall")
    } else {
      c("AL East", "AL Central", "AL West", "AL Overall",
        "NL East", "NL Central", "NL West", "NL Overall")
    }
  if (!(division %in% era_divisions)) {
    cli::cli_abort(c(
      "No {.val {division}} standings exist for {yr}.",
      "i" = "Divisions published for {yr}: {.val {era_divisions}}."
    ))
  }

  url <- paste0("https://www.baseball-reference.com/boxes",
                "?year=", sprintf("%04i", yr), "&month=",
                sprintf("%02i", lubridate::month(date)), "&day=", sprintf("%02i",
                                                                          lubridate::day(date)))

  x <- NULL
  tryCatch(
    expr = {
      html_doc <- bref_read_html(url)

      # The page carries one section heading per standings table, in DOM
      # order: first every division "up to and including" the date, then the
      # same set for games "after" the date (verified empirically via games
      # played: the first block averages the season-to-date game count).
      # Name the tables from the headings rather than a fixed-order vector,
      # so the era's table count (4, 12, or 16) never misaligns.
      headings <- html_doc |>
        rvest::html_elements(".section_heading h2") |>
        rvest::html_text() |>
        gsub(pattern = "\\s+", replacement = " ") |>
        trimws()
      stand_names <- headings[grepl("Division$|Overall$", headings)] |>
        gsub(pattern = " Division$", replacement = "")

      tables <- html_doc |>
        rvest::html_elements("table")
      n <- length(stand_names)
      stand_tables <- tables[(length(tables) - n + 1):length(tables)] |>
        rvest::html_table()

      half <- n / 2
      block <- if (isTRUE(from)) (half + 1):n else 1:half
      idx <- which(stand_names[block] == division)
      x <- stand_tables[[block[idx[1]]]] |>
        make_baseballr_data("MLB Standings on Date data from baseball-reference.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no standings on date data available!")
      cli::cli_alert_info("Original error: {conditionMessage(e)}")
    },
    finally = {
    }
  )
  return(x)
}

#' @rdname standings_on_date_bref
#' @title **(legacy) Scrape MLB Standings on a Given Date**
#' @inheritParams bref_standings_on_date
#' @return Returns a tibble of MLB standings
#' @keywords legacy
#' @export
standings_on_date_bref <- bref_standings_on_date
