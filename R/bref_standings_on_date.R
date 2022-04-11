#' @rdname bref_standings_on_date
#' @title **Scrape MLB Standings on a Given Date**
#' @description This function allows you to scrape the standings from MLB for any date you choose.
#' @param date a date object
#' @param division One or more of AL East, AL Central, AL West,
#' AL Overall, NL East, NL Central, NL West, and NL Overall
#' @param from a logical indicating whether you want standings up to and
#' including the date (FALSE, default) or rather standings for games played
#' after the date
#' @return Returns a data frame of MLB standings
#' |col_name |types     |
#' |:--------|:---------|
#' |Tm       |character |
#' |W        |integer   |
#' |L        |integer   |
#' |W-L%     |numeric   |
#' |GB       |character |
#' |RS       |integer   |
#' |RA       |integer   |
#' |pythW-L% |numeric   |
#' @import rvest 
#' @importFrom lubridate day month year
#' @export
#' @examples \donttest{
#'   try(bref_standings_on_date(date = "2015-08-04", division = "AL East"))
#' }

bref_standings_on_date <- function(date, division, from = FALSE) {
  all_divisions <- c("AL East", "AL Central", "AL West", "AL Overall", "NL East", 
                     "NL Central", "NL West", "NL Overall")
  if(!(division %in% all_divisions)){
    stop("Please select a division in the following: \n'AL East', 'AL Central', 'AL West', 'AL Overall',\n'NL Central', 'NL West', 'NL Overall'")
  }
  
  url <- paste0("http://www.baseball-reference.com/boxes",
                "?year=", sprintf("%04i", lubridate::year(date)), "&month=",
                sprintf("%02i", lubridate::month(date)), "&day=", sprintf("%02i",
                                                                          lubridate::day(date)))
  
  tryCatch(
    expr = {
      html_doc <- url %>% 
        xml2::read_html()
      
      tables <- html_doc %>% 
        rvest::html_elements("table")
      min <- length(tables)
      max <- length(tables) - 15
      tables <- tables[min:max] %>% html_table
      #table_names <- html_doc %>% rvest::html_elements(".section_heading") %>% rvest::html_text() %>% gsub(pattern = "\\s+", replacement = " ") %>% gsub(pattern = " Division", replacement = "") %>% trimws(which = c("left")) %>% trimws(which = c("right")) %>% .[1:16]
      
      table_names <- c("NL Overall", "AL Overall", "NL West" , "NL Central", "NL East", "AL West", "AL Central", "AL East", "NL Overall", "AL Overall", "NL West" , "NL Central", "NL East", "AL West", "AL Central", "AL East")
      table_names[1:8] <- paste0(table_names[1:8], "_after_", date)     # Customizing list names for "After this Date" case
      table_names[9:16] <- paste0(table_names[9:16], "_up to_", date)   # Customizing list names for "From this Date" case
      
      names(tables) <- table_names
      
      after <- tables[1:8]
      
      current <- tables[9:16]
      
      if (from == FALSE) {
        div_date <- paste0(division, "_up to_", date)
        x <- current[div_date]
        x <- x[[1]]
      } else if (from != FALSE) {
        div_date <- paste0(division, "_after_", date)
        x <- after[div_date]
        x <- x[[1]]
      }
      x <- x %>%
        make_baseballr_data("MLB Standings on Date data from baseball-reference.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no standings on date data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(x)
}

#' @rdname standings_on_date_bref
#' @title **(legacy) Scrape MLB Standings on a Given Date**
#' @inheritParams bref_standings_on_date
#' @return Returns a data frame of MLB standings
#' @keywords legacy
#' @export
standings_on_date_bref <- bref_standings_on_date