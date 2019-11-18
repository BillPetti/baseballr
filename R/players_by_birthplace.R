#' Scrape MLB Players by Birth Place
#'
#' This function allows to scrape a data frame with baseball players that have played in MLB based on their birthplace
#' 
#' @param place a string argument with the name of the birth place (e.g. Country, US State)
#' 'country' is a case sensitive argument, which needs to be entered as countries' names written in https://www.baseball-reference.com/bio/
#' @keywords MLB, players, country
#' @importFrom xml2 read_html html_find_all 
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter arrange mutate
#' @importFrom lubridate myd
#' @importFrom rvest html_table
#' @importFrom eeptools age_calc
#' @importFrom tidyr separate
#' @importFrom readr write_csv
#' @export players_by_birthplace
#' @examples
#' players_by_birthplace("Venezuela")
#' players_by_birthplace("Puerto-Rico")
#' players_by_birthplace("Dominican-Republic")
#' players_by_birthplace("AZ")
#' #' \dontrun{
#' players_by_birthplace("Puerto Rico")
#' }

#' The argument of this function (place) is a case sensitive string, to be entered as places' names written in https://www.baseball-reference.com/bio/
#' For country names composed by more than one word (e.g. Puerto Rico), the space between the words must be entered with  a dash between the words (e.g. "Puerto-Rico)
#' Players from the United States need to be retreive by State. For that case, the "place" argument must be entered as the State's Abbreviation (e.g. "AZ" for Arizona) 

players_by_birthplace <- function(place) {
  
  urlbbref <- xml2::read_html(paste0("https://www.baseball-reference.com/bio/", place, "_born.shtml"))
  
  # First table is in the markup
  players <- xml2::xml_find_all(urlbbref, "//table") %>%
    rvest::html_table()
  players <- tibble::as_tibble(players[[1]])
  
  # Cleaning the data
  names(players)[c(1, 3, 12:13)] <- c("Rank", "Years", "X2B", "X3B")
  players <- players %>% 
    dplyr::filter(Name != "Totals")
  players$Birthdate <- lubridate::mdy(players$Birthdate)
  players$Debut <- lubridate::mdy(players$Debut)
  
  # Ordering the data frame by Debut Date (ascending) and adding some addituional variables
  players <- players %>% 
    dplyr::arrange(Debut) %>% 
    dplyr::mutate(Rank = row_number(),
                  age_at_debut = signif(eeptools::age_calc(Birthdate,         # Player's age at MLB Debut
                                                    Debut,
                                                    units = "years"),
                                        digits = 4),
                  year_at_debut = lubridate::year(Debut),                     # Year of player MLB Debut
                  month_at_debut = lubridate::month(Debut)) %>%               # Month of player MLB Debut 
    tidyr::separate(Birthplace,                                               # Splits Birthplace in City and State
                    into = c("City", "State"),
                    sep = ", ",
                    extra = "merge")

    print(paste0("Data frame with ", nrow(players), " player(s) from ", country, " has been saved into the Environment"))
    players_place <<- players                                                 # Save the data frame to the environment
    
}