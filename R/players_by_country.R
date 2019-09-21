

# Country argument is a case sensitive string, to be entered as countries' names written in https://www.baseball-reference.com/bio/
# For country names composed by more than one word (e.g. Puerto Rico), the space between the words must be replaced by a dash (e.g. "Puerto-Rico)
# Players from United States need to be retreive by State. For that case, the "country" argument must be entered as the State's Abbreviation (e.g. "AZ" for Arizona) 

players_by_country <- function(country) {
  
  urlbbref <- xml2::read_html(paste0("https://www.baseball-reference.com/bio/", country, "_born.shtml"))
  
  # First table is in the markup
  players <- xml2::xml_find_all(urlbbref, "//table") %>%
    html_table
  players <- tibble::as_tibble(players[[1]])
  
  # Cleaning the data
  names(players)[c(1, 3, 12:13)] <- c("Rank", "Years", "X2B", "X3B")
  players <- players %>% 
    # separate(Birthplace, c("City", "State"), sep = ", ", extra = "merge") %>% 
    dplyr::filter(Name != "Totals")
  players$Birthdate <- lubridate::mdy(players$Birthdate)
  players$Debut <- lubridate::mdy(players$Debut)
  
  # Ordering the data by Debut Date (ascending)
  players <- players %>% 
    dplyr::arrange(Debut) %>% 
    dplyr::mutate(Rank = row_number())

    print(paste0("Data frame with ", nrow(players), " player(s) from ", country, " has been saved into the Environment"))
    country_players <<- players
  
}

