teams_data <- baseballr::teams_lu_table

teams <- teams_data [1:30,]%>% 
  dplyr::mutate(
    bref_abbreviation = dplyr::case_when(
      .data$name == "Chicago White Sox" ~ "CHW",
      .data$name == "Kansas City Royals" ~ "KCR", 
      .data$name == "Los Angeles Angels" ~ "ANA",
      .data$name == "Miami Marlins" ~ "MIA",
      .data$name == "San Diego Padres" ~ "SDP",
      .data$name == "San Francisco Giants" ~ "SFG",
      .data$name == "Tampa Bay Rays" ~ "TBR",
      .data$name == "Washington Nationals" ~ "WSN",
      TRUE ~ .data$abbreviation
      
    )
  )
# teams_data <- teams_data %>% dplyr::select(-.data$bref_abbreviation)
teams_lu_table <- teams_data %>% dplyr::left_join(teams %>% dplyr::select(.data$name,.data$bref_abbreviation), by=c("name"))
usethis::use_data(teams_lu_table,overwrite = TRUE)
