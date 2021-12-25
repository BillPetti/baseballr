#' @rdname mlb_draft 
#' @title **Retrieve draft pick information by year**
#' @param year The year for which to return data
#' @return Returns a data frame with information for every draft pick in every round for the year requested
#' @export
mlb_draft <- function(year) {

  api_call <- paste0("http://statsapi.mlb.com/api/v1/draft/", year)

  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)

  draft_table <- payload$drafts$rounds$picks

  draft_table <- draft_table %>%
    dplyr::bind_rows()

  column_structure_draft_mlb[1,] <- NA

  draft_table_filled <- column_structure_draft_mlb %>% 
    dplyr::bind_rows(draft_table) %>%
    dplyr::filter(!is.na(.data$bisPlayerId)) %>%
    janitor::clean_names()

  return(draft_table_filled)
}

#' @rdname mlb_draft
#' @export
get_draft_mlb <- mlb_draft
