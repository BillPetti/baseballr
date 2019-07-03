#' Retrieve draft pick information by year via the MLB api \url{http://statsapi.mlb.com/api/}
#'
#' @param year The year for which to return data
#' @importFrom dplyr bind_rows
#' @return Returns a data frame with information for every draft pick in every
#' round for that year
#' requested
#' @keywords MLB, sabermetrics
#' @export
#'
#' @examples
#' \dontrun{get_draft_mlb(2009)}

get_draft_mlb <- function(year) {

  api_call <- paste0("http://statsapi.mlb.com/api/v1/draft/", year)

  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)

  draft_table <- payload$drafts$rounds$picks

  draft_table <- draft_table %>%
    dplyr::bind_rows()

  draft_table_filled <- dplyr::bind_rows(column_structure_draft_mlb,
                                         draft_table) %>%
    janitor::clean_names()

  return(draft_table_filled)
}
