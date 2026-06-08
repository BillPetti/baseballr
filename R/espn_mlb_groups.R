# espn_mlb_groups.R
# espn_mlb_conferences() -- mirrors espn_mbb_conferences() using the MLB
# scoreboard-conferences endpoint.

#' **Get ESPN MLB Conferences**
#' @name espn_mlb_conferences
NULL
#' @title
#' **Get ESPN MLB Conferences**
#' @rdname espn_mlb_conferences
#' @author Saiem Gilani
#' @return A `baseballr_data` tibble with one row per conference:
#'
#'    |col_name              |types     |description                                  |
#'    |:---------------------|:---------|:--------------------------------------------|
#'    |group_id              |integer   |Group identifier (e.g. conference group_id). |
#'    |conference_short_name |character |Conference short name (e.g. 'ACC').          |
#'    |conference_uid        |character |ESPN universal conference identifier.        |
#'    |conference_name       |character |Full conference name.                        |
#'    |conference_logo       |character |Logo image URL for conference.               |
#'    |parent_group_id       |integer   |Unique identifier for parent group.          |
#'    |conference_id         |integer   |Conference identifier.                       |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom dplyr select filter mutate rename
#' @import rvest
#' @export
#' @keywords MLB Conferences
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   try(espn_mlb_conferences())
#' }
espn_mlb_conferences <- function() {
  .args <- .capture_args()
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  conferences <- .empty_baseballr_data(
    "ESPN MLB Conferences Information from ESPN.com",
    cols = c(
      "group_id", "conference_short_name", "conference_uid",
      "conference_name", "conference_logo", "parent_group_id",
      "conference_id"
    )
  )

  tryCatch(
    expr = {
      play_base_url <- paste0(
        "https://site.api.espn.com/apis/site/v2/sports/baseball/mlb",
        "/scoreboard/conferences?seasontype=2"
      )

      res <- .retry_request(play_base_url)

      check_status(res)

      resp <- res %>%
        .resp_text()

      conferences <- jsonlite::fromJSON(resp)[["conferences"]] %>%
        dplyr::select(-dplyr::any_of("subGroups")) %>%
        janitor::clean_names() %>%
        dplyr::filter(!(.data$group_id %in% c(0, 50))) %>%
        dplyr::mutate(
          group_id        = as.integer(.data$group_id),
          conference_id   = .data$group_id,
          parent_group_id = as.integer(.data$parent_group_id)
        ) %>%
        dplyr::rename(dplyr::any_of(c(
          "conference_short_name" = "short_name",
          "conference_uid"        = "uid",
          "conference_name"       = "name",
          "conference_logo"       = "logo"
        ))) %>%
        make_baseballr_data(
          "ESPN MLB Conferences Information from ESPN.com",
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = "Invalid arguments or no conferences info available!",
      args = .args
    ),
    warning = function(w) .report_api_warning(w, args = .args),
    finally = {}
  )
  return(conferences)
}
