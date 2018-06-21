#' Process Baseball Savant CSV payload
#'
#' This is a helper function for all scrape_statcast_savant functions. 
#' The function processes the initial csv payload acquired from 
#' baseballsavant to ensure consistency in formattting across downloads
#' @param payload payload from a Baseball Savant request, e.g. 
#' from \code{\link[readr]{read_csv}}
#' @keywords MLB, sabermetrics, Statcast
#' @importFrom dplyr mutate_
#' @export
#' @examples
#' \dontrun{
#' process_statcast_payload(payload)
#' }

process_statcast_payload <- function(payload) {

  # Clean up formatting.
  payload <- payload %>%
    dplyr::mutate_(
      barrel = ~ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 11 & launch_speed + launch_angle >= 124, 1, 0)
  )

  return(payload)

}
