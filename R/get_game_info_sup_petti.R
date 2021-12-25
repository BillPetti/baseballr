#' @title **Download a data frame of supplemental data about MLB games since 2008.**
#' @return Function returns a data frame with various columns, including: 
#' * game_pk
#' * game_date
#' * venue id 
#' * attendance
#' * game temperature
#' * wind speed 
#' * direction
#' * start time 
#' * end time
#' @export
#' @examples \donttest{
#'   get_game_info_sup_petti()
#' }

get_game_info_sup_petti <- function() {

  df <- csv_from_url("https://app.box.com/shared/static/qbtz8s1yxauamohcvrrjv2ba65v5p2d3.csv")

  return(df)

}
