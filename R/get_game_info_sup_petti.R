#' Download a data frame of supplemental data about MLB games since 2008.
#'
#' @keywords MLB, sabermetrics
#' @importFrom data.table fread
#' @return Function returns a data frame with various columns, including: game_pk,
#' game_date, venue id, attendance, game temperature, wind speed and direction,
#' and start and end time,
#' @export
#' @examples
#' \dontrun{get_game_info_sup_petti()}

get_game_info_sup_petti <- function() {

  df <- data.table::fread("https://app.box.com/shared/static/qbtz8s1yxauamohcvrrjv2ba65v5p2d3.csv")

  return(df)

}
