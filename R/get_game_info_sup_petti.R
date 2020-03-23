#' Download a data frame of supplemental data about MLB games since 2008.
#'
#' @keywords MLB, sabermetrics
#' @importFrom vroom vroom
#' @return Function returns a data frame with various columns, including: game_pk,
#' game_date, venue id, attendance, game temperature, wind speed and direction,
#' and start and end time,
#' @export
#' @examples
#' \dontrun{get_game_info_sup_petti()}

get_game_info_sup_petti <- function() {

  df <- vroom::vroom("https://app.box.com/shared/static/qbtz8s1yxauamohcvrrjv2ba65v5p2d3.csv",
                     delim = ',')

  return(df)

}
