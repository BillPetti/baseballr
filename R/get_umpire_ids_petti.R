#' Download a data frame of all umpires and their mlbamids for games since 2008
#'
#' @return Function returns a data frame with the following columns: id, position,
#' name, game_pk, game_date
#' @export
#' @examples
#' \donttest{get_umpire_ids_petti()}

get_umpire_ids_petti <- function() {

  df <- csv_from_url("https://app.box.com/shared/static/x20ahfe5e3a3y9sknz3g5y2ojbef3fzx.csv")

  return(df)

}
