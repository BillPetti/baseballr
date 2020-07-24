#' Download a data frame of all umpires and their mlbamids for games since 2008
#'
#' @keywords MLB, sabermetrics
#' @importFrom vroom vroom
#' @return Function returns a data frame with the following columns: id, position,
#' name, game_pk, game_date
#' @export
#' @examples
#' \dontrun{get_umpire_ids_petti()}

get_umpire_ids_petti <- function() {

  df <- vroom::vroom("https://app.box.com/shared/static/x20ahfe5e3a3y9sknz3g5y2ojbef3fzx.csv",
                      delim = ',')

  return(df)

}
