#' @rdname load_umpire_ids
#' @title **Download a data frame of all umpires and their mlbamids for games since 2008**
#' @return Function returns a data frame with the following columns: 
#' * id 
#' * position
#' * name
#' * game_pk
#' * game_date
#' @export
#' @examples \donttest{
#'   load_umpire_ids()
#' }

load_umpire_ids <- function() {

  df <- csv_from_url("https://app.box.com/shared/static/x20ahfe5e3a3y9sknz3g5y2ojbef3fzx.csv")

  return(df)

}
#' @rdname get_umpire_ids_petti
#' @title **(legacy) Download a data frame of all umpires and their MLBAM IDs for games since 2008**
#' @return Function returns a data frame with the following columns: 
#' * id 
#' * position,
#' * name
#' * game_pk
#' * game_date
#' @keywords legacy
#' @export
get_umpire_ids_petti <- load_umpire_ids