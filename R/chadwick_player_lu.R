#' @rdname chadwick_player_lu
#' @title **Download the Chadwick Bureau's public register of baseball players**
#' @return A data frame of baseball players and the various IDs associated with them in different systems of record.
#' @export
#' @examples \donttest{
#'   chadwick_player_lu()
#' }
chadwick_player_lu <- function() {
  suppressWarnings(
    df <- csv_from_url("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv")
  )
  return(df)
}
#' @rdname chadwick_player_lu
#' @export
get_chadwick_lu <-  chadwick_player_lu
