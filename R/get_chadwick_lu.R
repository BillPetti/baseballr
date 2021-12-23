#' @title Directly download the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#'
#' @export
#' @examples \donttest{
#'   get_chadwick_lu()
#' }

get_chadwick_lu <- function() {
  suppressWarnings(
    df <- csv_from_url("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv")
  )
  return(df)

}
