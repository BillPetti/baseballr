#' Directly download the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#'
#' @importFrom vroom vroom
#' @export
#' @examples
#' \donttest{get_chadwick_lu()}

get_chadwick_lu <- function() {
  suppressWarnings(
    df <- vroom::vroom("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv",
                       delim = ',')
  )
  return(df)

}
