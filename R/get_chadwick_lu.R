#' Directly download the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#'
#' @keywords MLB, sabermetrics
#' @importFrom vroom vroom
#' @export
#' @examples
#' \dontrun{get_chadwick_lu()}

get_chadwick_lu <- function() {

  df <- vroom::vroom("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv",
                     delim = ',')

  return(df)

}
