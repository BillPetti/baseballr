#' Directly download the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#'
#' @keywords MLB, sabermetrics
#' @importFrom data.table fread
#' @export
#' @examples
#' \dontrun{get_chadwick_lu()}

get_chadwick_lu <- function() {

    df <- data.table::fread("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv")

  return(df)

}
