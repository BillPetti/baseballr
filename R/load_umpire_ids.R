#' @rdname load_umpire_ids
#' @title **Download a data frame of all umpires and their mlbamids for games since 2008**
#' @return Function returns a tibble with the following columns: 
#' * id 
#' * position
#' * name
#' * game_pk
#' * game_date
#' @export
#' @examples \donttest{
#'   try(load_umpire_ids())
#' }

load_umpire_ids <- function() {
  tryCatch(
    expr = {
      df <- csv_from_url("https://app.box.com/shared/static/x20ahfe5e3a3y9sknz3g5y2ojbef3fzx.csv", encoding ="UTF-8")
      
      df <- df %>%
        make_baseballr_data("MLB Umpire IDs data from baseballr-data repository",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no Umpire IDs data available!"))
    },
    finally = {
    }
  )
  return(df)
}
#' @rdname get_umpire_ids_petti
#' @title **(legacy) Download a data frame of all umpires and their MLBAM IDs for games since 2008**
#' @return Function returns a tibble with the following columns: 
#' * id 
#' * position,
#' * name
#' * game_pk
#' * game_date
#' @keywords legacy
#' @export
get_umpire_ids_petti <- load_umpire_ids
