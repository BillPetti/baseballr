#' @rdname load_ncaa_baseball_season_ids
#' @title **Load cleaned NCAA men's college baseball season IDs from the baseballr data repo**
#' @description helper that loads multiple seasons of season IDs from the data repo either into memory
#' or writes it into a db using some forwarded arguments in the dots
#'
#' @param ... Additional arguments passed to an underlying function that writes
#' the season data into a database.
#' @param dbConnection A `DBIConnection` object, as returned by
#' @param tablename The name of the data table within the database
#' @return Returns a tibble
#' @export
#' @examples \donttest{
#'   load_ncaa_baseball_season_ids()
#' }
load_ncaa_baseball_season_ids <- function(...,
                              dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  dots <- rlang::dots_list(...)
  
  loader <- rds_from_url
  
  if (!is.null(dbConnection) && !is.null(tablename)) in_db <- TRUE else in_db <- FALSE
 
  urls <- paste0("https://raw.githubusercontent.com/sportsdataverse/baseballr-data/main/ncaa/seasons_info/ncaa_season_id_lu.rds")
  
  p <- NULL

  out <- lapply(urls, progressively(loader, p))
  out <- rbindlist_with_attrs(out)
  if (in_db) {
    DBI::dbWriteTable(dbConnection, tablename, out, append = TRUE)
    out <- NULL
  } else {
    class(out) <- c("baseballr_data","tbl_df","tbl","data.table","data.frame")
  }
  out
}
