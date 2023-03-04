#' @title
#' **Load cleaned NCAA baseball play-by-play data from the baseballr data repo**
#' @rdname load_ncaa_baseball_pbp
#' @description helper that loads multiple seasons from the data repo either into memory
#' or writes it into a db using some forwarded arguments in the dots
#' @param seasons A vector of 4-digit years associated with given NCAA college baseball seasons. (Min: 2022)
#' @param ... Additional arguments passed to an underlying function that writes
#' the season data into a database.
#' @param dbConnection A `DBIConnection` object, as returned by
#' @param tablename The name of the schedule data table within the database
#' @return Returns a tibble
#' @export
#' @examples \donttest{
#'   load_ncaa_baseball_pbp(seasons = 2022)
#' }
load_ncaa_baseball_pbp <- function(seasons = most_recent_ncaa_baseball_season(), ...,
                              dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  dots <- rlang::dots_list(...)
  
  loader <- rds_from_url
  
  if (!is.null(dbConnection) && !is.null(tablename)) in_db <- TRUE else in_db <- FALSE
  if (isTRUE(seasons)) seasons <- 2012:most_recent_ncaa_baseball_season()
  
  stopifnot(is.numeric(seasons),
            seasons >= 2012,
            seasons <= most_recent_ncaa_baseball_season())
  
  urls <- paste0("https://raw.githubusercontent.com/sportsdataverse/baseballr-data/main/ncaa/pbp/rds/ncaa_baseball_pbp_",seasons,".rds")
  
  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = seasons)
  
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
