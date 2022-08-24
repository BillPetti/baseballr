.datatable.aware <- TRUE

#' @title **Progressively**
#'
#' @description This function helps add progress-reporting to any function - given function `f()` and progressor `p()`, it will return a new function that calls `f()` and then (on-exiting) will call `p()` after every iteration.
#'
#' This is inspired by purrr's `safely`, `quietly`, and `possibly` function decorators.
#'
#' @param f a function to add progressr functionality to.
#' @param p a progressor function as created by `progressr::progressor()`
#' @keywords internal
#'
#' @return a function that does the same as `f` but it calls `p()` after iteration.
progressively <- function(f, p = NULL){
  if(!is.null(p) && !inherits(p, "progressor")) stop("`p` must be a progressor function!")
  if(is.null(p)) p <- function(...) NULL
  force(f)
  
  function(...){
    on.exit(p("loading..."))
    f(...)
  }
  
}


#' Load rds/parquet/csv files from remote URL
#' @param url a vector of URLs to load into memory. If more than one URL provided, will row-bind them.
#' @param seasons a numeric vector of years that will be used to filter the dataframe's `season` column. If `TRUE` (default), does not filter.
#' @param baseballr TRUE to add baseballr_data classing and attributes.
#' @param ... named arguments that will be added as attributes to the data
#' @return a dataframe
load_from_url <- function(url, ..., seasons = TRUE, baseballr = FALSE){
  
  url <- as.character(url)
  
  if(length(url) == 1) {
    out <- loader(url)
    if(!isTRUE(seasons)) stopifnot(is.numeric(seasons))
    if(!isTRUE(seasons) && "season" %in% names(out)) out <- out[out$season %in% seasons]
  }
  
  if(length(url) > 1) {
    p <- NULL
    if (is_installed("progressr")) p <- progressr::progressor(along = url)
    out <- lapply(url, progressively(loader, p))
    out <- rbindlist_with_attrs(out)
  }
  
  if(baseballr) out <- make_baseballr_data(out,...)
  return(out)
}





loader <- function(url){
  switch(detect_filetype(url),
         "rds" = rds_from_url(url),
         "parquet" = parquet_from_url(url),
         "csv" = csv_from_url(url)
  )
}

detect_filetype <- function(url){
  tools::file_ext(gsub(x = url, pattern = ".gz$", replacement = ""))
}

#' @title
#' **Load .parquet file from a remote connection**
#' @description 
#' Retrieves a parquet file from URL.
#' @param url a character url
#' @keywords internal
#' @importFrom arrow read_parquet
#' @importFrom data.table data.table setDT
parquet_from_url <- function(url){
  rlang::check_installed("arrow")
  load <- try(curl::curl_fetch_memory(url), silent = TRUE)
  
  if (inherits(load, "try-error")) {
    cli::cli_warn("Failed to retrieve data from {.url {url}}")
    return(data.table::data.table())
  }
  
  content <- try(arrow::read_parquet(load$content), silent = TRUE)
  
  if (inherits(content, "try-error")) {
    cli::cli_warn("Failed to parse file with {.fun arrow::read_parquet()} from {.url {url}}")
    return(data.table::data.table())
  }
  
  data.table::setDT(content)
  return(content)
}

#' @title
#' **Load .csv / .csv.gz file from a remote connection**
#' @description
#' This is a thin wrapper on data.table::fread
#' @param ... passed to data.table::fread
#' @keywords internal
#' @importFrom data.table fread
csv_from_url <- function(...){
  data.table::fread(...)
}

#' @title
#' **Load .rds file from a remote connection**
#' @param url a character url
#' @keywords internal
#' @return a dataframe as created by [`readRDS()`]
#' @importFrom data.table data.table setDT
#' @import rvest
rds_from_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)
  
  if (inherits(load, "try-error")) {
    warning(paste0("Failed to readRDS from <", url, ">"), call. = FALSE)
    return(data.table::data.table())
  }
  
  data.table::setDT(load)
  return(load)
}


# check if a package is installed
is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)
# custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
custom_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
# The function `message_completed` to create the green "...completed" message
# only exists to hide the option `in_builder` in dots
message_completed <- function(x, in_builder = FALSE) {
  if (isFALSE(in_builder)) {
    str <- paste0(my_time(), " | ", x)
    cli::cli_alert_success("{{.field {str}}}")
  } else if (in_builder) {
    cli::cli_alert_success("{my_time()} | {x}")
  }
}

user_message <- function(x, type) {
  if (type == "done") {
    cli::cli_alert_success("{my_time()} | {x}")
  } else if (type == "todo") {
    cli::cli_ul("{my_time()} | {x}")
  } else if (type == "info") {
    cli::cli_alert_info("{my_time()} | {x}")
  } else if (type == "oops") {
    cli::cli_alert_danger("{my_time()} | {x}")
  }
}

my_time <- function() strftime(Sys.time(), format = "%H:%M:%S")

rule_header <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      right = paste0("baseballr version ", utils::packageVersion("baseballr")),
      width = getOption("width")
    )
  )
}

rule_footer <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      width = getOption("width")
    )
  )
}

#' @import rvest
check_status <- function(res) {
  x = httr::status_code(res)
  if(x != 200) stop("The API returned an error", call. = FALSE)
}

#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @import utils
utils::globalVariables(c("where"))


#' @importFrom Rcpp getRcppVersion
#' @importFrom RcppParallel defaultNumThreads
NULL

`%c%` <- function(x,y){
  ifelse(!is.na(x),x,y)
}


#' @title
#' **Most Recent NCAA Baseball Season**
#' @return An integer indicating the year of the most recent season of NCAA baseball
#' @export
most_recent_ncaa_baseball_season <- function() {
  ifelse(
    as.double(substr(Sys.Date(), 6, 7)) >= 3,
    as.double(substr(Sys.Date(), 1, 4)),
    as.double(substr(Sys.Date(), 1, 4)-1)
  )
}

#' @title
#' **Most Recent MLB Season**
#' @return An integer indicating the year of the most recent season of Major League Baseball
#' @export
most_recent_mlb_season <- function() {
  ifelse(
    as.double(substr(Sys.Date(), 6, 7)) >= 3,
    as.double(substr(Sys.Date(), 1, 4)),
    as.double(substr(Sys.Date(), 1, 4)-1)
  )
}
# Functions for custom class
# turn a data.frame into a tibble/baseballr_data
make_baseballr_data <- function(df,type,timestamp){
  out <- df %>%
    tidyr::as_tibble()
  
  class(out) <- c("baseballr_data","tbl_df","tbl","data.table","data.frame")
  attr(out,"baseballr_timestamp") <- timestamp
  attr(out,"baseballr_type") <- type
  return(out)
}

#' @export
#' @return A print method for tibbles indicating the update timestamp of the resource
#' @noRd
print.baseballr_data <- function(x,...) {
  cli::cli_rule(left = "{attr(x,'baseballr_type')}",right = "{.emph baseballr {utils::packageVersion('baseballr')}}")
  
  if(!is.null(attr(x,'baseballr_timestamp'))) {
    cli::cli_alert_info(
      "Data updated: {.field {format(attr(x,'baseballr_timestamp'), tz = Sys.timezone(), usetz = TRUE)}}"
    )
  }
  
  NextMethod(print,x)
  invisible(x)
}

# rbindlist but maintain attributes of last file
rbindlist_with_attrs <- function(dflist){
  
  baseballr_timestamp <- attr(dflist[[length(dflist)]], "baseballr_timestamp")
  baseballr_type <- attr(dflist[[length(dflist)]], "baseballr_type")
  out <- data.table::rbindlist(dflist, use.names = TRUE, fill = TRUE)
  attr(out,"baseballr_timestamp") <- baseballr_timestamp
  attr(out,"baseballr_type") <- baseballr_type
  out
}

#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @import utils
utils::globalVariables(c("where"))



#' @keywords internal
"_PACKAGE"

