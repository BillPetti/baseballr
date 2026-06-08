.datatable.aware <- TRUE

#' @title
#' **Retry an http request (with optional proxy) and rate-limit**
#' @description
#' `httr2`-based GET helper used by the NCAA (`stats.ncaa.org`) scrapers. Pass a
#' `proxy` through `...` to route the request through a proxy, e.g.
#' \preformatted{
#' ncaa_roster(team_id = 104, year = 2023,
#'   proxy = list(url = "http://HOST:PORT", username = "USER", password = "PASS"))
#' }
#' The `stats.ncaa.org` edge (Akamai) rate-limits and IP-bans aggressive
#' scrapers, so this helper sleeps 5 seconds after every request. Rotate proxies
#' across calls to spread load.
#' @param url Request url
#' @param ... currently unused (kept for backwards compatibility)
#' @param headers A named character vector of request headers. Defaults to
#'   `.ncaa_headers()` (a modern browser header set that passes the
#'   `stats.ncaa.org` Akamai edge).
#' @param proxy Optional proxy. Either a URL string (e.g.
#'   `"http://user:pass@host:port"`) or a list of arguments for
#'   [httr2::req_proxy()] (e.g. `list(url = "http://host:port", username = "u",
#'   password = "p")`). Defaults to `getOption("baseballr.proxy")`, so a proxy
#'   can be set once per session with
#'   `options(baseballr.proxy = list(url = ..., username = ..., password = ...))`.
#' @keywords internal
#' @return An [httr2::response] object.
request_with_proxy <- function(url, ..., headers = .ncaa_headers(),
                               proxy = getOption("baseballr.proxy")){
  req <- httr2::request(url) |>
    httr2::req_headers(!!!headers) |>
    httr2::req_timeout(15) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE)
  if (!is.null(proxy)) {
    req <- if (is.list(proxy)) {
      do.call(httr2::req_proxy, c(list(req), proxy))
    } else {
      httr2::req_proxy(req, url = proxy)
    }
  }
  resp <- httr2::req_perform(req)
  # Mandatory courtesy delay: stats.ncaa.org aggressively rate-limits / IP-bans.
  Sys.sleep(5)
  resp
}

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
  if (!is.null(p) && !inherits(p, "progressor")) stop("`p` must be a progressor function!")
  if (is.null(p)) p <- function(...) NULL
  force(f)
  
  function(...){
    on.exit(p("loading..."))
    f(...)
  }
  
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
    cli::cli_warn("Failed to readRDS from {.url {url}}")
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
  x = httr2::resp_status(res)
  if (x != 200) stop(glue::glue("The API returned an error, HTTP Response Code {x}"), call. = FALSE)
}

#' @title **Detect a stats.ncaa.org Akamai interstitial challenge**
#' @description
#' Some `stats.ncaa.org` routes (e.g. `/teams/{id}/season_to_date_stats`) are
#' gated behind Akamai Bot Manager. When challenged, the server returns HTTP 200
#' but the body is a short interstitial shell (a `bm-verify` meta-refresh, an
#' `akamai_validation.html` iframe, or a `request_quota_reached.html` notice)
#' rather than the data page. A static request cannot solve the challenge, so we
#' detect the shell and degrade gracefully instead of silently scraping a page
#' that has no data tables.
#' @param body A character scalar: the response body (HTML) as text.
#' @keywords internal
#' @return `TRUE` when `body` looks like an Akamai interstitial, else `FALSE`.
.ncaa_is_interstitial <- function(body) {
  if (length(body) != 1 || is.na(body)) return(TRUE)
  grepl("bm-verify|akamai_validation|request_quota_reached", body, ignore.case = TRUE)
}

#' @title **Resolve a stats.ncaa.org season-team id**
#' @description
#' `stats.ncaa.org` migrated team statistics from the franchise-centric
#' `/team/{team_id}/stats?...` query form to a season-team resource at
#' `/teams/{season_team_id}/season_to_date_stats`. The `season_team_id` is
#' year-specific and is **not** the franchise `team_id`. This helper resolves it
#' by reading the still-working roster page
#' (`/team/{team_id}/roster/{season_id}`) and extracting the "Team Statistics"
#' link it points to.
#' @param team_id Franchise team id used by the NCAA site.
#' @param season_id Season id from [load_ncaa_baseball_season_ids()] (the `id`
#'   column), i.e. the value the roster URL expects.
#' @param ... Passed through to [request_with_proxy()] (e.g. `proxy`).
#' @keywords internal
#' @return A length-1 character season-team id, or `NA_character_` if the roster
#'   page could not be read (e.g. challenged) or the link was not found.
#' @import rvest
.ncaa_resolve_season_team_id <- function(team_id, season_id, ...) {
  roster_url <- paste0("https://stats.ncaa.org/team/", team_id, "/roster/", season_id)
  resp <- request_with_proxy(url = roster_url, ...)
  body <- httr2::resp_body_string(resp)
  if (.ncaa_is_interstitial(body)) return(NA_character_)

  hrefs <- body |>
    xml2::read_html() |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  hit <- stringr::str_extract(hrefs, "teams/(\\d+)/season_to_date_stats", group = 1)
  hit <- hit[!is.na(hit)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @import utils
utils::globalVariables(c("where"))


#' @importFrom Rcpp getRcppVersion
#' @importFrom RcppParallel defaultNumThreads
NULL

`%c%` <- function(x, y) {
  ifelse(!is.na(x), x, y)
}


#' @title
#' **Most Recent NCAA Baseball Season**
#' @return An integer indicating the year of the most recent season of NCAA baseball
#' @export
most_recent_ncaa_baseball_season <- function() {
  ifelse(
    as.double(substr(Sys.Date(), 6, 7)) >= 1,
    as.double(substr(Sys.Date(), 1, 4)),
    as.double(substr(Sys.Date(), 1, 4)) - 1
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
    as.double(substr(Sys.Date(), 1, 4)) - 1
  )
}
# Functions for custom class
# turn a data.frame into a tibble/baseballr_data
make_baseballr_data <- function(df, type, timestamp){
  out <- df |>
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
  
  if (!is.null(attr(x,'baseballr_timestamp'))) {
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

