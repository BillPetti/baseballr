# Record the HTTP responses replayed by the tests/testthat/test-*_offline.R files.
#
# Run from the package root (needs network):
#   Rscript data-raw/http_fixtures/record_fixtures.R [candidates.txt]
#
# Each call in data-raw/http_fixtures/calls.txt (plus an optional candidates file)
# is run once against the live source. Every httr2 request is performed for real,
# with the request's own user agent and headers (so FanGraphs' okhttp UA and the
# Fox Origin header still apply), and the response is saved gzipped under
# tests/testthat/fixtures/http/, listed in index.json by URL. JSON arrays longer
# than `max_items` are trimmed (recursively) *before* the function sees them, so the
# recording run proves the function works on the trimmed fixture; a call ending in
# "# keep" is recorded whole. Calls that return data are written back to calls.txt;
# then run data-raw/http_fixtures/generate_tests.R.
# Pass season / date arguments explicitly so URLs don't drift with the calendar.
# Do not add stats.ncaa.org calls: that site IP-bans scrapers.

suppressMessages(devtools::load_all(".", quiet = TRUE))
`%||%` <- function(a, b) if (is.null(a)) b else a

fix_dir   <- file.path("tests", "testthat", "fixtures", "http")
calls_txt <- file.path("data-raw", "http_fixtures", "calls.txt")
max_items <- 5L
args <- commandArgs(trailingOnly = TRUE)

unlink(fix_dir, recursive = TRUE)
dir.create(fix_dir, recursive = TRUE, showWarnings = FALSE)

trim <- TRUE
trim_json <- function(x) {
  if (!is.list(x) || length(x) == 0) return(x)
  if (trim && is.null(names(x)) && length(x) > max_items) x <- x[seq_len(max_items)]
  x[] <- lapply(x, trim_json)
  x
}

index <- list()
saved_response <- function(url) {
  hit <- index[[url]]
  httr2::response(
    status_code = hit$status, url = url,
    headers = list(`Content-Type` = hit$type),
    body = memDecompress(readBin(file.path(fix_dir, hit$file), "raw", 50e6), "gzip")
  )
}

recorder <- function(req) {
  url <- req$url
  # Same URL twice in one run: replay what we saved, unless this is a "# keep" call
  # and the saved copy was trimmed -- then refetch it whole (it serves both).
  if (!is.null(index[[url]]) && !(isTRUE(index[[url]]$trimmed) && !trim)) {
    return(saved_response(url))
  }
  resp <- tryCatch(httr2::req_perform(req, mock = NULL),
                   httr2_http = function(e) e$resp, error = function(e) NULL)
  Sys.sleep(0.2)
  if (is.null(resp)) return(httr2::response(status_code = 599L, url = url))
  status <- httr2::resp_status(resp)
  type <- httr2::resp_header(resp, "content-type") %||% "application/octet-stream"
  body <- httr2::resp_body_raw(resp)
  if (status == 200L && grepl("json", type)) {
    j <- tryCatch(jsonlite::fromJSON(rawToChar(body), simplifyVector = FALSE),
                  error = function(e) NULL)
    if (!is.null(j)) {
      body <- charToRaw(enc2utf8(as.character(jsonlite::toJSON(
        trim_json(j), auto_unbox = TRUE, null = "null", na = "null", digits = NA
      ))))
    }
  }
  file <- index[[url]]$file %||% sprintf("%04d.gz", length(index) + 1L)
  writeBin(memCompress(body, "gzip"), file.path(fix_dir, file))
  index[[url]] <<- list(file = file, status = status, type = type, trimmed = trim)
  saved_response(url)
}

calls <- unique(c(readLines(calls_txt), if (length(args)) readLines(args[[1]])))
calls <- calls[nzchar(trimws(calls)) & !startsWith(trimws(calls), "#")]
ok <- logical(length(calls))
for (i in seq_along(calls)) {
  trim <- !grepl("#\\s*keep\\s*$", calls[[i]])
  out <- httr2::with_mocked_responses(recorder, tryCatch(
    suppressMessages(suppressWarnings(eval(parse(text = calls[[i]])))),
    error = function(e) e
  ))
  ok[[i]] <- !inherits(out, "error") && !is.null(out) &&
    ((is.data.frame(out) && nrow(out) > 0) || (!is.data.frame(out) && length(out) > 0))
  message(sprintf("[%d/%d] %s %s", i, length(calls), if (ok[[i]]) "ok  " else "FAIL", calls[[i]]))
}

# Keep only the fixtures the successful calls need: replay them and drop the rest.
used <- character()
for (call in calls[ok]) {
  httr2::with_mocked_responses(function(req) {
    used <<- union(used, req$url)
    saved_response(req$url)
  }, suppressMessages(suppressWarnings(eval(parse(text = call)))))
}
drop <- setdiff(names(index), used)
unlink(file.path(fix_dir, vapply(index[drop], `[[`, "", "file")))
index <- lapply(index[used], function(x) x[c("file", "status", "type")])

writeLines(calls[ok], calls_txt)
jsonlite::write_json(index[order(names(index))], file.path(fix_dir, "index.json"),
                     auto_unbox = TRUE, pretty = TRUE)
message(sum(ok), " of ", length(calls), " calls recorded; ", length(index), " responses kept")
