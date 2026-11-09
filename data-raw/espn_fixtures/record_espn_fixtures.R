# Record the ESPN responses replayed by tests/testthat/test-espn_offline.R.
#
# Run from the package root (needs network; ESPN tolerates this pace):
#   Rscript data-raw/espn_fixtures/record_espn_fixtures.R [candidates.txt]
#
# Each call is run once against the live API. Every HTTP response is saved, gzipped,
# under tests/testthat/fixtures/espn/ and listed in index.json by URL. JSON arrays
# longer than `max_items` are trimmed (recursively) *before* the function sees them,
# so the recording run proves the function works on the trimmed fixture, and the
# fixtures stay small enough to ship. Calls that return data are written to
# data-raw/espn_fixtures/calls.txt, which test-espn_offline.R is generated from.
# Pass a candidates file to try extra calls; known-good calls are always re-run.
# Season arguments are always explicit so the URLs don't drift with the calendar.

suppressMessages(devtools::load_all(".", quiet = TRUE))

fix_dir   <- file.path("tests", "testthat", "fixtures", "espn")
calls_txt <- file.path("data-raw", "espn_fixtures", "calls.txt")
max_items <- 5L
args <- commandArgs(trailingOnly = TRUE)
`%||%` <- function(a, b) if (is.null(a)) b else a

unlink(fix_dir, recursive = TRUE)
dir.create(fix_dir, recursive = TRUE, showWarnings = FALSE)

# A call ending in "# keep" is recorded untrimmed (parsers that need every
# play / every division, e.g. espn_mlb_pbp(), espn_mlb_standings()).
trim <- TRUE
trim_json <- function(x) {
  if (!is.list(x) || length(x) == 0) return(x)
  if (trim && is.null(names(x)) && length(x) > max_items) x <- x[seq_len(max_items)]
  x[] <- lapply(x, trim_json)
  x
}

index <- list()
recorder <- function(req) {
  url <- req$url
  # Same URL twice in one run: replay what we saved, unless this is a "# keep" call
  # and the saved copy was trimmed -- then refetch it whole (it serves both).
  if (!is.null(index[[url]]) && !(isTRUE(index[[url]]$trimmed) && !trim)) {
    hit <- index[[url]]
    return(httr2::response(status_code = hit$status, url = url,
      headers = list(`Content-Type` = "application/json"),
      body = memDecompress(readBin(file.path(fix_dir, hit$file), "raw", 50e6), "gzip")))
  }
  h <- curl::new_handle(timeout = 60, useragent = "baseballr-fixture-recorder")
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  Sys.sleep(0.15)
  if (is.null(res)) return(httr2::response(status_code = 599L, url = url))
  body <- res$content
  if (res$status_code == 200L) {
    j <- tryCatch(jsonlite::fromJSON(rawToChar(body), simplifyVector = FALSE),
                  error = function(e) NULL)
    if (!is.null(j)) {
      body <- charToRaw(enc2utf8(as.character(jsonlite::toJSON(
        trim_json(j), auto_unbox = TRUE, null = "null", na = "null", digits = NA
      ))))
    }
  }
  file <- index[[url]]$file %||% sprintf("%04d.json.gz", length(index) + 1L)
  writeBin(memCompress(body, "gzip"), file.path(fix_dir, file))
  index[[url]] <<- list(file = file, status = res$status_code, trimmed = trim)
  httr2::response(
    status_code = res$status_code, url = url,
    headers = list(`Content-Type` = "application/json"), body = body
  )
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
    hit <- index[[req$url]]
    httr2::response(status_code = hit$status, url = req$url,
      headers = list(`Content-Type` = "application/json"),
      body = memDecompress(readBin(file.path(fix_dir, hit$file), "raw", 50e6), "gzip"))
  }, suppressMessages(suppressWarnings(eval(parse(text = call)))))
}
drop <- setdiff(names(index), used)
unlink(file.path(fix_dir, vapply(index[drop], `[[`, "", "file")))
index <- index[used]

writeLines(calls[ok], calls_txt)
index <- lapply(index, function(x) x[c("file", "status")])
jsonlite::write_json(index[order(names(index))], file.path(fix_dir, "index.json"),
                     auto_unbox = TRUE, pretty = TRUE)
message(sum(ok), " of ", length(calls), " calls recorded; ", length(index), " responses kept")
