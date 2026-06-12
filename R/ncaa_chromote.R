# ---------------------------------------------------------------------------
# Browser-based fallback for stats.ncaa.org (Akamai bot protection)
# ---------------------------------------------------------------------------
#
# stats.ncaa.org sits behind Akamai Bot Manager, which (as of mid-2026) returns
# HTTP 403 "Access Denied" to httr2/curl requests no matter how closely the
# request headers mimic a browser -- the block is on the client's TLS/sensor
# fingerprint, not the headers. It ALSO blocks a vanilla headless Chrome: Akamai
# fingerprints the `HeadlessChrome` user-agent and the `navigator.webdriver`
# automation flag. A *stealth* headless Chrome -- a real Chrome UA plus the
# webdriver flag hidden -- passes the edge and returns the real page.
#
# This module provides that stealth fallback. It is invoked by
# `request_with_proxy()` ONLY when a direct stats.ncaa.org request 403s, so the
# fast httr2 path is still used whenever Akamai allows it (and resumes
# automatically if Akamai ever relaxes). `chromote` + Google Chrome are an
# optional (Suggests) dependency; when absent, the NCAA scrapers degrade with a
# clear, actionable message instead of an opaque parse error.

# A current desktop-Chrome UA. NOT "HeadlessChrome" -- Akamai blocks that token.
.ncaa_chrome_ua <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
  "(KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36"
)

# Stealth patches applied to every new document before page scripts run, so
# Akamai's sensor sees a genuine browser: hide the webdriver flag and ensure
# `window.chrome` exists.
.ncaa_stealth_js <- paste0(
  "Object.defineProperty(navigator, 'webdriver', {get: () => undefined});",
  "window.chrome = window.chrome || {runtime: {}};"
)

# Package-internal cache so the headless-Chrome startup cost is paid at most
# once per session; the session is reused across the many requests an
# `ncaa_schedule_info(pbp_links = TRUE)` call makes.
.ncaa_chromote_cache <- new.env(parent = emptyenv())

# Markers Akamai serves on a blocked / bot-challenge page. Two flavours are seen
# from stats.ncaa.org: a hard "Access Denied" (HTTP 403) and a *soft*
# interstitial that returns HTTP 200 with a tiny body that JS-redirects through a
# `bm-verify` token (it posts to `/_sec/verify` and reloads). Either means the
# real content was not served, so both must trigger the browser fallback and
# neither may be accepted as a successful render.
.ncaa_block_markers <- paste(
  "Access Denied", "Reference&#32;#", "Reference #", "Pardon Our Interruption",
  "bm-verify", "akamai_validation", "triggerInterstitialChallenge", "/_sec/verify",
  sep = "|"
)

#' Does this response body look like an Akamai block / bot-challenge page?
#' @keywords internal
#' @noRd
.ncaa_is_blocked <- function(body) {
  !is.null(body) && nzchar(body) && grepl(.ncaa_block_markers, body)
}

#' Return a cached, stealth-configured chromote session, creating one if needed.
#' @keywords internal
#' @noRd
.ncaa_browser <- function() {
  sess <- .ncaa_chromote_cache$session
  alive <- !is.null(sess) && tryCatch(
    {
      sess$Runtime$evaluate("1")
      TRUE
    },
    error = function(e) FALSE
  )
  if (!alive) {
    sess <- chromote::ChromoteSession$new()
    sess$Network$setUserAgentOverride(userAgent = .ncaa_chrome_ua)
    sess$Page$addScriptToEvaluateOnNewDocument(source = .ncaa_stealth_js)
    .ncaa_chromote_cache$session <- sess
  }
  sess
}

#' Close the cached NCAA browser session (frees the headless Chrome process).
#' @keywords internal
#' @noRd
.ncaa_browser_close <- function() {
  sess <- .ncaa_chromote_cache$session
  if (!is.null(sess)) try(sess$close(), silent = TRUE)
  .ncaa_chromote_cache$session <- NULL
  invisible(NULL)
}

#' Fetch a stats.ncaa.org page through a stealth headless Chrome.
#'
#' @param url Page url.
#' @param wait Maximum seconds to wait for Akamai's sensor to resolve and the
#'   real content to render. Defaults to 30.
#' @return The rendered page HTML as a single character string, or `NULL` when
#'   `chromote`/Chrome is unavailable or the page could not be retrieved.
#' @keywords internal
#' @noRd
.ncaa_chromote_fetch <- function(url, wait = 30) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    cli::cli_alert_danger(paste0(
      "{Sys.time()}: stats.ncaa.org returned HTTP 403 (Akamai bot protection). ",
      "Install the optional {{chromote}} package and Google Chrome to enable the ",
      "browser-based fallback: install.packages(\"chromote\")."
    ))
    return(NULL)
  }
  # A successfully rendered stats.ncaa.org page is past the challenge (no block
  # markers) and table-based -- every page this scraper consumes (schedule,
  # roster, game logs, stats, box score) contains a <table>. Requiring one
  # rejects the tiny Akamai interstitial AND the occasional partial render that
  # lands before the real content paints.
  ok <- function(h) {
    !is.null(h) && nchar(h) > 3000 && !grepl(.ncaa_block_markers, h) &&
      grepl("<table", h, ignore.case = TRUE)
  }
  read_dom <- function(sess) {
    ready <- tryCatch(
      sess$Runtime$evaluate("document.readyState")$result$value,
      error = function(e) ""
    )
    html <- tryCatch(
      sess$Runtime$evaluate("document.documentElement.outerHTML")$result$value,
      error = function(e) ""
    )
    list(ready = ready, html = if (is.null(html)) "" else html)
  }
  tryCatch(
    {
      sess <- .ncaa_browser()
      sess$Page$navigate(url, wait_ = FALSE)
      deadline <- Sys.time() + wait
      # Akamai's interstitial flow is: tiny challenge page -> JS posts to
      # /_sec/verify -> reload -> real content. Poll until the document is
      # `complete` and the real (table-bearing) page has rendered, then take one
      # short settle read to capture any final reflow.
      html <- ""
      repeat {
        Sys.sleep(1.5)
        st <- read_dom(sess)
        html <- st$html
        if (identical(st$ready, "complete") && ok(html)) {
          Sys.sleep(1.5)
          html <- read_dom(sess)$html
          if (ok(html)) break
        }
        if (Sys.time() > deadline) break
      }
      if (ok(html)) html else NULL
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: NCAA browser fallback failed: {conditionMessage(e)}")
      # a dead/poisoned session shouldn't persist into the next call
      .ncaa_browser_close()
      NULL
    }
  )
}
