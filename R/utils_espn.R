# Internal HTTP + error-reporting helpers shared by the ESPN MLB wrappers
# (`espn_mlb_*()`). Ported from the proven hoopR / wehoop ESPN layer and
# retargeted to baseball/mlb. These mirror the conventions documented in
# CLAUDE.md (return-value initialization, cli messaging, column-drift
# resilience) and are intentionally kept separate from the MLB Stats API /
# FanGraphs / NCAA helpers in `utils_mlb_stats.R` and `utils.R`.

#' @importFrom rlang %||%
NULL

# ---------------------------------------------------------------------------
# League validation
# ---------------------------------------------------------------------------

# ESPN models baseball as sport = "baseball" with a league slug. baseballr
# wraps Major League Baseball (`mlb`); the validator is kept extensible so a
# future college-baseball family can reuse the same helpers.
.espn_baseball_validate_league <- function(league) {
  valid <- c("mlb")
  if (!league %in% valid) {
    cli::cli_abort(
      "league must be one of {.val {valid}}, not {.val {league}}."
    )
  }
}

# Alias retained so ported helpers that call the `_cat` variant resolve to the
# same single source of truth.
.espn_baseball_validate_league_cat <- .espn_baseball_validate_league

# ---------------------------------------------------------------------------
# HTTP layer
# ---------------------------------------------------------------------------

#' Perform an HTTP GET request with retry logic (ESPN)
#'
#' Thin `httr2` wrapper used by the ESPN MLB wrappers. Supports optional query
#' parameters, custom headers, and proxy routing. Proxy resolution order:
#'   1. `proxy` argument (caller-supplied, highest precedence).
#'   2. `getOption("baseballr.proxy")` (session-level fallback -- set once with
#'      `options(baseballr.proxy = ...)`; ESPN wrappers call `.retry_request()`
#'      directly without `...`, so per-call overrides are not threaded through).
#'   3. `http_proxy` / `https_proxy` / `no_proxy` env vars (read by libcurl
#'      automatically when no explicit proxy is supplied).
#'
#' The `proxy` value accepts a single URL string (`"http://host:port"`, passed
#' to `httr2::req_proxy(url = )`) or a named list spread as keyword args into
#' `httr2::req_proxy()` (`url`, `port`, `username`, `password`, `auth`).
#' @param url The URL to request.
#' @param params Named list of query parameters (default: empty list).
#' @param headers Named character vector of headers (default: NULL).
#' @param timeout Timeout in seconds (default: 60).
#' @param proxy Optional proxy (see above). Defaults to
#'   `getOption("baseballr.proxy")`.
#' @keywords internal
#' @return An [httr2::response] object.
.retry_request <- function(url, params = list(), headers = NULL, timeout = 60,
                           proxy = NULL) {
  req <- httr2::request(url)
  if (length(params) > 0) {
    req <- req |> httr2::req_url_query(!!!params)
  }
  if (!is.null(headers)) {
    req <- req |> httr2::req_headers(!!!as.list(headers))
  }
  if (is.null(proxy)) {
    proxy <- getOption("baseballr.proxy", default = NULL)
  }
  if (!is.null(proxy)) {
    req <- if (is.list(proxy)) {
      do.call(httr2::req_proxy, c(list(req = req), proxy))
    } else {
      httr2::req_proxy(req, url = proxy)
    }
  }
  req |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
}

#' Extract an httr2 response body as UTF-8 text
#' @param resp An httr2 response object.
#' @keywords internal
#' @return Character string of the response body.
.resp_text <- function(resp) {
  httr2::resp_body_string(resp, encoding = "UTF-8")
}

# ---------------------------------------------------------------------------
# Error / warning reporting
# ---------------------------------------------------------------------------

#' Minimal brace-template interpolator
#'
#' Replaces `{expr}` tokens in `template` by evaluating `expr` in `envir`.
#' Used in `.report_api_error()` / `.report_api_warning()` so callers can write
#' hints like `"No data for {game_id}"` and have `{game_id}` resolve against the
#' function's frame at the call-site. Per-token failures leave the literal
#' `{expr}` in place rather than erroring.
#' @param template character(1).
#' @param envir environment to evaluate expressions against.
#' @keywords internal
#' @return character(1).
.interp_braces <- function(template, envir = parent.frame()) {
  if (length(template) != 1L || !is.character(template) || is.na(template)) {
    return(as.character(template))
  }
  m <- gregexpr("\\{([^{}]+)\\}", template, perl = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) return(template)
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  out <- character(0)
  pos <- 1L
  for (i in seq_along(starts)) {
    s <- starts[i]; l <- lens[i]
    if (s > pos) out <- c(out, substr(template, pos, s - 1L))
    expr <- substr(template, s + 1L, s + l - 2L)
    val <- tryCatch(
      paste(as.character(eval(parse(text = expr), envir = envir)), collapse = ""),
      error = function(.e) substr(template, s, s + l - 1L)
    )
    out <- c(out, val)
    pos <- s + l
  }
  if (pos <= nchar(template)) out <- c(out, substr(template, pos, nchar(template)))
  paste(out, collapse = "")
}

#' Report an API-call error with full context
#'
#' Standardizes the message every ESPN MLB wrapper emits inside its
#' `tryCatch(error = ...)` block: a timestamped friendly hint
#' (brace-interpolated against the caller env), a dump of the call's arguments,
#' and the actual error message. Wrappers capture their formals once near the
#' top with `.args <- mget(setdiff(names(formals()), "..."))` (or
#' `.args <- .capture_args()` for arg-less wrappers).
#' @param e error condition.
#' @param hint character. Friendly message with optional `{name}` tokens that
#'   resolve against the caller's environment. Defaults to "Request failed".
#' @param args optional named list of caller arguments to dump.
#' @keywords internal
#' @return Invisibly `NULL`. Called for its side effects.
.report_api_error <- function(e, hint = NULL, args = list()) {
  caller_env <- parent.frame()
  hint_text <- if (!is.null(hint)) .interp_braces(hint, envir = caller_env) else "Request failed"
  cli::cli_alert_danger("{Sys.time()}: {hint_text}")
  if (length(args) > 0) {
    args_str <- paste0(
      names(args), " = ",
      vapply(args, function(a) {
        s <- tryCatch(deparse(a, width.cutoff = 60)[1], error = function(...) "<?>")
        if (nchar(s) > 60) paste0(substr(s, 1, 60), "...") else s
      }, character(1)),
      collapse = ", "
    )
    cli::cli_alert_danger("Args: {args_str}")
  }
  cli::cli_alert_danger("Error: {conditionMessage(e)}")
  invisible(NULL)
}

#' Report an API-call warning with full context
#'
#' Mirrors `.report_api_error()` for `tryCatch(warning = ...)` handlers.
#' @param w warning condition.
#' @param hint character. Same semantics as `.report_api_error()`'s `hint`.
#'   Defaults to "Request emitted a warning".
#' @param args optional named list of caller arguments to dump.
#' @keywords internal
#' @return Invisibly `NULL`. Called for its side effects.
.report_api_warning <- function(w, hint = NULL, args = list()) {
  caller_env <- parent.frame()
  hint_text <- if (!is.null(hint)) .interp_braces(hint, envir = caller_env) else "Request emitted a warning"
  cli::cli_alert_warning("{Sys.time()}: {hint_text}")
  if (length(args) > 0) {
    args_str <- paste0(
      names(args), " = ",
      vapply(args, function(a) {
        s <- tryCatch(deparse(a, width.cutoff = 60)[1], error = function(...) "<?>")
        if (nchar(s) > 60) paste0(substr(s, 1, 60), "...") else s
      }, character(1)),
      collapse = ", "
    )
    cli::cli_alert_warning("Args: {args_str}")
  }
  cli::cli_alert_warning("Warning: {conditionMessage(w)}")
  invisible(NULL)
}

#' Capture the calling function's formal arguments
#'
#' Returns a named list of the bound formal arguments (excluding `...`) of the
#' calling function, suitable for passing to `.report_api_error()` /
#' `.report_api_warning()`. Tolerates `...`-only / arg-less wrappers (where
#' `names(formals())` is `NULL`).
#' @keywords internal
#' @return Named list. Empty list if the caller has no non-`...` formals.
.capture_args <- function() {
  parent_fn <- sys.function(sys.parent())
  if (is.null(parent_fn)) return(list())
  fmls <- formals(parent_fn)
  if (length(fmls) == 0L) return(list())
  nms <- setdiff(names(fmls), "...")
  if (length(nms) == 0L) return(list())
  mget(nms, envir = parent.frame(), ifnotfound = list(NULL))
}

# ---------------------------------------------------------------------------
# Return-value initialization
# ---------------------------------------------------------------------------

# Build a class-carrying empty baseballr_data tibble for return-variable
# initialization (see CLAUDE.md "Return-Value Initialization"). Tibble-returning
# ESPN wrappers initialize their return variable BEFORE the tryCatch so that a
# connection error / HTTP 500 in the error handler yields a typed empty tibble
# rather than NULL (NULL breaks downstream dplyr::bind_rows() chains and the
# skip-if-empty test guard). `cols` (optional) gives a stable zero-row,
# character-typed schema for fixed-schema endpoints; when omitted a 0x0 classed
# tibble is returned (correct for the dynamic schemas most ESPN endpoints emit).
.empty_baseballr_data <- function(type = "ESPN data from ESPN.com", cols = NULL) {
  df <- if (is.null(cols) || length(cols) == 0L) {
    data.frame()
  } else {
    stats::setNames(
      as.data.frame(
        replicate(length(cols), character(0), simplify = FALSE),
        stringsAsFactors = FALSE
      ),
      cols
    )
  }
  make_baseballr_data(dplyr::as_tibble(df), type, Sys.time())
}
