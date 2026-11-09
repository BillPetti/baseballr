#' tryCatch() whose warning handler reports without discarding the result
#'
#' A `warning =` handler given to [tryCatch()] unwinds the call, so any warning
#' raised while parsing (for example data.frame()'s "row names were found from a
#' short variable and have been discarded") made a wrapper return its empty
#' default. `.try_catch()` takes the same arguments but runs `warning` as a calling
#' handler: the warning is reported (or silently dropped when the handler does
#' nothing) and evaluation carries on. `error` and `finally` behave as in
#' [tryCatch()].
#'
#' @param expr Expression to evaluate.
#' @param ... Other condition handlers, passed to [tryCatch()] (usually `error =`).
#' @param warning A function of one argument (the warning) or `NULL`.
#' @param finally Expression evaluated on exit, as in [tryCatch()].
#' @return The value of `expr`, or of the handler that caught a condition.
#' @keywords internal
#' @noRd
.try_catch <- function(expr, ..., warning = NULL, finally = NULL) {
  withCallingHandlers(
    tryCatch(expr, ..., finally = finally),
    warning = function(w) {
      if (is.function(warning)) warning(w)
      invokeRestart("muffleWarning")
    }
  )
}
