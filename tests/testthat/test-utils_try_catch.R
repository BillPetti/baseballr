test_that(".try_catch() reports a warning without discarding the result", {
  skip_on_cran()
  seen <- NULL
  out <- .try_catch(
    expr = {
      x <- 1
      warning("harmless")
      x + 1
    },
    error = function(e) "error branch",
    warning = function(w) seen <<- conditionMessage(w)
  )
  expect_equal(out, 2)
  expect_equal(seen, "harmless")
})

test_that(".try_catch() still catches errors and runs finally", {
  skip_on_cran()
  ran <- FALSE
  out <- .try_catch(
    expr = stop("boom"),
    error = function(e) conditionMessage(e),
    warning = function(w) NULL,
    finally = ran <- TRUE
  )
  expect_equal(out, "boom")
  expect_true(ran)
})

test_that(".try_catch() keeps assignments and early return() in the caller", {
  skip_on_cran()
  f <- function(empty) {
    result <- "default"
    .try_catch(
      expr = {
        if (empty) return(result)
        suppressWarnings(as.integer("x"))   # muffled inside
        warning("parse warning")             # reported by the handler
        result <- "parsed"
      },
      error = function(e) NULL,
      warning = function(w) {}
    )
    result
  }
  expect_equal(f(TRUE), "default")
  expect_equal(f(FALSE), "parsed")
})
