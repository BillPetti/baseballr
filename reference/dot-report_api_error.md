# Report an API-call error with full context

Standardizes the message every ESPN MLB wrapper emits inside its
`tryCatch(error = ...)` block: a timestamped friendly hint
(brace-interpolated against the caller env), a dump of the call's
arguments, and the actual error message. Wrappers capture their formals
once near the top with `.args <- mget(setdiff(names(formals()), "..."))`
(or `.args <- .capture_args()` for arg-less wrappers).

## Usage

``` r
.report_api_error(e, hint = NULL, args = list())
```

## Arguments

- e:

  error condition.

- hint:

  character. Friendly message with optional `{name}` tokens that resolve
  against the caller's environment. Defaults to "Request failed".

- args:

  optional named list of caller arguments to dump.

## Value

Invisibly `NULL`. Called for its side effects.
