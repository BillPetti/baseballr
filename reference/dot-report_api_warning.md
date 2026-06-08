# Report an API-call warning with full context

Mirrors
[`.report_api_error()`](https://billpetti.github.io/baseballr/reference/dot-report_api_error.md)
for `tryCatch(warning = ...)` handlers.

## Usage

``` r
.report_api_warning(w, hint = NULL, args = list())
```

## Arguments

- w:

  warning condition.

- hint:

  character. Same semantics as
  [`.report_api_error()`](https://billpetti.github.io/baseballr/reference/dot-report_api_error.md)'s
  `hint`. Defaults to "Request emitted a warning".

- args:

  optional named list of caller arguments to dump.

## Value

Invisibly `NULL`. Called for its side effects.
