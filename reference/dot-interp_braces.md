# Minimal brace-template interpolator

Replaces `{expr}` tokens in `template` by evaluating `expr` in `envir`.
Used in
[`.report_api_error()`](https://billpetti.github.io/baseballr/reference/dot-report_api_error.md)
/
[`.report_api_warning()`](https://billpetti.github.io/baseballr/reference/dot-report_api_warning.md)
so callers can write hints like `"No data for {game_id}"` and have
`{game_id}` resolve against the function's frame at the call-site.
Per-token failures leave the literal `{expr}` in place rather than
erroring.

## Usage

``` r
.interp_braces(template, envir = parent.frame())
```

## Arguments

- template:

  character(1).

- envir:

  environment to evaluate expressions against.

## Value

character(1).
