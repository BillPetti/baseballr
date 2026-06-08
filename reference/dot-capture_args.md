# Capture the calling function's formal arguments

Returns a named list of the bound formal arguments (excluding `...`) of
the calling function, suitable for passing to
[`.report_api_error()`](https://billpetti.github.io/baseballr/reference/dot-report_api_error.md)
/
[`.report_api_warning()`](https://billpetti.github.io/baseballr/reference/dot-report_api_warning.md).
Tolerates `...`-only / arg-less wrappers (where `names(formals())` is
`NULL`).

## Usage

``` r
.capture_args()
```

## Value

Named list. Empty list if the caller has no non-`...` formals.
