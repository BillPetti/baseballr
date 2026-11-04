# **Progressively**

This function helps add progress-reporting to any function - given
function `f()` and progressor `p()`, it will return a new function that
calls `f()` and then (on-exiting) will call `p()` after every iteration.

This is inspired by purrr's `safely`, `quietly`, and `possibly` function
decorators.

## Usage

``` r
progressively(f, p = NULL)
```

## Arguments

- f:

  a function to add progressr functionality to.

- p:

  a progressor function as created by
  [`progressr::progressor()`](https://progressr.futureverse.org/reference/progressor.html)

## Value

a function that does the same as `f` but it calls `p()` after iteration.
