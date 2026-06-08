# **Process Baseball Savant CSV payload**

This is a helper function for all
[`statcast_search()`](https://billpetti.github.io/baseballr/reference/statcast_search.md)
functions. The function processes the initial csv payload acquired from
Baseball Savant to ensure consistency in formatting across downloads

## Usage

``` r
process_statcast_payload(payload)
```

## Arguments

- payload:

  payload from a Baseball Savant request

## Value

A tibble with the processed Statcast data coerced to the correct types.
