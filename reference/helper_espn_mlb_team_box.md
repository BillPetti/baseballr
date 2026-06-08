# **Parse ESPN MLB Team Box, helper function**

**Parse ESPN MLB Team Box, helper function**

## Usage

``` r
helper_espn_mlb_team_box(resp)
```

## Arguments

- resp:

  Response object (text) from the ESPN MLB game-summary endpoint.

## Value

Returns a `baseballr_data` tibble (one row per team), or `NULL`.
