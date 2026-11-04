# **Parse ESPN MLB Player Box, helper function**

**Parse ESPN MLB Player Box, helper function**

## Usage

``` r
helper_espn_mlb_player_box(resp)
```

## Arguments

- resp:

  Response object (text) from the ESPN MLB game-summary endpoint.

## Value

Returns a `baseballr_data` tibble (one row per athlete-side), or `NULL`.
A two-way player appears once for `batting` and once for `pitching`
(`stat_group`).
