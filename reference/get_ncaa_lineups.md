# **(legacy) Retrieve lineups for a given NCAA game via its `game_info_url`**

**(legacy) Retrieve lineups for a given NCAA game via its
`game_info_url`**

## Usage

``` r
get_ncaa_lineups(game_info_url = NULL, ...)
```

## Arguments

- game_info_url:

  The unique game info url

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

Returns a tibble of each school's starting lineup and starting pitcher
