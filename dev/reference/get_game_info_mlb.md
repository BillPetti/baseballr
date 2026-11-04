# **(legacy) Retrieve additional game information for major and minor league games**

**(legacy) Retrieve additional game information for major and minor
league games**

## Usage

``` r
get_game_info_mlb(game_pk)
```

## Arguments

- game_pk:

  The unique game_pk identifier for the game

## Value

Returns a tibble that includes supplemental information, such as
weather, official scorer, attendance, etc., for the game_pk provided
