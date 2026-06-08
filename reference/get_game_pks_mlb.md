# **(legacy) Get MLB Game Info by Date and Level**

**(legacy) Get MLB Game Info by Date and Level**

## Usage

``` r
get_game_pks_mlb(date, level_ids = c(1))
```

## Arguments

- date:

  The date for which you want to find game_pk values for MLB games

- level_ids:

  A numeric vector with ids for each level where game_pks are desired.
  See below for a reference of level ids.

## Value

Returns a tibble that includes game_pk values and additional information
for games scheduled or played
