# **(legacy) Retrieve batting orders for a given MLB game**

**(legacy) Retrieve batting orders for a given MLB game**

## Usage

``` r
get_batting_orders(game_pk, type = "starting")
```

## Arguments

- game_pk:

  The unique game_pk identifier for the game

- type:

  Whether to just return the starting lineup ('starting') or all batters
  that appeared ('all')

## Value

Returns a tibble that includes probable starting pitchers and the home
plate umpire for the `game_pk` requested
