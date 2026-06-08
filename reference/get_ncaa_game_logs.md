# **(legacy) Get NCAA Baseball Game Logs**

**(legacy) Get NCAA Baseball Game Logs**

## Usage

``` r
get_ncaa_game_logs(player_id, year, type = "batting", span = "game", ...)
```

## Arguments

- player_id:

  A player's unique id. Can be found using the get_ncaa_baseball_roster
  function.

- year:

  The year of interest.

- type:

  The kind of statistics you want to return. Current options are
  'batting' or 'pitching'.

- span:

  The span of time; can either be 'game' for game logs in a season, or
  'career' which returns seasonal stats for a player's career.

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame containing player and school information as well as game by
game statistics
