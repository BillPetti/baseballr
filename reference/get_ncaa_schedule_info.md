# **(legacy) Get Schedule and Results for NCAA Baseball Teams**

**(legacy) Get Schedule and Results for NCAA Baseball Teams**

## Usage

``` r
get_ncaa_schedule_info(team_id = NULL, year = NULL, pbp_links = FALSE, ...)
```

## Arguments

- team_id:

  The team's unique NCAA id.

- year:

  The season (i.e. use 2016 for the 2015-2016 season, etc.)

- pbp_links:

  Logical parameter to run process for scraping play_by_play urls for
  each game

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame with the following fields: date, opponent, result, score,
innings (if more than regulation), and the url for the game itself.
