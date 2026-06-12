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

Returns a tibble of each team's batting lineup (one row per batter, in
batting order) parsed from the game's `individual_stats` box scores.

|               |           |                                                  |
|---------------|-----------|--------------------------------------------------|
| col_name      | types     | description                                      |
| player_name   | character | Player name.                                     |
| position      | character | Fielding position (the box score "P" column).    |
| batting_order | integer   | Spot in the batting order (box-score row order). |
| team_name     | character | Team name (from the box-score totals row).       |
| player_id     | integer   | stats.ncaa.org player identifier.                |
| player_url    | character | Full stats.ncaa.org url for the player page.     |
| slug          | character | Relative stats.ncaa.org url for the player page. |
