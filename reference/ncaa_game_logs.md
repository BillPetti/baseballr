# **Get NCAA Baseball Game Logs**

**Get NCAA Baseball Game Logs**

## Usage

``` r
ncaa_game_logs(player_id, year, type = "batting", span = "game", ...)
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

|               |           |
|---------------|-----------|
| col_name      | types     |
| player_id     | numeric   |
| player_name   | character |
| Date          | character |
| Opponent      | character |
| Result        | character |
| App           | numeric   |
| G             | numeric   |
| GS            | numeric   |
| IP            | numeric   |
| CG            | numeric   |
| H             | numeric   |
| R             | numeric   |
| ER            | numeric   |
| BB            | numeric   |
| SO            | numeric   |
| SHO           | numeric   |
| BF            | numeric   |
| P-OAB         | numeric   |
| 2B-A          | numeric   |
| 3B-A          | numeric   |
| Bk            | numeric   |
| HR-A          | numeric   |
| WP            | numeric   |
| HB            | numeric   |
| IBB           | numeric   |
| Inh Run       | numeric   |
| Inh Run Score | numeric   |
| SHA           | numeric   |
| SFA           | numeric   |
| Pitches       | numeric   |
| GO            | numeric   |
| FO            | numeric   |
| W             | numeric   |
| L             | numeric   |
| SV            | numeric   |
| OrdAppeared   | numeric   |
| KL            | numeric   |
| pickoffs      | character |

## Examples

``` r
# \donttest{
  try(ncaa_game_logs(player_id = 2649785, year = 2023, type = "pitching", span = "game"))
#> 2026-06-08 03:46:25.352247: Invalid arguments provided
#> data frame with 0 columns and 0 rows
  try(ncaa_game_logs(player_id = 2477974, year = 2023, type = "pitching", span = "career"))
#> 2026-06-08 03:46:30.632898: Invalid arguments provided
#> data frame with 0 columns and 0 rows
  try(ncaa_game_logs(player_id = 2680961, year = 2023, type = "batting", span = "game"))
#> 2026-06-08 03:46:35.942313: Invalid arguments provided
#> data frame with 0 columns and 0 rows
  try(ncaa_game_logs(player_id = 2486588, year = 2023, type = "batting", span = "career"))
#> 2026-06-08 03:46:41.1782: Invalid arguments provided
#> data frame with 0 columns and 0 rows
# }
```
