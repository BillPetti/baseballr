# **Retrieve lineups for a given NCAA game via its `game_info_url`**

**Retrieve lineups for a given NCAA game via its `game_info_url`**

## Usage

``` r
ncaa_lineups(game_info_url = NULL, ...)
```

## Arguments

- game_info_url:

  The unique game info url

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

Returns a tibble of each school's starting lineup and starting pitcher

|               |           |
|---------------|-----------|
| col_name      | types     |
| year          | numeric   |
| player_name   | character |
| position      | character |
| slug          | character |
| batting_order | character |
| team_name     | character |
| sub           | numeric   |
| attendance    | character |
| game_date     | character |
| location      | character |
| player_id     | integer   |
| team_id       | numeric   |
| team_url      | character |
| conference_id | numeric   |
| conference    | character |
| division      | numeric   |
| season_id     | numeric   |

## Examples

``` r
# \donttest{
  try(ncaa_lineups(game_info_url="https://stats.ncaa.org/contests/2167178/box_score"))
#> 2026-06-12 03:20:17.122245: Invalid arguments provided
#> Error in ncaa_lineups(game_info_url = "https://stats.ncaa.org/contests/2167178/box_score") : 
#>   object 'lineup_table' not found
  try(ncaa_lineups(game_info_url="https://stats.ncaa.org/game/index/4587474?org_id=528"))
#> 2026-06-12 03:20:22.637211: Invalid arguments provided
#> Error in ncaa_lineups(game_info_url = "https://stats.ncaa.org/game/index/4587474?org_id=528") : 
#>   object 'lineup_table' not found
# }
```
