# **MLB Team Leaders**

**MLB Team Leaders**

## Usage

``` r
mlb_team_leaders(
  team_id = NULL,
  leader_categories = NULL,
  leader_game_types = NULL,
  season = NULL,
  limit = 1000
)
```

## Arguments

- team_id:

  Team ID to return team leader information for.

- leader_categories:

  Team leader category to return information and ranking for a
  particular statistic.

- leader_game_types:

  Game type to return information and ranking for a particular statistic
  in a particular game type.

- season:

  Season to return team leader information for.

- limit:

  A limit to limit return to a particular number of records.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| leader_category | character | Team leader category (e.g., homeRuns). |
| rank | integer | Rank within the team leaderboard. |
| value | character | Statistic value for the leader. |
| season | character | Season year. |
| team_id | integer | Team MLBAM ID. |
| team_name | character | Team name. |
| team_link | character | API link to the team. |
| league_id | integer | League MLBAM ID. |
| league_name | character | League name. |
| league_link | character | API link to the league. |
| person_id | integer | Player MLBAM ID. |
| person_full_name | character | Player full name. |
| person_link | character | API link to the player. |
| person_first_name | character | Player first name. |
| person_last_name | character | Player last name. |
| sport_id | integer | Sport MLBAM ID. |
| sport_link | character | API link to the sport. |
| sport_abbreviation | character | Sport abbreviation (e.g., MLB). |
| stat_group | character | Stat group (e.g., hitting). |
| total_splits | integer | Total number of splits in the leaderboard. |
| game_type_id | character | Game type code (e.g., R for regular season). |
| game_type_description | character | Game type description. |

## Examples

``` r
# \donttest{
  try(mlb_team_leaders(team_id = 137, leader_categories = "homeRuns", season = 2021))
#> ── MLB Team Leaders data from MLB.com ─────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:57:27 UTC
#> # A tibble: 52 × 22
#>    leader_category  rank value season team_id team_name        team_link
#>    <chr>           <int> <chr> <chr>    <int> <chr>            <chr>    
#>  1 homeRuns            1 29    2021       137 San Francisco G… /api/v1/…
#>  2 homeRuns            2 25    2021       137 San Francisco G… /api/v1/…
#>  3 homeRuns            3 24    2021       137 San Francisco G… /api/v1/…
#>  4 homeRuns            4 18    2021       137 San Francisco G… /api/v1/…
#>  5 homeRuns            4 18    2021       137 San Francisco G… /api/v1/…
#>  6 homeRuns            4 18    2021       137 San Francisco G… /api/v1/…
#>  7 homeRuns            7 16    2021       137 San Francisco G… /api/v1/…
#>  8 homeRuns            8 13    2021       137 San Francisco G… /api/v1/…
#>  9 homeRuns            8 13    2021       137 San Francisco G… /api/v1/…
#> 10 homeRuns           10 12    2021       137 San Francisco G… /api/v1/…
#> # ℹ 42 more rows
#> # ℹ 15 more variables: league_id <int>, league_name <chr>,
#> #   league_link <chr>, person_id <int>, person_full_name <chr>,
#> #   person_link <chr>, person_first_name <chr>, person_last_name <chr>,
#> #   sport_id <int>, sport_link <chr>, sport_abbreviation <chr>,
#> #   stat_group <chr>, total_splits <int>, game_type_id <chr>,
#> #   game_type_description <chr>
# }
```
