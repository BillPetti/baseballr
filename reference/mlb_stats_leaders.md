# **MLB Stats Leaders**

**MLB Stats Leaders**

## Usage

``` r
mlb_stats_leaders(
  leader_categories = NULL,
  player_pool = NULL,
  leader_game_types = NULL,
  sit_codes = NULL,
  position = NULL,
  stat_group = NULL,
  season = NULL,
  league_id = NULL,
  sport_id = NULL,
  start_date = NULL,
  end_date = NULL,
  stat_type = NULL,
  limit = 1000
)
```

## Arguments

- leader_categories:

  League leader category to return information and ranking for a
  particular statistic.

- player_pool:

  There are 4 different types of player pools to return statistics for a
  particular player pool across a sport. Acceptable values include: All,
  Qualified, Rookies, or Qualified_rookies

- leader_game_types:

  Game type to return information and ranking for a particular statistic
  in a particular game type.

- sit_codes:

  Situation code to return information and ranking for a particular
  statistic in a particular game type.

- position:

  Position to return statistics for a given position. Default to
  "Qualified" player pool Acceptable values include:

  - P

  - C

  - 1B

  - 2B

  - 3B

  - SS

  - LF

  - CF

  - RF

  - DH

  - PH

  - PR

  - BR

  - OF

  - IF

  - SP

  - RP

  - CP

  - UT

  - UI

  - UO

  - RHP

  - LHP

  - RHS

  - LHS

  - LHR

  - RHR

  - B

  - X

- stat_group:

  Stat group to return information and ranking for a particular
  statistic in a particular group.

- season:

  Year to return information and ranking for a particular statistic in a
  given year.

- league_id:

  League ID to return statistics for a given league. Default to
  "Qualified" player pool.

- sport_id:

  The sport_id to return information and ranking information for.

- start_date:

  Start date to return information and ranking for a particular
  statistic for a particular date range. Format: MM/DD/YYYY *start_date
  must be coupled with end_date and byDateRange stat_type*

- end_date:

  End date to return information and ranking for a particular statistic
  for a particular date range. Format: MM/DD/YYYY *end_date must be
  coupled with start_date and byDateRange stat_type*

- stat_type:

  The stat_type to return information and ranking for a particular
  statistic for a particular stat type.

- limit:

  A limit to limit return to a particular number of records.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| leader_category | character | Statistical leader category (e.g., homeRuns). |
| rank | integer | Rank within the leaderboard. |
| value | character | Statistic value for the leader. |
| season | character | Season year. |
| num_teams | integer | Number of teams the player appeared for. |
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
 try(mlb_stats_leaders(leader_categories='homeRuns',sport_id=1, season = 2021))
#> ── MLB Stats Leaders data from MLB.com ────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:25:11 UTC
#> # A tibble: 306 × 23
#>    leader_category  rank value season num_teams team_id team_name       
#>    <chr>           <int> <chr> <chr>      <int>   <int> <chr>           
#>  1 homeRuns            1 48    2021           1     141 Toronto Blue Ja…
#>  2 homeRuns            1 48    2021           1     118 Kansas City Roy…
#>  3 homeRuns            3 46    2021           1     108 Los Angeles Ang…
#>  4 homeRuns            4 45    2021           1     141 Toronto Blue Ja…
#>  5 homeRuns            5 42    2021           1     135 San Diego Padres
#>  6 homeRuns            6 39    2021           1     136 Seattle Mariners
#>  7 homeRuns            6 39    2021           1     147 New York Yankees
#>  8 homeRuns            6 39    2021           1     139 Tampa Bay Rays  
#>  9 homeRuns            6 39    2021           1     133 Oakland Athleti…
#> 10 homeRuns           10 38    2021           1     111 Boston Red Sox  
#> # ℹ 296 more rows
#> # ℹ 16 more variables: team_link <chr>, league_id <int>,
#> #   league_name <chr>, league_link <chr>, person_id <int>,
#> #   person_full_name <chr>, person_link <chr>, person_first_name <chr>,
#> #   person_last_name <chr>, sport_id <int>, sport_link <chr>,
#> #   sport_abbreviation <chr>, stat_group <chr>, total_splits <int>,
#> #   game_type_id <chr>, game_type_description <chr>
# }
```
