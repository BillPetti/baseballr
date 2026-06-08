# **MLB Teams**

**MLB Teams**

## Usage

``` r
mlb_teams(
  season = NULL,
  active_status = NULL,
  all_star_statuses = NULL,
  league_ids = NULL,
  sport_ids = NULL,
  game_type = NULL
)
```

## Arguments

- season:

  Year to return to return team information for.

- active_status:

  The active statuses to populate teams for a given season.

- all_star_statuses:

  The all-star statuses to populate teams for a given season.

- league_ids:

  The league_id(s) to return team information for.

- sport_ids:

  The sport_id(s) to return team information for.

- game_type:

  The game_type to return team information for.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| all_star_status | character | All-star status flag. |
| team_id | integer | Team MLBAM ID. |
| team_full_name | character | Full team name. |
| link | character | API link to the team. |
| season | integer | Season year. |
| team_code | character | Internal team code. |
| file_code | character | File code abbreviation. |
| team_abbreviation | character | Team abbreviation. |
| team_name | character | Short team name. |
| location_name | character | Team location (city). |
| first_year_of_play | character | First year the franchise played. |
| short_name | character | Short display name. |
| franchise_name | character | Franchise name. |
| club_name | character | Club name. |
| active | logical | Whether the team is active. |
| spring_league_id | integer | Spring league MLBAM ID. |
| spring_league_name | character | Spring league name. |
| spring_league_link | character | API link to the spring league. |
| spring_league_abbreviation | character | Spring league abbreviation. |
| venue_id | integer | Home venue MLBAM ID. |
| venue_name | character | Home venue name. |
| venue_link | character | API link to the venue. |
| spring_venue_id | integer | Spring training venue MLBAM ID. |
| spring_venue_link | character | API link to the spring venue. |
| league_id | integer | League MLBAM ID. |
| league_name | character | League name. |
| league_link | character | API link to the league. |
| division_id | integer | Division MLBAM ID. |
| division_name | character | Division name. |
| division_link | character | API link to the division. |
| sport_id | integer | Sport MLBAM ID. |
| sport_link | character | API link to the sport. |
| sport_name | character | Sport name (e.g., Major League Baseball). |

## Examples

``` r
# \donttest{
  try(mlb_teams(season = 2021, sport_ids = c(1)))
#> ── MLB Teams data from MLB.com ────────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 01:57:35 UTC
#> # A tibble: 30 × 33
#>    all_star_status team_id team_full_name       link    season team_code
#>    <chr>             <int> <chr>                <chr>    <int> <chr>    
#>  1 N                   133 Oakland Athletics    /api/v…   2021 oak      
#>  2 N                   134 Pittsburgh Pirates   /api/v…   2021 pit      
#>  3 N                   135 San Diego Padres     /api/v…   2021 sdn      
#>  4 N                   136 Seattle Mariners     /api/v…   2021 sea      
#>  5 N                   137 San Francisco Giants /api/v…   2021 sfn      
#>  6 N                   138 St. Louis Cardinals  /api/v…   2021 sln      
#>  7 N                   139 Tampa Bay Rays       /api/v…   2021 tba      
#>  8 N                   140 Texas Rangers        /api/v…   2021 tex      
#>  9 N                   141 Toronto Blue Jays    /api/v…   2021 tor      
#> 10 N                   142 Minnesota Twins      /api/v…   2021 min      
#> # ℹ 20 more rows
#> # ℹ 27 more variables: file_code <chr>, team_abbreviation <chr>,
#> #   team_name <chr>, location_name <chr>, first_year_of_play <chr>,
#> #   short_name <chr>, franchise_name <chr>, club_name <chr>,
#> #   active <lgl>, spring_league_id <int>, spring_league_name <chr>,
#> #   spring_league_link <chr>, spring_league_abbreviation <chr>,
#> #   venue_id <int>, venue_name <chr>, venue_link <chr>, …
# }
```
