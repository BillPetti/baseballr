# **MLB Team Info**

**MLB Team Info**

## Usage

``` r
mlb_team_info(team_id = NULL, season = NULL, sport_id = NULL)
```

## Arguments

- team_id:

  The team_id to return team data for.

- season:

  The season to return team data for the given year.

- sport_id:

  The sport_id to return a directory of team data for a particular club
  in a sport.

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
  try(mlb_team_info(team_id = 147))
#> ── MLB Team Info data from MLB.com ────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 03:19:40 UTC
#> # A tibble: 1 × 33
#>   all_star_status team_id team_full_name   link         season team_code
#>   <chr>             <int> <chr>            <chr>         <int> <chr>    
#> 1 N                   147 New York Yankees /api/v1/tea…   2026 nya      
#> # ℹ 27 more variables: file_code <chr>, team_abbreviation <chr>,
#> #   team_name <chr>, location_name <chr>, first_year_of_play <chr>,
#> #   short_name <chr>, franchise_name <chr>, club_name <chr>,
#> #   active <lgl>, spring_league_id <int>, spring_league_name <chr>,
#> #   spring_league_link <chr>, spring_league_abbreviation <chr>,
#> #   venue_id <int>, venue_name <chr>, venue_link <chr>,
#> #   spring_venue_id <int>, spring_venue_link <chr>, league_id <int>, …
# }
```
