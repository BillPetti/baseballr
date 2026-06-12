# **MLB Teams History**

**MLB Teams History**

## Usage

``` r
mlb_team_history(team_ids = NULL, start_season = NULL, end_season = NULL)
```

## Arguments

- team_ids:

  The team_id(s) to return historical data for.

- start_season:

  The start_season to return historical data for from the given year to
  present.

- end_season:

  The end_season to return historical data for from the the creation to
  the given year.

## Value

Returns a tibble with the following columns

|                    |           |                                           |
|--------------------|-----------|-------------------------------------------|
| col_name           | types     | description                               |
| all_star_status    | character | All-star status flag.                     |
| team_id            | integer   | Team MLBAM ID.                            |
| team_full_name     | character | Full team name.                           |
| link               | character | API link to the team.                     |
| season             | integer   | Season year for the historical record.    |
| team_code          | character | Internal team code.                       |
| file_code          | character | File code abbreviation.                   |
| team_abbreviation  | character | Team abbreviation.                        |
| team_name          | character | Short team name.                          |
| location_name      | character | Team location (city).                     |
| first_year_of_play | character | First year the franchise played.          |
| short_name         | character | Short display name.                       |
| franchise_name     | character | Franchise name.                           |
| club_name          | character | Club name.                                |
| active             | logical   | Whether the team is active.               |
| venue_id           | integer   | Home venue MLBAM ID for that season.      |
| venue_name         | character | Home venue name for that season.          |
| venue_link         | character | API link to the venue.                    |
| spring_venue_id    | integer   | Spring training venue MLBAM ID.           |
| spring_venue_link  | character | API link to the spring venue.             |
| league_id          | integer   | League MLBAM ID.                          |
| league_name        | character | League name.                              |
| league_link        | character | API link to the league.                   |
| sport_id           | integer   | Sport MLBAM ID.                           |
| sport_link         | character | API link to the sport.                    |
| sport_name         | character | Sport name (e.g., Major League Baseball). |

## Examples

``` r
# \donttest{
  try(mlb_team_history(team_ids = 147))
#> ── MLB Team History data from MLB.com ─────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:42:59 UTC
#> # A tibble: 5 × 26
#>   all_star_status team_id team_full_name       link     season team_code
#>   <chr>             <int> <chr>                <chr>     <int> <chr>    
#> 1 N                   147 New York Yankees     /api/v1…   2009 nya      
#> 2 N                   147 New York Yankees     /api/v1…   1974 nya      
#> 3 N                   147 New York Yankees     /api/v1…   1923 nya      
#> 4 N                   147 New York Yankees     /api/v1…   1913 nya      
#> 5 N                   147 New York Highlanders /api/v1…   1903 nya      
#> # ℹ 20 more variables: file_code <chr>, team_abbreviation <chr>,
#> #   team_name <chr>, location_name <chr>, first_year_of_play <chr>,
#> #   short_name <chr>, franchise_name <chr>, club_name <chr>,
#> #   active <lgl>, venue_id <int>, venue_name <chr>, venue_link <chr>,
#> #   spring_venue_id <int>, spring_venue_link <chr>, league_id <int>,
#> #   league_name <chr>, league_link <chr>, sport_id <int>,
#> #   sport_link <chr>, sport_name <chr>
# }
```
