# **MLB Team Affiliates**

**MLB Team Affiliates**

## Usage

``` r
mlb_team_affiliates(team_ids = NULL, sport_ids = NULL, season = NULL)
```

## Arguments

- team_ids:

  The team_id(s) to return affiliates data for.

- sport_ids:

  The sport_id to return team affiliates information for.

- season:

  The season to return team affiliates data for the particular season.

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
| parent_org_name | character | Parent organization name. |
| parent_org_id | integer | Parent organization MLBAM ID. |
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
  try(mlb_team_affiliates(team_ids = 147))
#> ── MLB Team Affiliates data from MLB.com ──────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-24 02:06:35 UTC
#> # A tibble: 11 × 35
#>    all_star_status team_id team_full_name         link  season team_code
#>    <chr>             <int> <chr>                  <chr>  <int> <chr>    
#>  1 N                   147 New York Yankees       /api…   2026 nya      
#>  2 F                   386 New York Yankees Pros… /api…   2026 nyp      
#>  3 N                   531 Scranton/Wilkes-Barre… /api…   2026 swb      
#>  4 N                  1956 Somerset Patriots      /api…   2026 som      
#>  5 N                   537 Hudson Valley Renegad… /api…   2026 hdv      
#>  6 N                   634 DSL NYY Bombers        /api…   2026 dyb      
#>  7 N                   635 DSL NYY Yankees        /api…   2026 dya      
#>  8 N                   475 FCL Yankees            /api…   2026 fya      
#>  9 N                   587 Tampa Tarpons          /api…   2026 tpa      
#> 10 N                  3308 Yankees Alternate Tra… /api…   2026 nys      
#> 11 O                  3309 Yankees Organization   /api…   2026 nyo      
#> # ℹ 29 more variables: file_code <chr>, team_abbreviation <chr>,
#> #   team_name <chr>, location_name <chr>, first_year_of_play <chr>,
#> #   short_name <chr>, franchise_name <chr>, club_name <chr>,
#> #   active <lgl>, parent_org_name <chr>, parent_org_id <int>,
#> #   spring_league_id <int>, spring_league_name <chr>,
#> #   spring_league_link <chr>, spring_league_abbreviation <chr>,
#> #   venue_id <int>, venue_name <chr>, venue_link <chr>, …
# }
```
