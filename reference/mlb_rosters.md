# **Find MLB Rosters by Roster Type**

**Find MLB Rosters by Roster Type**

## Usage

``` r
mlb_rosters(team_id = NULL, season = NULL, date = NULL, roster_type = NULL)
```

## Arguments

- team_id:

  team_id to return team roster information for a particular club.

- season:

  Year to return team roster information for a particular club in a
  specific season.

- date:

  Date to return team roster and their coaching staff directorial
  information for a particular team.

- roster_type:

  roster_type to return team directorial information for. See
  [`mlb_roster_types()`](https://billpetti.github.io/baseballr/reference/mlb_roster_types.md)
  for more options. Valid options include: '40Man', 'fullSeason',
  'fullRoster', 'nonRosterInvitees', 'active', 'allTime', 'depthChart',
  'gameday', 'coach'

## Value

Returns a tibble with one row per roster member with the following
columns (player roster types). The `coach` roster type instead returns
`job`, `job_id`, and `title` columns in place of the position/status
columns:

|                       |           |                                            |
|-----------------------|-----------|--------------------------------------------|
| col_name              | types     | description                                |
| jersey_number         | character | Player's uniform number.                   |
| person_id             | integer   | MLBAM player ID.                           |
| person_full_name      | character | Player's full name.                        |
| person_link           | character | API link to the player resource.           |
| position_code         | character | Numeric scorekeeping position code.        |
| position_name         | character | Full position name.                        |
| position_type         | character | Position category (e.g. 'Infielder').      |
| position_abbreviation | character | Position abbreviation (e.g. 'SS').         |
| status_code           | character | Roster status code (e.g. 'A').             |
| status_description    | character | Roster status description (e.g. 'Active'). |
| link                  | character | API link to the team roster resource.      |
| team_id               | integer   | MLBAM team ID.                             |
| roster_type           | character | Roster type returned.                      |
| season                | numeric   | Season requested.                          |
| date                  | character | Date requested, or NA if not supplied.     |

## Examples

``` r
# \donttest{
  try(mlb_rosters(team_id = 109, season = 2018, roster_type = 'active'))
#> ── MLB Roster data from MLB.com ───────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 13:46:41 UTC
#> # A tibble: 49 × 15
#>    jersey_number person_id person_full_name person_link    position_code
#>    <chr>             <int> <chr>            <chr>          <chr>        
#>  1 13               605113 Nick Ahmed       /api/v1/peopl… 6            
#>  2 35               542882 Matt Andriese    /api/v1/peopl… 1            
#>  3 5                488671 Alex Avila       /api/v1/peopl… 2            
#>  4 33               545332 Jake Barrett     /api/v1/peopl… 1            
#>  5 31               502202 Brad Boxberger   /api/v1/peopl… 1            
#>  6 61               611093 Silvino Bracho   /api/v1/peopl… 1            
#>  7 25               605151 Archie Bradley   /api/v1/peopl… 1            
#>  8 19               593647 Sócrates Brito   /api/v1/peopl… 9            
#>  9 32               453329 Clay Buchholz    /api/v1/peopl… 1            
#> 10 40               605177 Andrew Chafin    /api/v1/peopl… 1            
#> # ℹ 39 more rows
#> # ℹ 10 more variables: position_name <chr>, position_type <chr>,
#> #   position_abbreviation <chr>, status_code <chr>,
#> #   status_description <chr>, link <chr>, team_id <int>,
#> #   roster_type <chr>, season <dbl>, date <chr>
  try(mlb_rosters(team_id = 109, season = 2018, roster_type = 'coach'))
#> ── MLB Roster data from MLB.com ───────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 13:46:41 UTC
#> # A tibble: 13 × 12
#>    jersey_number job             job_id title person_id person_full_name
#>    <chr>         <chr>           <chr>  <chr>     <int> <chr>           
#>  1 "17"          Manager         MNGR   Mana…    117950 Torey Lovullo   
#>  2 "12"          Bench Coach     COAB   Benc…    119655 Jerry Narron    
#>  3 "18"          Hitting Coach   COAT   Hitt…    118132 Dave Magadan    
#>  4 "23"          Pitching Coach  COAP   Pitc…    111807 Mike Butcher    
#>  5 "36"          First Base Coa… COA1   Firs…    118763 Dave McKay      
#>  6 "21"          Third Base Coa… COA3   Thir…    120418 Tony Perezchica 
#>  7 "71"          Bullpen Coach   COAU   Bull…    114097 Mike Fetters    
#>  8 "59"          Assistant Hitt… COAA   Assi…    117420 Tim Laker       
#>  9 "7"           Quality Contro… QCCC   Qual…    425631 Robby Hammock   
#> 10 "60"          Coach           COAC   Majo…    468222 Luis Urueta     
#> 11 "83"          Bullpen Catcher BCAT   Bull…    461861 Mark Reed       
#> 12 "84"          Bullpen Catcher BCAT   Bull…    279827 Humberto Quinte…
#> 13 ""            Senior Basebal… SBAD   Seni…    678771 Shawn Marette   
#> # ℹ 6 more variables: person_link <chr>, link <chr>, team_id <int>,
#> #   roster_type <chr>, season <dbl>, date <chr>
# }
```
