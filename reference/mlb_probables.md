# **Retrieve probable starters for a given MLB game**

**Retrieve probable starters for a given MLB game**

## Usage

``` r
mlb_probables(game_pk)
```

## Arguments

- game_pk:

  The unique game_pk identifier for the game

## Value

Returns a tiible that includes probable starting pitchers and the home
plate umpire for the `game_pk` requested including the following
columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_pk | integer | Unique game identifier. |
| game_date | character | Date of the game (YYYY-MM-DD). |
| fullName | character | Full name of the probable starting pitcher. |
| id | integer | MLBAM player ID of the probable starting pitcher. |
| team | character | Team name for the probable pitcher. |
| team_id | integer | MLBAM team ID for the probable pitcher. |
| home_plate_full_name | character | Full name of the home plate umpire. |
| home_plate_id | integer | MLBAM ID of the home plate umpire. |

## Examples

``` r
# \donttest{
  try(mlb_probables(566001))
#> ── MLB Probables data from MLB.com ────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-24 02:06:25 UTC
#> # A tibble: 2 × 8
#>   game_pk game_date  fullName      id team  team_id home_plate_full_name
#>     <int> <chr>      <chr>      <int> <chr>   <int> <chr>               
#> 1  566001 2019-04-29 Tanner R… 543699 Cinc…     113 Gabe Morales        
#> 2  566001 2019-04-29 Zack Whe… 554430 New …     121 Gabe Morales        
#> # ℹ 1 more variable: home_plate_id <int>
# }
```
