# **MLB Game Status Codes**

**MLB Game Status Codes**

## Usage

``` r
mlb_game_status_codes()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| abstract_game_state | character | Abstract game state grouping (e.g. 'Preview', 'Live', 'Final'). |
| coded_game_state | character | Single-letter coded game state (e.g. 'S', 'P', 'I', 'F'). |
| detailed_state | character | Detailed status description (e.g. 'Scheduled', 'Pre-Game', 'In Progress'). |
| status_code | character | Status code identifier (e.g. 'S', 'P', 'I', 'F'). |
| reason | character | Reason text for the status when applicable (e.g. delay/postponement). |
| abstract_game_code | character | Single-letter abstract game code (e.g. 'P', 'L', 'F'). |

## Examples

``` r
# \donttest{
  try(mlb_game_status_codes())
#> ── MLB Game Status Codes data from MLB.com ────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:23:52 UTC
#> # A tibble: 209 × 6
#>    abstract_game_state coded_game_state detailed_state       status_code
#>    <chr>               <chr>            <chr>                <chr>      
#>  1 Preview             S                Scheduled            S          
#>  2 Preview             P                Pre-Game             P          
#>  3 Live                P                Warmup               PW         
#>  4 Preview             P                Delayed Start: Rain  PR         
#>  5 Preview             P                Delayed Start: Snow  PS         
#>  6 Preview             P                Delayed Start: Wet … PG         
#>  7 Preview             P                Delayed Start: Venue PV         
#>  8 Preview             P                Delayed Start: Fog   PF         
#>  9 Preview             P                Delayed Start: Cold  PC         
#> 10 Preview             P                Delayed Start: Air … PD         
#> # ℹ 199 more rows
#> # ℹ 2 more variables: abstract_game_code <chr>, reason <chr>
# }
```
