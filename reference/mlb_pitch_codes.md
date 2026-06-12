# **MLB Pitch Codes**

**MLB Pitch Codes**

## Usage

``` r
mlb_pitch_codes()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| pitch_code | character | Single-character code identifying the pitch result. |
| pitch_description | character | Human-readable description of the pitch code. |
| swing_status | logical | Whether the code represents a swing. |
| swing_miss_status | logical | Whether the code represents a swinging strike (miss). |
| swing_contact_status | logical | Whether the code represents a swing that made contact. |
| sort_order | integer | Display sort order for the pitch code. |
| strike_status | logical | Whether the code counts as a strike. |
| ball_status | logical | Whether the code counts as a ball. |
| pitch_status | logical | Whether the code represents a pitch (vs. non-pitch event). |
| pitch_result_text | character | Display text for the pitch result. |
| bunt_attempt_status | logical | Whether the code represents a bunt attempt. |
| contact_status | logical | Whether the code represents bat-ball contact. |

## Examples

``` r
# \donttest{
  try(mlb_pitch_codes())
#> ── MLB Pitch Codes data from MLB.com ──────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 13:46:38 UTC
#> # A tibble: 39 × 12
#>    pitch_code pitch_description           swing_status swing_miss_status
#>    <chr>      <chr>                       <lgl>        <lgl>            
#>  1 C          Strike - Called             FALSE        FALSE            
#>  2 PSO        Pitcher Step Off            FALSE        FALSE            
#>  3 AC         Strike - Automatic (Pitch … FALSE        FALSE            
#>  4 VC         Ball - Automatic (Pitch Ti… FALSE        FALSE            
#>  5 S          Strike - Swinging           TRUE         TRUE             
#>  6 VP         Ball - Automatic (Pitch Ti… FALSE        FALSE            
#>  7 AB         Strike - Automatic (Batter… FALSE        FALSE            
#>  8 L          Strike - Foul Bunt          TRUE         FALSE            
#>  9 M          Strike - Missed Bunt        TRUE         TRUE             
#> 10 K          Strike - Unknown            FALSE        FALSE            
#> # ℹ 29 more rows
#> # ℹ 8 more variables: swing_contact_status <lgl>, sort_order <int>,
#> #   strike_status <lgl>, ball_status <lgl>, pitch_status <lgl>,
#> #   pitch_result_text <chr>, bunt_attempt_status <lgl>,
#> #   contact_status <lgl>
# }
```
