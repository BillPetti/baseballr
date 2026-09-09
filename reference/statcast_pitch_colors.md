# **Baseball Savant pitch-type colors**

Returns Baseball Savant's pitch-type color palette as a tibble
(`pitch_type` abbreviation, `pitch_name`, and `color` hex code), for
coloring pitch charts consistently with Savant's own visuals.

## Usage

``` r
statcast_pitch_colors()
```

## Value

A tibble with columns `pitch_type`, `pitch_name`, `color`.

## Examples

``` r
statcast_pitch_colors()
#> ── MLB Baseball Savant pitch-type colors ──────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-09-09 22:15:25 UTC
#> # A tibble: 18 × 3
#>    pitch_type pitch_name       color  
#>    <chr>      <chr>            <chr>  
#>  1 FF         4-Seam Fastball  #D22D49
#>  2 SI         Sinker           #FE9D00
#>  3 FC         Cutter           #933F2C
#>  4 CH         Changeup         #1DBE3A
#>  5 FS         Splitter         #3BACAC
#>  6 FO         Forkball         #55CCAB
#>  7 SC         Screwball        #60DB33
#>  8 CU         Curveball        #00D1ED
#>  9 KC         Knuckle Curve    #6236CD
#> 10 CS         Slow Curve       #274BFC
#> 11 SL         Slider           #EEE716
#> 12 ST         Sweeper          #DDB33A
#> 13 SV         Slurve           #93AFD4
#> 14 KN         Knuckleball      #3C44CD
#> 15 EP         Eephus           #888888
#> 16 FA         Other            #888888
#> 17 IN         Intentional Ball #888888
#> 18 PO         Pitchout         #888888
```
