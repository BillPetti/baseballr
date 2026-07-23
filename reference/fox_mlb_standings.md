# **Get Fox Sports MLB standings**

**Get Fox Sports MLB standings**

## Usage

``` r
fox_mlb_standings(team_id)
```

## Arguments

- team_id:

  Fox Bifrost team id (standings of that team's division/league).

## Value

A `baseballr_data` tibble of standings rows (`team_id`, `section`, the
standings columns, `entity_id`).

## Examples

``` r
 try(fox_mlb_standings("1")) 
#> ── Fox Sports MLB standings ───────────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-07-23 17:33:40 UTC
#> # A tibble: 90 × 24
#>    team_id section  al_east v2       w_l   pct   gb    home  away  rs   
#>    <chr>   <chr>    <chr>   <chr>    <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 1       DIVISION 1       Rays     59-42 .584  -     35-15 24-27 458  
#>  2 1       DIVISION 2       Yankees  57-45 .559  2.5   26-23 31-22 480  
#>  3 1       DIVISION 3       Red Sox  52-49 .515  7.0   23-28 29-21 426  
#>  4 1       DIVISION 4       Orioles  50-53 .485  10.0  28-25 22-28 472  
#>  5 1       DIVISION 5       Blue Ja… 46-56 .451  13.5  25-30 21-26 402  
#>  6 1       DIVISION NA      White S… 54-47 .535  -     31-17 23-30 483  
#>  7 1       DIVISION NA      Guardia… 54-49 .524  1.0   27-25 27-24 416  
#>  8 1       DIVISION NA      Twins    50-53 .485  5.0   26-25 24-28 495  
#>  9 1       DIVISION NA      Tigers   48-54 .471  6.5   27-23 21-31 433  
#> 10 1       DIVISION NA      Royals   43-60 .417  12.0  26-27 17-33 438  
#> # ℹ 80 more rows
#> # ℹ 14 more variables: ra <chr>, diff <chr>, l10 <chr>, strk <chr>,
#> #   entity_id <chr>, al_central <chr>, al_west <chr>, nl_east <chr>,
#> #   nl_central <chr>, nl_west <chr>, division_leaders <chr>,
#> #   wild_card <chr>, grapefruit_league <chr>, cactus_league <chr>
```
