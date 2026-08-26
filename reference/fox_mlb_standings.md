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
#> ℹ Data updated: 2026-08-26 21:30:05 UTC
#> # A tibble: 90 × 24
#>    team_id section  al_east v2       w_l   pct   gb    home  away  rs   
#>    <chr>   <chr>    <chr>   <chr>    <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 1       DIVISION 1       Rays     79-54 .594  -     43-23 36-31 595  
#>  2 1       DIVISION 2       Yankees  74-57 .565  4.0   33-29 41-28 586  
#>  3 1       DIVISION 3       Red Sox  73-59 .553  5.5   34-32 39-27 600  
#>  4 1       DIVISION 4       Orioles  64-68 .485  14.5  34-34 30-34 596  
#>  5 1       DIVISION 5       Blue Ja… 64-69 .481  15.0  33-34 31-35 524  
#>  6 1       DIVISION NA      White S… 69-63 .523  -     40-27 29-36 631  
#>  7 1       DIVISION NA      Guardia… 67-66 .504  2.5   31-34 36-32 539  
#>  8 1       DIVISION NA      Twins    64-69 .481  5.5   35-31 29-38 611  
#>  9 1       DIVISION NA      Tigers   62-71 .466  7.5   33-33 29-38 592  
#> 10 1       DIVISION NA      Royals   59-74 .444  10.5  36-30 23-44 565  
#> # ℹ 80 more rows
#> # ℹ 14 more variables: ra <chr>, diff <chr>, l10 <chr>, strk <chr>,
#> #   entity_id <chr>, al_central <chr>, al_west <chr>, nl_east <chr>,
#> #   nl_central <chr>, nl_west <chr>, division_leaders <chr>,
#> #   wild_card <chr>, grapefruit_league <chr>, cactus_league <chr>
```
