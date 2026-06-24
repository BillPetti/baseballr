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
#> ℹ Data updated: 2026-06-24 02:05:38 UTC
#> # A tibble: 90 × 24
#>    team_id section  al_east v2       w_l   pct   gb    home  away  rs   
#>    <chr>   <chr>    <chr>   <chr>    <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 1       DIVISION 1       Yankees  47-31 .603  -     22-15 25-16 392  
#>  2 1       DIVISION 2       Rays     43-33 .566  3.0   26-12 17-21 333  
#>  3 1       DIVISION 3       Blue Ja… 39-40 .494  8.5   22-19 17-21 327  
#>  4 1       DIVISION 4       Orioles  38-42 .475  10.0  22-19 16-23 375  
#>  5 1       DIVISION 5       Red Sox  31-45 .408  15.0  12-25 19-20 296  
#>  6 1       DIVISION NA      White S… 40-37 .519  -     25-12 15-25 359  
#>  7 1       DIVISION NA      Guardia… 41-38 .519  -     19-17 22-21 315  
#>  8 1       DIVISION NA      Twins    38-42 .475  3.5   20-20 18-22 389  
#>  9 1       DIVISION NA      Tigers   34-45 .430  7.0   22-17 12-28 319  
#> 10 1       DIVISION NA      Royals   34-46 .425  7.5   19-22 15-24 345  
#> # ℹ 80 more rows
#> # ℹ 14 more variables: ra <chr>, diff <chr>, l10 <chr>, strk <chr>,
#> #   entity_id <chr>, al_central <chr>, al_west <chr>, nl_east <chr>,
#> #   nl_central <chr>, nl_west <chr>, division_leaders <chr>,
#> #   wild_card <chr>, grapefruit_league <chr>, cactus_league <chr>
```
