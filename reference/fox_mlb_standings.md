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
#> ℹ Data updated: 2026-09-27 04:37:16 UTC
#> # A tibble: 90 × 24
#>    team_id section  al_east v2       w_l   pct   gb    home  away  rs   
#>    <chr>   <chr>    <chr>   <chr>    <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 1       DIVISION 1       Rays     98-63 .609  -     55-26 43-37 733  
#>  2 1       DIVISION 2       Yankees  93-68 .578  5.0   46-34 47-34 739  
#>  3 1       DIVISION 3       Red Sox  87-74 .540  11.0  41-39 46-35 687  
#>  4 1       DIVISION 4       Orioles  79-82 .491  19.0  39-42 40-40 718  
#>  5 1       DIVISION 5       Blue Ja… 78-83 .484  20.0  41-39 37-44 643  
#>  6 1       DIVISION NA      Guardia… 85-76 .528  -     41-40 44-36 676  
#>  7 1       DIVISION NA      White S… 83-78 .516  2.0   47-33 36-45 772  
#>  8 1       DIVISION NA      Twins    76-85 .472  9.0   41-39 35-46 733  
#>  9 1       DIVISION NA      Tigers   76-85 .472  9.0   42-38 34-47 721  
#> 10 1       DIVISION NA      Royals   68-93 .422  17.0  40-40 28-53 687  
#> # ℹ 80 more rows
#> # ℹ 14 more variables: ra <chr>, diff <chr>, l10 <chr>, strk <chr>,
#> #   entity_id <chr>, al_central <chr>, al_west <chr>, nl_east <chr>,
#> #   nl_central <chr>, nl_west <chr>, division_leaders <chr>,
#> #   wild_card <chr>, grapefruit_league <chr>, cactus_league <chr>
```
