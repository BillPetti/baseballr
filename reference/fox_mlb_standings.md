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
#> ℹ Data updated: 2026-09-30 14:28:03 UTC
#> # A tibble: 90 × 24
#>    team_id section  al_east v2       w_l   pct   gb    home  away  rs   
#>    <chr>   <chr>    <chr>   <chr>    <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 1       DIVISION 1       Rays     98-64 .605  -     55-26 43-38 736  
#>  2 1       DIVISION 2       Yankees  93-68 .578  4.5   46-34 47-34 739  
#>  3 1       DIVISION 3       Red Sox  87-75 .537  11.0  41-40 46-35 689  
#>  4 1       DIVISION 4       Orioles  79-82 .491  18.5  39-42 40-40 718  
#>  5 1       DIVISION 5       Blue Ja… 79-83 .488  19.0  42-39 37-44 648  
#>  6 1       DIVISION NA      Guardia… 85-77 .525  -     41-40 44-37 678  
#>  7 1       DIVISION NA      White S… 84-78 .519  1.0   48-33 36-45 776  
#>  8 1       DIVISION NA      Twins    77-85 .475  8.0   42-39 35-46 739  
#>  9 1       DIVISION NA      Tigers   76-86 .469  9.0   42-39 34-47 723  
#> 10 1       DIVISION NA      Royals   69-93 .426  16.0  41-40 28-53 690  
#> # ℹ 80 more rows
#> # ℹ 14 more variables: ra <chr>, diff <chr>, l10 <chr>, strk <chr>,
#> #   entity_id <chr>, al_central <chr>, al_west <chr>, nl_east <chr>,
#> #   nl_central <chr>, nl_west <chr>, division_leaders <chr>,
#> #   wild_card <chr>, grapefruit_league <chr>, cactus_league <chr>
```
