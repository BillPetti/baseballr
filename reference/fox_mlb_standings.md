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
#> ℹ Data updated: 2026-09-26 06:30:06 UTC
#> # A tibble: 90 × 24
#>    team_id section  al_east v2       w_l   pct   gb    home  away  rs   
#>    <chr>   <chr>    <chr>   <chr>    <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 1       DIVISION 1       Rays     97-63 .606  -     55-26 42-37 721  
#>  2 1       DIVISION 2       Yankees  93-68 .578  4.5   46-34 47-34 739  
#>  3 1       DIVISION 3       Red Sox  87-74 .540  10.5  41-39 46-35 687  
#>  4 1       DIVISION 4       Orioles  79-82 .491  18.5  39-42 40-40 718  
#>  5 1       DIVISION 5       Blue Ja… 78-82 .488  19.0  41-38 37-44 642  
#>  6 1       DIVISION NA      Guardia… 84-76 .525  -     41-40 43-36 665  
#>  7 1       DIVISION NA      White S… 83-77 .519  1.0   47-32 36-45 766  
#>  8 1       DIVISION NA      Twins    76-84 .475  8.0   41-38 35-46 731  
#>  9 1       DIVISION NA      Tigers   75-85 .469  9.0   41-38 34-47 717  
#> 10 1       DIVISION NA      Royals   68-92 .425  16.0  40-39 28-53 682  
#> # ℹ 80 more rows
#> # ℹ 14 more variables: ra <chr>, diff <chr>, l10 <chr>, strk <chr>,
#> #   entity_id <chr>, al_central <chr>, al_west <chr>, nl_east <chr>,
#> #   nl_central <chr>, nl_west <chr>, division_leaders <chr>,
#> #   wild_card <chr>, grapefruit_league <chr>, cactus_league <chr>
```
