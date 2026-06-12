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
#> ℹ Data updated: 2026-06-12 14:09:05 UTC
#> # A tibble: 90 × 24
#>    team_id section  al_east v2       w_l   pct   gb    home  away  rs   
#>    <chr>   <chr>    <chr>   <chr>    <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 1       DIVISION 1       Rays     40-25 .615  -     24-9  16-16 297  
#>  2 1       DIVISION 2       Yankees  41-26 .612  -     19-12 22-14 342  
#>  3 1       DIVISION 3       Blue Ja… 33-36 .478  9.0   20-16 13-20 281  
#>  4 1       DIVISION 4       Orioles  33-37 .471  9.5   21-17 12-20 331  
#>  5 1       DIVISION 5       Red Sox  27-39 .409  13.5  10-21 17-18 258  
#>  6 1       DIVISION NA      White S… 36-31 .537  -     22-11 14-20 318  
#>  7 1       DIVISION NA      Guardia… 37-33 .529  0.5   17-17 20-16 283  
#>  8 1       DIVISION NA      Twins    31-39 .443  6.5   18-18 13-21 318  
#>  9 1       DIVISION NA      Tigers   29-40 .420  8.0   18-16 11-24 285  
#> 10 1       DIVISION NA      Royals   28-41 .406  9.0   16-19 12-22 269  
#> # ℹ 80 more rows
#> # ℹ 14 more variables: ra <chr>, diff <chr>, l10 <chr>, strk <chr>,
#> #   entity_id <chr>, al_central <chr>, al_west <chr>, nl_east <chr>,
#> #   nl_central <chr>, nl_west <chr>, division_leaders <chr>,
#> #   wild_card <chr>, grapefruit_league <chr>, cactus_league <chr>
```
