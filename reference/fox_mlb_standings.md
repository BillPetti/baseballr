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
#> ℹ Data updated: 2026-09-10 00:15:39 UTC
#> # A tibble: 90 × 24
#>    team_id section  al_east v2       w_l   pct   gb    home  away  rs   
#>    <chr>   <chr>    <chr>   <chr>    <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 1       DIVISION 1       Rays     86-58 .597  -     47-25 39-33 654  
#>  2 1       DIVISION 2       Yankees  82-62 .569  4.0   38-31 44-31 651  
#>  3 1       DIVISION 3       Red Sox  80-66 .548  7.0   36-35 44-31 648  
#>  4 1       DIVISION 4       Blue Ja… 73-74 .497  14.5  37-35 36-39 589  
#>  5 1       DIVISION 5       Orioles  70-76 .479  17.0  35-39 35-37 652  
#>  6 1       DIVISION NA      White S… 75-69 .521  -     43-29 32-40 683  
#>  7 1       DIVISION NA      Guardia… 74-72 .507  2.0   36-39 38-33 593  
#>  8 1       DIVISION NA      Twins    69-77 .473  7.0   38-34 31-43 675  
#>  9 1       DIVISION NA      Tigers   67-79 .459  9.0   36-36 31-43 641  
#> 10 1       DIVISION NA      Royals   64-82 .438  12.0  38-36 26-46 624  
#> # ℹ 80 more rows
#> # ℹ 14 more variables: ra <chr>, diff <chr>, l10 <chr>, strk <chr>,
#> #   entity_id <chr>, al_central <chr>, al_west <chr>, nl_east <chr>,
#> #   nl_central <chr>, nl_west <chr>, division_leaders <chr>,
#> #   wild_card <chr>, grapefruit_league <chr>, cactus_league <chr>
```
