# **Download a data frame of all umpires and their mlbamids for games since 2008**

**Download a data frame of all umpires and their mlbamids for games
since 2008**

## Usage

``` r
load_umpire_ids()
```

## Value

Function returns a tibble with the following columns:

- id

- position

- name

- game_pk

- game_date

## Examples

``` r
# \donttest{
  try(load_umpire_ids())
#> ── MLB Umpire IDs data from baseballr-data repository ──────────────────
#> ℹ Data updated: 2026-06-08 03:19:58 UTC
#> # A tibble: 140,609 × 5
#>        id position name           game_pk game_date 
#>     <int> <chr>    <chr>            <int> <IDate>   
#>  1 644760 3B       Adam Beck       662601 2022-09-07
#>  2 427533 2B       Mark Wegner     662601 2022-09-07
#>  3 429805 1B       Todd Tichenor   662601 2022-09-07
#>  4 427128 HP       Rob Drake       662601 2022-09-07
#>  5 573769 3B       Jeremy Riggs    662138 2022-09-07
#>  6 596809 2B       Ryan Additon    662138 2022-09-07
#>  7 484198 1B       Alan Porter     662138 2022-09-07
#>  8 511890 HP       Quinn Wolcott   662138 2022-09-07
#>  9 605670 3B       Dan Merzel      662313 2022-09-07
#> 10 594151 2B       Ramon De Jesus  662313 2022-09-07
#> # ℹ 140,599 more rows
# }
```
