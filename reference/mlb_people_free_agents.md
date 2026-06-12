# **Find Information About MLB Free Agents**

**Find Information About MLB Free Agents**

## Usage

``` r
mlb_people_free_agents(season = NULL)
```

## Arguments

- season:

  Season preceding free agency

## Value

Returns a tibble with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| date_declared | character | Date the player declared free agency (YYYY-MM-DD). |
| notes | character | Notes on the signing (e.g. 'One-year contract'). |
| date_signed | character | Date the player signed a new contract (YYYY-MM-DD). |
| sort_order | integer | Display sort order for the free agent record. |
| player_id | integer | MLB player id of the free agent. |
| player_full_name | character | Free agent full name. |
| player_link | character | API relative link to the player. |
| original_team_id | integer | Team id the player left. |
| original_team_name | character | Name of the team the player left. |
| original_team_link | character | API relative link to the original team. |
| position_code | character | Player position code. |
| position_name | character | Player position name (e.g. 'Relief Pitcher'). |
| position_type | character | Player position type (e.g. 'Pitcher'). |
| position_abbreviation | character | Player position abbreviation (e.g. 'RP'). |
| new_team_id | integer | Team id the player signed with. |
| new_team_name | character | Name of the team the player signed with. |
| new_team_link | character | API relative link to the new team. |

## Examples

``` r
# \donttest{
  try(mlb_people_free_agents(season = 2018))
#> ── MLB People - Free Agents data from MLB.com ─────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 14:09:44 UTC
#> # A tibble: 515 × 17
#>    notes date_signed date_declared sort_order player_id player_full_name
#>    <chr> <chr>       <chr>              <int>     <int> <chr>           
#>  1 Mino… 2018-12-17  2018-11-30            NA    592094 Jason Adam      
#>  2 One-… 2018-12-18  2018-10-29            NA    571431 Matt Adams      
#>  3 Mino… 2019-01-02  2018-10-24            NA    451192 James Adduci    
#>  4 Clai… 2019-03-01  2019-03-01            NA    593643 Hanser Alberto  
#>  5 NA    NA          2018-10-05            NA    593417 Raúl Alcántara  
#>  6 Trad… 2018-02-07  2018-02-07            NA    595751 Jorge Alfaro    
#>  7 One-… 2019-01-20  2018-10-29            NA    592102 Cody Allen      
#>  8 Mino… 2018-11-06  2018-07-18            NA    501659 Abraham Almonte 
#>  9 Trad… 2018-12-15  2018-12-15            NA    475174 Yonder Alonso   
#> 10 Trad… 2018-12-06  2018-12-06            NA    501625 Jose Alvarez    
#> # ℹ 505 more rows
#> # ℹ 11 more variables: player_link <chr>, original_team_id <int>,
#> #   original_team_name <chr>, original_team_link <chr>,
#> #   new_team_id <int>, new_team_name <chr>, new_team_link <chr>,
#> #   position_code <chr>, position_name <chr>, position_type <chr>,
#> #   position_abbreviation <chr>
# }
```
