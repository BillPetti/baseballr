# **View all PCL conferences**

**View all PCL conferences**

## Usage

``` r
mlb_conferences(conference_id = NULL, season = NULL)
```

## Arguments

- conference_id:

  Conference ID to return information for.

- season:

  Year to return to return conference information for.

## Value

Returns a tibble with the following columns

|                         |           |                                         |
|-------------------------|-----------|-----------------------------------------|
| col_name                | types     | description                             |
| conference_id           | integer   | MLB conference ID.                      |
| conference_name         | character | Conference name.                        |
| link                    | character | MLB Stats API relative conference link. |
| conference_abbreviation | character | Conference abbreviation.                |
| has_wildcard            | logical   | Whether the conference has a wild card. |
| conference_name_short   | character | Short conference name.                  |
| league_id               | integer   | MLB league ID.                          |
| league_link             | character | MLB Stats API relative league link.     |
| sport_id                | integer   | MLB sport ID.                           |
| sport_link              | character | MLB Stats API relative sport link.      |

## Examples

``` r
# \donttest{
  try(mlb_conferences())
#> ── MLB Conferences data from MLB.com ──────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 11:08:48 UTC
#> # A tibble: 2 × 10
#>   conference_id conference_name         link      conference_abbreviat…¹
#>           <int> <chr>                   <chr>     <chr>                 
#> 1           301 PCL American Conference /api/v1/… PCLA                  
#> 2           302 PCL Pacific Conference  /api/v1/… PCLP                  
#> # ℹ abbreviated name: ¹​conference_abbreviation
#> # ℹ 6 more variables: has_wildcard <lgl>, conference_name_short <chr>,
#> #   league_id <int>, league_link <chr>, sport_id <int>,
#> #   sport_link <chr>
  try(mlb_conferences(conference_id =  301, season = 2020))
#> ── MLB Conferences data from MLB.com ──────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 11:08:48 UTC
#> # A tibble: 1 × 10
#>   conference_id conference_name         link      conference_abbreviat…¹
#>           <int> <chr>                   <chr>     <chr>                 
#> 1           301 PCL American Conference /api/v1/… PCLA                  
#> # ℹ abbreviated name: ¹​conference_abbreviation
#> # ℹ 6 more variables: has_wildcard <lgl>, conference_name_short <chr>,
#> #   league_id <int>, league_link <chr>, sport_id <int>,
#> #   sport_link <chr>
# }
```
