# **MLB Sport IDs**

**MLB Sport IDs**

## Usage

``` r
mlb_sports(sport_id = NULL)
```

## Arguments

- sport_id:

  The sport_id to return information for.

## Value

Returns a tibble with the following columns

|                    |           |                                       |
|--------------------|-----------|---------------------------------------|
| col_name           | types     | description                           |
| sport_id           | integer   | MLBAM sport (level) identifier.       |
| sport_code         | character | Short sport code (e.g. 'mlb', 'aaa'). |
| sport_link         | character | API link to the sport resource.       |
| sport_name         | character | Full sport/level name.                |
| sport_abbreviation | character | Sport abbreviation (e.g. 'MLB').      |
| sort_order         | integer   | Display sort order for the sport.     |
| active_status      | logical   | Whether the sport/level is active.    |

and the following values:

|  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|
| sport_id | sport_code | sport_link | sport_name | sport_abbreviation | sort_order | active_status |
| 1 | mlb | /api/v1/sports/1 | Major League Baseball | MLB | 11 | TRUE |
| 11 | aaa | /api/v1/sports/11 | Triple-A | AAA | 101 | TRUE |
| 12 | aax | /api/v1/sports/12 | Double-A | AA | 201 | TRUE |
| 13 | afa | /api/v1/sports/13 | High-A | A+ | 301 | TRUE |
| 14 | afx | /api/v1/sports/14 | Low-A | A | 401 | TRUE |
| 16 | rok | /api/v1/sports/16 | Rookie | ROK | 701 | TRUE |
| 17 | win | /api/v1/sports/17 | Winter Leagues | WIN | 1301 | TRUE |
| 8 | bbl | /api/v1/sports/8 | Organized Baseball | Pros | 1401 | TRUE |
| 21 | min | /api/v1/sports/21 | Minor League Baseball | Minors | 1402 | TRUE |
| 23 | ind | /api/v1/sports/23 | Independent Leagues | IND | 2101 | TRUE |
| 51 | int | /api/v1/sports/51 | International Baseball | INT | 3501 | TRUE |
| 508 | nat | /api/v1/sports/508 | International Baseball (Collegiate) | INTC | 3502 | TRUE |
| 509 | nae | /api/v1/sports/509 | International Baseball (18 and under) | 18U | 3503 | TRUE |
| 510 | nas | /api/v1/sports/510 | International Baseball (16 and under) | 16U | 3505 | TRUE |
| 22 | bbc | /api/v1/sports/22 | College Baseball | College | 5101 | TRUE |
| 586 | hsb | /api/v1/sports/586 | High School Baseball | H.S. | 6201 | TRUE |

## Examples

``` r
# \donttest{
  try(mlb_sports())
#> ── MLB Sports data from MLB.com ───────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:25:08 UTC
#> # A tibble: 20 × 7
#>    sport_id sport_code sport_link          sport_name sport_abbreviation
#>       <int> <chr>      <chr>               <chr>      <chr>             
#>  1        1 mlb        /api/v1/sports/1    Major Lea… MLB               
#>  2       11 aaa        /api/v1/sports/11   Triple-A   AAA               
#>  3       12 aax        /api/v1/sports/12   Double-A   AA                
#>  4       13 afa        /api/v1/sports/13   High-A     A+                
#>  5       14 afx        /api/v1/sports/14   Single-A   A                 
#>  6       16 rok        /api/v1/sports/16   Rookie     ROK               
#>  7       17 win        /api/v1/sports/17   Winter Le… WIN               
#>  8       21 min        /api/v1/sports/21   Minor Lea… Minors            
#>  9       23 ind        /api/v1/sports/23   Independe… IND               
#> 10       61 nlb        /api/v1/sports/61   Negro Lea… NLB               
#> 11       32 kor        /api/v1/sports/32   Korean Ba… KOR               
#> 12       31 jml        /api/v1/sports/31   Nippon Pr… NPB               
#> 13       51 int        /api/v1/sports/51   Internati… INT               
#> 14      509 nae        /api/v1/sports/509  Internati… 18U               
#> 15      510 nas        /api/v1/sports/510  Internati… 16U               
#> 16     6005 ame        /api/v1/sports/6005 Internati… AME               
#> 17       52 oly        /api/v1/sports/52   Olympic B… OLY               
#> 18       22 bbc        /api/v1/sports/22   College B… College           
#> 19      586 hsb        /api/v1/sports/586  High Scho… H.S.              
#> 20      576 wps        /api/v1/sports/576  Women's P… WPS               
#> # ℹ 2 more variables: sort_order <int>, active_status <lgl>
# }
```
