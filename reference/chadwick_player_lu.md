# **Download the Chadwick Bureau's public register of baseball players**

**Download the Chadwick Bureau's public register of baseball players**

## Usage

``` r
chadwick_player_lu()

get_chadwick_lu()
```

## Value

A data frame of baseball players and the various IDs associated with
them in different systems of record and the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| key_person | character | Chadwick Bureau primary person key. |
| key_uuid | character | Chadwick Bureau universally unique identifier for the person. |
| key_mlbam | integer | MLB Advanced Media (MLBAM) player ID. |
| key_retro | character | Retrosheet player ID. |
| key_bbref | character | Baseball-Reference major league player ID. |
| key_bbref_minors | character | Baseball-Reference minor league player ID. |
| key_fangraphs | integer | FanGraphs player ID. |
| key_npb | integer | Nippon Professional Baseball (Japan) player ID. |
| key_sr_nfl | character | Sports Reference NFL player ID. |
| key_sr_nba | character | Sports Reference NBA player ID. |
| key_sr_nhl | character | Sports Reference NHL player ID. |
| key_wikidata | character | Wikidata entity ID for the person. |
| name_last | character | Player last name. |
| name_first | character | Player first name. |
| name_given | character | Player full given (legal) name. |
| name_suffix | character | Name suffix (e.g. Jr., Sr., III). |
| name_matrilineal | character | Maternal surname, where recorded. |
| name_nick | character | Player nickname. |
| birth_year | integer | Year of birth. |
| birth_month | integer | Month of birth. |
| birth_day | integer | Day of birth. |
| death_year | integer | Year of death. |
| death_month | integer | Month of death. |
| death_day | integer | Day of death. |
| pro_played_first | integer | First season the person played professional baseball. |
| pro_played_last | integer | Last season the person played professional baseball. |
| mlb_played_first | integer | First MLB season as a player. |
| mlb_played_last | integer | Last MLB season as a player. |
| col_played_first | integer | First college season as a player. |
| col_played_last | integer | Last college season as a player. |
| pro_managed_first | integer | First professional season as a manager. |
| pro_managed_last | integer | Last professional season as a manager. |
| mlb_managed_first | integer | First MLB season as a manager. |
| mlb_managed_last | integer | Last MLB season as a manager. |
| col_managed_first | integer | First college season as a manager. |
| col_managed_last | integer | Last college season as a manager. |
| pro_umpired_first | integer | First professional season as an umpire. |
| pro_umpired_last | integer | Last professional season as an umpire. |
| mlb_umpired_first | integer | First MLB season as an umpire. |
| mlb_umpired_last | integer | Last MLB season as an umpire. |

A data frame of baseball players and the various IDs associated with
them in different systems of record.

## Examples

``` r
# \donttest{
  try(chadwick_player_lu())
#> ── Player Lookup from the Chadwick Bureau's public register of baseball 
#> ℹ Data updated: 2026-06-08 04:38:13 UTC
#> # A tibble: 516,106 × 40
#>    key_person key_uuid    key_mlbam key_retro key_bbref key_bbref_minors
#>    <chr>      <chr>           <int> <chr>     <chr>     <chr>           
#>  1 000007d9   000007d9-a…    472542 ""        ""        garcia001ado    
#>  2 000018b8   000018b8-a…    572962 ""        ""        kaupan001ste    
#>  3 00002834   00002834-f…    533242 ""        ""        smith-001ado    
#>  4 00002901   00002901-1…        NA ""        ""        decarl000lou    
#>  5 000045b3   000045b3-e…        NA ""        ""        spaine001har    
#>  6 00006177   00006177-b…        NA ""        ""        battle001elg    
#>  7 00007bd0   00007bd0-4…        NA ""        ""        desous000jam    
#>  8 0000815e   0000815e-3…        NA ""        ""        burr--001rob    
#>  9 0000ba28   0000ba28-8…        NA ""        ""        love--001jac    
#> 10 0000e73a   0000e73a-6…        NA ""        ""        hewitt000ale    
#> # ℹ 516,096 more rows
#> # ℹ 34 more variables: key_fangraphs <int>, key_npb <int>,
#> #   key_sr_nfl <chr>, key_sr_nba <chr>, key_sr_nhl <chr>,
#> #   key_wikidata <chr>, name_last <chr>, name_first <chr>,
#> #   name_given <chr>, name_suffix <chr>, name_matrilineal <chr>,
#> #   name_nick <chr>, birth_year <int>, birth_month <int>,
#> #   birth_day <int>, death_year <int>, death_month <int>, …
# }
```
