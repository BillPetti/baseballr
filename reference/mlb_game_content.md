# **Retrieve additional game content for major and minor league games**

**Retrieve additional game content for major and minor league games**

## Usage

``` r
mlb_game_content(game_pk)
```

## Arguments

- game_pk:

  The unique game_pk identifier for the game

## Value

Returns a tibble of game content data with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| title | character | Media/EPG title (e.g. MLBTV). |
| call_letters | character | Broadcaster call letters. |
| espn_auth_required | logical | Whether ESPN authentication is required. |
| tbs_auth_required | logical | Whether TBS authentication is required. |
| espn2auth_required | logical | Whether ESPN2 authentication is required. |
| game_date | character | Game date associated with the content. |
| content_id | character | Content identifier. |
| fs1auth_required | logical | Whether FS1 authentication is required. |
| media_id | character | Media identifier. |
| media_feed_type | character | Media feed type (HOME/AWAY/NATIONAL). |
| mlbn_auth_required | logical | Whether MLB Network authentication is required. |
| fox_auth_required | logical | Whether FOX authentication is required. |
| media_feed_sub_type | character | Media feed sub-type code. |
| free_game | logical | Whether the broadcast is a free game. |
| epg_id | integer | Electronic programming guide identifier. |
| media_state | character | Media state (e.g. MEDIA_ARCHIVE). |
| abc_auth_required | logical | Whether ABC authentication is required. |
| rendition_name | character | Media rendition name. |
| description | character | Content description. |
| language | character | Broadcast language. |
| type | character | Content/media type. |

## Examples

``` r
# \donttest{
  try(mlb_game_content(game_pk = 566001))
#> ── MLB Game Content data from MLB.com ─────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-09 20:43:33 UTC
#> # A tibble: 8 × 21
#>   title       call_letters espn_auth_required tbs_auth_required
#>   <chr>       <chr>        <lgl>              <lgl>            
#> 1 MLBTV       SNY          FALSE              FALSE            
#> 2 MLBTV       FSO          FALSE              FALSE            
#> 3 MLBTV-Audio NA           NA                 NA               
#> 4 MLBTV-Audio NA           NA                 NA               
#> 5 MLBTV-Audio NA           NA                 NA               
#> 6 Audio       WCBS 880     NA                 NA               
#> 7 Audio       WLW          NA                 NA               
#> 8 Audio       ESPN-1050    NA                 NA               
#> # ℹ 17 more variables: espn2auth_required <lgl>, game_date <chr>,
#> #   content_id <chr>, fs1auth_required <lgl>, media_id <chr>,
#> #   media_feed_type <chr>, mlbn_auth_required <lgl>,
#> #   fox_auth_required <lgl>, media_feed_sub_type <chr>,
#> #   free_game <lgl>, epg_id <int>, media_state <chr>,
#> #   abc_auth_required <lgl>, rendition_name <chr>, description <chr>,
#> #   language <chr>, type <chr>
# }
```
