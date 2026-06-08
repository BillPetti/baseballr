# **MLB API Language Options**

**MLB API Language Options**

## Usage

``` r
mlb_languages()
```

## Value

Returns a tibble with the following columns

|               |           |                                                  |
|---------------|-----------|--------------------------------------------------|
| col_name      | types     | description                                      |
| language_id   | integer   | Numeric language identifier.                     |
| language_code | character | Short language code (e.g. 'en', 'xe').           |
| language_name | character | Language display name (e.g. 'English', 'Elias'). |
| locale        | character | Locale string for the language (e.g. 'en_US').   |

## Examples

``` r
# \donttest{
  try(mlb_languages())
#> ── MLB Languages data from MLB.com ────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 03:45:57 UTC
#> # A tibble: 16 × 4
#>    language_id language_code language_name locale
#>          <int> <chr>         <chr>         <chr> 
#>  1           1 en            English       en_US 
#>  2           2 xe            Elias         en_US 
#>  3           3 es            Spanish       es_ES 
#>  4           4 xc            English       en_US 
#>  5           5 ja            Japanese      ja_JA 
#>  6           6 ko            Korean        ko_KO 
#>  7           7 zh            Chinese       zh_ZH 
#>  8           8 fr            French        fr_FR 
#>  9           9 de            German        de_DE 
#> 10          10 it            Italian       it_IT 
#> 11          11 nl            Dutch         nl_NL 
#> 12          12 pt            Portuguese    pt_PT 
#> 13          13 el            Greek         el_EL 
#> 14          14 tl            Tagalog       tl_TL 
#> 15          21 j2            Japanese 2    j2_J2 
#> 16          22 ov            English       en_US 
# }
```
