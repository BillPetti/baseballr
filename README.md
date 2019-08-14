
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/BillPetti/baseballr.svg?branch=master)](https://travis-ci.org/BillPetti/baseballr)
<!-- badges: end -->

# baseballr

# `baseballr` 0.5.0

**(latest version released 2019-06-25)**

`baseballr` is a package written for R focused on baseball analysis. It
includes functions for scraping various data from websites, such as
FanGraphs.com, Baseball-Reference.com, and baseballsavant.com. It also
includes functions for calculating metrics, such as wOBA, FIP, and
team-level consistency over custom time frames.

You can read more about some of the functions and how to use them at its
[official site](http://billpetti.github.io/baseballr/) as well as this
[Hardball Times
article](http://www.hardballtimes.com/developing-the-baseballr-package-for-r/).

## Installation

You can install `baseballr` from github with:

``` r
# install.packages("devtools")
devtools::install_github("BillPetti/baseballr")
```

For experimental functions in development, you can install the [development branch](https://github.com/BillPetti/baseballr/tree/development_branch):

``` r
# install.packages("devtools")
devtools::install_github("BillPetti/baseballr", ref = "development_branch")
```

## Pull Requests

Pull request are welcome, but I cannot guarantee that they will be accepted or accepted quickly. Please make all pull requests to the [development branch](https://github.com/BillPetti/baseballr/tree/development_branch) for review.

## Functionality

The package consists of two main sets of functions: data acquisition and
metric calculation.

For example, if you want to see the standings for a specific MLB
division on a given date, you can use the `standings_on_date_bref()`
function. Just pass the year, month, day, and division you want:

``` r
library(baseballr)
standings_on_date_bref("2015-08-01", "NL East", from = FALSE)
#> $`NL East_up to_2015-08-01`
#>    Tm  W  L  W-L%   GB  RS  RA pythW-L%
#> 1 WSN 54 48 0.529   -- 422 391    0.535
#> 2 NYM 54 50 0.519  1.0 368 373    0.494
#> 3 ATL 46 58 0.442  9.0 379 449    0.423
#> 4 MIA 42 62 0.404 13.0 370 408    0.455
#> 5 PHI 41 64 0.390 14.5 386 511    0.374
```

Right now the function works as far as back as 1994, which is when both
leagues split into three divisions.

You can also pull data for all hitters over a specific date range. Here
are the results for all hitters from August 1st through October 3rd
during the 2015 season:

``` r
library(dplyr)
data <- daily_batter_bref("2015-08-01", "2015-10-03") %>%
  head()
```

In terms of metric calculation, the package allows the user to calculate
the consistency of team scoring and run prevention for any year using
`team_consistency()`:

``` r
team_consistency(2015)
#> # A tibble: 30 x 5
#>    Team  Con_R Con_RA Con_R_Ptile Con_RA_Ptile
#>    <chr> <dbl>  <dbl>       <dbl>        <dbl>
#>  1 ARI    0.37   0.39          43           80
#>  2 ATL    0.38   0.36          65           25
#>  3 BAL    0.4    0.36          88           25
#>  4 BOS    0.37   0.39          43           80
#>  5 CHC    0.41   0.37          97           50
#>  6 CHW    0.38   0.36          65           25
#>  7 CIN    0.36   0.38          22           63
#>  8 CLE    0.38   0.42          65          100
#>  9 COL    0.38   0.38          65           63
#> 10 DET    0.39   0.35          82            3
#> # … with 20 more rows
```

You can also calculate wOBA per plate appearance and wOBA on contact for
any set of data over any date range, provided you have the data
available.

Simply pass the proper data frame to `woba_plus`:

``` r
data %>%
  filter(PA > 200) %>%
  woba_plus %>%
  arrange(desc(wOBA)) %>%
  select(Name, Team, season, PA, wOBA, wOBA_CON) %>%
  head()
#>               Name          Team season  PA  wOBA wOBA_CON
#> 1    Shin-Soo Choo         Texas   2015 260 0.430    0.495
#> 2 Francisco Lindor     Cleveland   2015 259 0.404    0.468
#> 3      Jose Altuve       Houston   2015 262 0.372    0.406
#> 4       Adam Eaton       Chicago   2015 262 0.367    0.436
#> 5    Manny Machado     Baltimore   2015 266 0.362    0.396
#> 6       Matt Duffy San Francisco   2015 264 0.312    0.338
```

You can also generate these wOBA-based stats, as well as FIP, for
pitchers using the `fip_plus()` function:

``` r
daily_pitcher_bref("2015-04-05", "2015-04-30") %>% 
  fip_plus() %>% 
  select(season, Name, IP, ERA, SO, uBB, HBP, HR, FIP, wOBA_against, wOBA_CON_against) %>%
  arrange(desc(IP)) %>% 
  head(10)
#>    season            Name   IP  ERA SO uBB HBP HR  FIP wOBA_against
#> 1    2015    Johnny Cueto 37.0 1.95 38   4   2  3 2.62        0.210
#> 2    2015  Dallas Keuchel 37.0 0.73 22  11   0  0 2.84        0.169
#> 3    2015      Sonny Gray 36.1 1.98 25   6   1  1 2.69        0.218
#> 4    2015      Mike Leake 35.2 3.03 25   7   0  5 4.16        0.240
#> 5    2015 Felix Hernandez 34.2 1.82 36   6   3  1 2.20        0.225
#> 6    2015    Corey Kluber 34.0 4.24 36   5   2  2 2.40        0.295
#> 7    2015   Jake Odorizzi 33.2 2.41 26   8   1  0 2.38        0.213
#> 8    2015 Josh Collmenter 32.2 2.76 16   3   0  1 2.82        0.290
#> 9    2015   Bartolo Colon 32.2 3.31 25   1   0  4 3.29        0.280
#> 10   2015    Zack Greinke 32.2 1.93 27   7   1  2 3.01        0.240
#>    wOBA_CON_against
#> 1             0.276
#> 2             0.151
#> 3             0.239
#> 4             0.281
#> 5             0.272
#> 6             0.391
#> 7             0.228
#> 8             0.330
#> 9             0.357
#> 10            0.274
```

The `edge_scrape()` function allows the user to scrape PITCHf/x data
from the GameDay application using Carson Sievert’s
[pitchRx](https://github.com/cpsievert/pitchRx) package and to calculate
metrics associated with
[Edge%](https://billpetti.shinyapps.io/edge_shiny/). The function
returns a dataframe grouped by either pitchers or batters and the
percentge of pitches in each of the various Edge zones.

Example (pitchers):

``` r
edge_scrape("2015-04-06", "2015-04-07", "pitcher") %>% 
  select(-6:-4, -13) %>% 
  head(10)
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_tormlb_nyamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_minmlb_detmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_colmlb_milmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_bosmlb_phimlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_balmlb_tbamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_nynmlb_wasmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_atlmlb_miamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_chamlb_kcamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_anamlb_seamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_pitmlb_cinmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_sdnmlb_lanmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_clemlb_houmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_texmlb_oakmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_sfnmlb_arimlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_atlmlb_miamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_balmlb_tbamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_colmlb_milmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_sfnmlb_arimlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_texmlb_oakmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_anamlb_seamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_sdnmlb_lanmlb_1/inning/inning_all.xml
#> # A tibble: 10 x 9
#>    pitcher_name pitcher All_pitches Upper_Edge Lower_Edge Inside_Edge
#>    <chr>          <dbl>       <int>      <dbl>      <dbl>       <dbl>
#>  1 Bartolo Col…  112526          86      0.035      0.07        0.058
#>  2 LaTroy Hawk…  115629          12      0.083      0.333       0    
#>  3 Joe Nathan    150274           4      0          0           0    
#>  4 Buddy Carly…  234194           9      0          0.222       0    
#>  5 Jason Grilli  276351          14      0          0           0.214
#>  6 Kevin Gregg   276514          17      0          0.059       0.118
#>  7 Joaquin Ben…  276542          19      0          0           0.158
#>  8 Ryan Vogels…  285064          99      0.01       0.071       0.141
#>  9 Jeremy Affe…  346793           5      0          0           0.4  
#> 10 Grant Balfo…  346797          21      0.095      0           0    
#> # … with 3 more variables: Outside_Edge <dbl>, Heart <dbl>,
#> #   Out_of_Zone <dbl>
```

Example (batters):

``` r
edge_scrape("2015-04-06", "2015-04-07", "batter") %>% 
  select(-6:-4, -13) %>% 
  head(10)
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_tormlb_nyamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_minmlb_detmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_colmlb_milmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_bosmlb_phimlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_balmlb_tbamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_nynmlb_wasmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_atlmlb_miamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_chamlb_kcamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_anamlb_seamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_pitmlb_cinmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_sdnmlb_lanmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_clemlb_houmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_texmlb_oakmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_06/gid_2015_04_06_sfnmlb_arimlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_atlmlb_miamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_balmlb_tbamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_colmlb_milmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_sfnmlb_arimlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_texmlb_oakmlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_anamlb_seamlb_1/inning/inning_all.xml 
#> http://gd2.mlb.com/components/game/mlb/year_2015/month_04/day_07/gid_2015_04_07_sdnmlb_lanmlb_1/inning/inning_all.xml
#> # A tibble: 10 x 9
#>    batter_name batter All_pitches Upper_Edge Lower_Edge Inside_Edge
#>    <chr>        <dbl>       <int>      <dbl>      <dbl>       <dbl>
#>  1 Bartolo Co… 112526           7      0          0           0.429
#>  2 Torii Hunt… 116338          19      0          0.158       0.105
#>  3 David Ortiz 120074          18      0          0           0.111
#>  4 Alex Rodri… 121347          17      0          0           0.353
#>  5 Aramis Ram… 133380          23      0          0.087       0.217
#>  6 Adrian Bel… 134181          26      0          0.038       0.154
#>  7 Carlos Bel… 136860          22      0.091      0           0.136
#>  8 Michael Cu… 150212          14      0          0.143       0.143
#>  9 Jimmy Roll… 276519          41      0.024      0.146       0.049
#> 10 Ryan Vogel… 285064          10      0          0.1         0.3  
#> # … with 3 more variables: Outside_Edge <dbl>, Heart <dbl>,
#> #   Out_of_Zone <dbl>
```

More functionality will be added soon. Please leave any suggestions or
bugs in the [Issues
section](https://github.com/BillPetti/baseballr/issues).
