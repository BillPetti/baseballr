
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg?style=for-the-badge&logo=github)
![R-CMD-check](https://img.shields.io/github/workflow/status/SaiemGilani/baseballr/R-CMD-check?label=R-CMD-Check&logo=R&logoColor=blue&style=for-the-badge)
![Contributors](https://img.shields.io/github/contributors/BillPetti/baseballr?style=for-the-badge)
![Version-Number](https://img.shields.io/github/r-package/v/BillPetti/baseballr?label=baseballr&logo=R&style=for-the-badge)
[![Twitter
Follow](https://img.shields.io/twitter/follow/BillPetti?color=blue&label=%40baseballr&logo=twitter&style=for-the-badge)](https://twitter.com/BillPetti)
[![Travis build
status](https://travis-ci.org/BillPetti/baseballr.svg?branch=master)](https://travis-ci.org/BillPetti/baseballr)
<!-- badges: end -->

# baseballr

# `baseballr` 0.6.0

**(latest version released 2020-01-07)**

`baseballr` is a package written for R focused on baseball analysis. It
includes functions for scraping various data from websites, such as
FanGraphs.com, Baseball-Reference.com, and baseballsavant.com. It also
includes functions for calculating metrics, such as wOBA, FIP, and
team-level consistency over custom time frames.

You can read more about some of the functions and how to use them at its
[official site](http://billpetti.github.io/baseballr/) as well as this
[Hardball Times
article](http://www.hardballtimes.com/developing-the-baseballr-package-for-r/).

## **Installation**

You can install the released version of
[**`baseballr`**](https://github.com/BillPetti/baseballr/) from
[GitHub](https://github.com/BillPetti/baseballr) with:

``` r
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("saiemgilani/baseballr")
```

``` r
# if you would prefer devtools installation
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
# Alternatively, using the devtools package:
devtools::install_github(repo = "BillPetti/baseballr")
```

For experimental functions in development, you can install the
[development
branch](https://github.com/BillPetti/baseballr/tree/development_branch):

``` r
# install.packages("devtools")
devtools::install_github("BillPetti/baseballr", ref = "development_branch")
```

## Pull Requests

Pull request are welcome, but I cannot guarantee that they will be
accepted or accepted quickly. Please make all pull requests to the
[development
branch](https://github.com/BillPetti/baseballr/tree/development_branch)
for review.

## Functionality

The package consists of two main sets of functions: data acquisition and
metric calculation.

For example, if you want to see the standings for a specific MLB
division on a given date, you can use the `standings_on_date_bref()`
function. Just pass the year, month, day, and division you want:

``` r
library(baseballr)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
standings_on_date_bref("2015-08-01", "NL East", from = FALSE)
#> Data courtesy of Baseball-Reference.com. Please consider supporting Baseball-Reference by signing up for a Stathead account: https://stathead.com
#> # A tibble: 5 x 8
#>   Tm        W     L `W-L%` GB       RS    RA `pythW-L%`
#>   <chr> <int> <int>  <dbl> <chr> <int> <int>      <dbl>
#> 1 WSN      54    48  0.529 --      422   391      0.535
#> 2 NYM      54    50  0.519 1.0     368   373      0.494
#> 3 ATL      46    58  0.442 9.0     379   449      0.423
#> 4 MIA      42    62  0.404 13.0    370   408      0.455
#> 5 PHI      41    64  0.39  14.5    386   511      0.374
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
#> Data courtesy of Baseball-Reference.com. Please consider supporting Baseball-Reference by signing up for a Statehead account: https://stathead.com
```

In terms of metric calculation, the package allows the user to calculate
the consistency of team scoring and run prevention for any year using
`team_consistency()`:

``` r
team_consistency(2015)
#> Data courtesy of Baseball-Reference.com. Please consider supporting Baseball-Reference by signing up for a Stathead account: https://stathead.com
#> [1] "ARI"
#> [1] "ATL"
#> [1] "BAL"
#> [1] "BOS"
#> [1] "CHC"
#> [1] "CHW"
#> [1] "CIN"
#> [1] "CLE"
#> [1] "COL"
#> [1] "DET"
#> [1] "HOU"
#> [1] "KCR"
#> [1] "LAA"
#> [1] "LAD"
#> [1] "MIA"
#> [1] "MIL"
#> [1] "MIN"
#> [1] "NYM"
#> [1] "NYY"
#> [1] "OAK"
#> [1] "PHI"
#> [1] "PIT"
#> [1] "SDP"
#> [1] "SEA"
#> [1] "SFG"
#> [1] "STL"
#> [1] "TBR"
#> [1] "TEX"
#> [1] "TOR"
#> [1] "WSN"
#> # A tibble: 30 x 5
#>    Team  Con_R Con_RA Con_R_Ptile Con_RA_Ptile
#>    <chr> <dbl>  <dbl>       <dbl>        <dbl>
#>  1 ARI    0.37   0.36          17           15
#>  2 ATL    0.41   0.4           88           63
#>  3 BAL    0.4    0.38          70           42
#>  4 BOS    0.39   0.4           52           63
#>  5 CHC    0.38   0.41          30           85
#>  6 CHW    0.39   0.4           52           63
#>  7 CIN    0.41   0.36          88           15
#>  8 CLE    0.41   0.4           88           63
#>  9 COL    0.35   0.34           7            3
#> 10 DET    0.39   0.38          52           42
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
#> Data courtesy of Baseball-Reference.com. Please consider supporting Baseball-Reference by signing up for a Statehead account: https://stathead.com
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
library(dplyr)
edge_scrape("2015-04-06", "2015-04-07", "pitcher") %>% 
  select(-6:-4, -13) %>% 
  head(10)
```

Example (batters):

``` r
edge_scrape("2015-04-06", "2015-04-07", "batter") %>% 
  select(-6:-4, -13) %>% 
  head(10)
```

More functionality will be added soon. Please leave any suggestions or
bugs in the [Issues
section](https://github.com/BillPetti/baseballr/issues).

# Current Issues

| issue | icon                                                                                                                         | title                                                                                                                                                                                                                                                                                                                                                                                                                                                | labels | opened\_by                                                            | date       | closed |
| :---- | :--------------------------------------------------------------------------------------------------------------------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :----- | :-------------------------------------------------------------------- | :--------- | :----- |
| 209   | <span title="Open Issue"><img src="https://github.com/yonicd/issue/blob/master/inst/icons/issue-opened.png?raw=true"></span> | <span title="Attempting to join Savant queried data with get_game_info_sup_petti() (particularly interested in getting temperature and venue) and it seems that there are 23 game_pks misisng from get_game_info_sup_petti().  ...">[get\_game\_info\_sup\_petti seems to be missing 23 game\_pks from 2015-2021](https://github.com/BillPetti/baseballr/issues/209)</span>                                                                          |        | [mjona99](https://github.com/mjona99)                                 | 2021-06-20 | NA     |
| 206   | <span title="Open Issue"><img src="https://github.com/yonicd/issue/blob/master/inst/icons/issue-opened.png?raw=true"></span> | <span title="I&#39;m unable to successfully execute baseballr functions due to what may be a rookie error. The error reads:...">[withCallingHandlers list object error](https://github.com/BillPetti/baseballr/issues/206)</span>                                                                                                                                                                                                                    |        | [Jeffrey-Roberts-Osborne](https://github.com/Jeffrey-Roberts-Osborne) | 2021-04-22 | NA     |
| 208   | <span title="Open Issue"><img src="https://github.com/yonicd/issue/blob/master/inst/icons/issue-opened.png?raw=true"></span> | <span title="Only for years 2019-2021.  Works fine for 2018 and beyond......">[get\_ncaa\_schedule\_info() returns the “RPI Ranking” Header as score and inning data…](https://github.com/BillPetti/baseballr/issues/208)</span>                                                                                                                                                                                                                     | bug    | [supernole1](https://github.com/supernole1)                           | 2021-05-23 | NA     |
| 207   | <span title="Open Issue"><img src="https://github.com/yonicd/issue/blob/master/inst/icons/issue-opened.png?raw=true"></span> | <span title="When calling daily_batter_bref(today-14, today) the data pulls back but the bbref_id does not match the player name....">[bbref\_id incorrect in daily\_batter\_bref()](https://github.com/BillPetti/baseballr/issues/207)</span>                                                                                                                                                                                                       |        | [danielmcintosh1](https://github.com/danielmcintosh1)                 | 2021-04-24 | NA     |
| 202   | <span title="Open Issue"><img src="https://github.com/yonicd/issue/blob/master/inst/icons/issue-opened.png?raw=true"></span> | <span title="I followed the post here(https://billpetti.github.io/2020-05-26-build-statcast-database-rstats-version-2.0/) in order to create a PostgreSQL Statcast database.  I also followed the instructions to index.  Simple summary functions are taking 93 seconds.  Is there a way to fix/improve the Statcast database performance?...">[Speeding up slow Statcast Database query](https://github.com/BillPetti/baseballr/issues/202)</span> |        | [jestarr](https://github.com/jestarr)                                 | 2021-04-08 | NA     |

<details>

<summary>View More</summary>

| issue | icon                                                                                                                           | title                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | labels | opened\_by                                  | date       | closed              |
| :---- | :----------------------------------------------------------------------------------------------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :----- | :------------------------------------------ | :--------- | :------------------ |
| 198   | <span title="Closed Issue"><img src="https://github.com/yonicd/issue/blob/master/inst/icons/issue-closed.png?raw=true"></span> | <span title="Recently, the scrape_statcast_savant function has not been working for me. When I try to pull play-by-play data with code such as the following:...">[Issue with scrape\_statcast\_savant](https://github.com/BillPetti/baseballr/issues/198)</span>                                                                                                                                                                                                                                                                                                                                                 |        | [mcanmann20](https://github.com/mcanmann20) | 2021-04-03 | 2021-04-20 14:01:44 |
| 200   | <span title="Closed Issue"><img src="https://github.com/yonicd/issue/blob/master/inst/icons/issue-closed.png?raw=true"></span> | <span title="Running any of the scrape_statcast_savant functions populates the fielding alignment columns with NA. This is something I only noticed recently. It was returning proper characters as recently as last month, but when running the same code today, the columns were NA. I reinstalled the most recent version of baseballr (and dependencies) and that didn&#39;t resolve the issue. I also tried with multiple player IDs. Thanks!">[scrape\_statcast\_savant() populating of\_fielding\_alignment and if\_fielding\_alignment with NA](https://github.com/BillPetti/baseballr/issues/200)</span> |        | [maxbay](https://github.com/maxbay)         | 2021-04-05 | 2021-04-20 14:01:33 |
| 201   | <span title="Closed Issue"><img src="https://github.com/yonicd/issue/blob/master/inst/icons/issue-closed.png?raw=true"></span> | <span title="Hi Bill,...">[pitcher\_boxscore](https://github.com/BillPetti/baseballr/issues/201)</span>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |        | [timecsvk](https://github.com/timecsvk)     | 2021-04-07 | 2021-04-20 14:01:21 |
| 199   | <span title="Closed Issue"><img src="https://github.com/yonicd/issue/blob/master/inst/icons/issue-closed.png?raw=true"></span> | <span title="Whenever I try to run the code to retrieve the pbp data for the mlb, i get this error:...">[Error in get\_pbp\_mlb data](https://github.com/BillPetti/baseballr/issues/199)</span>                                                                                                                                                                                                                                                                                                                                                                                                                   |        | [joeytitus](https://github.com/joeytitus)   | 2021-04-03 | 2021-04-03 18:27:43 |
| 192   | <span title="Closed Issue"><img src="https://github.com/yonicd/issue/blob/master/inst/icons/issue-closed.png?raw=true"></span> | <span title="The following code is returning the error: &#39;Error: Can&#39;t recycle `..1` (size 38) to match `..2` (size 76).&#39;...">[get\_ncaa\_baseball\_roster issue](https://github.com/BillPetti/baseballr/issues/192)</span>                                                                                                                                                                                                                                                                                                                                                                            |        | [jdrnym](https://github.com/jdrnym)         | 2021-03-27 | 2021-04-03 18:25:11 |

</details>

<br>

# **Our Authors**

  - [Bill Petti](https://twitter.com/BillPetti)  
    <a href="https://twitter.com/BillPetti" target="blank"><img src="https://img.shields.io/twitter/follow/BillPetti?color=blue&label=%40BillPetti&logo=twitter&style=for-the-badge" alt="@BillPetti" /></a>
    <a href="https://github.com/BillPetti" target="blank"><img src="https://img.shields.io/github/followers/BillPetti?color=eee&logo=Github&style=for-the-badge" alt="@BillPetti" /></a>

# **Our Contributors (they’re awesome)**

  - [Ben Baumer](https://twitter.com/BaumerBen)  
    <a href="https://twitter.com/BaumerBen" target="blank"><img src="https://img.shields.io/twitter/follow/BaumerBen?color=blue&label=%40BaumerBen&logo=twitter&style=for-the-badge" alt="@BaumerBen" /></a>
    <a href="https://github.com/beanumber" target="blank"><img src="https://img.shields.io/github/followers/beanumber?color=eee&logo=Github&style=for-the-badge" alt="@beanumber" /></a>

  - [Ben Dilday](https://twitter.com/BenDilday)  
    <a href="https://twitter.com/BenDilday" target="blank"><img src="https://img.shields.io/twitter/follow/BenDilday?color=blue&label=%40BenDilday&logo=twitter&style=for-the-badge" alt="@BenDilday" /></a>
    <a href="https://github.com/bdilday" target="blank"><img src="https://img.shields.io/github/followers/bdilday?color=eee&logo=Github&style=for-the-badge" alt="@bdilday" /></a>

  - [Robert Frey](https://twitter.com/RobertFrey40)  
    <a href="https://twitter.com/RobertFrey40" target="blank"><img src="https://img.shields.io/twitter/follow/RobertFrey40?color=blue&label=%40RobertFrey40&logo=twitter&style=for-the-badge" alt="@RobertFrey40" /></a>
    <a href="https://github.com/robert-frey" target="blank"><img src="https://img.shields.io/github/followers/robert-frey?color=eee&logo=Github&style=for-the-badge" alt="@robert-frey" /></a>

  - [Saiem Gilani](https://twitter.com/saiemgilani)  
    <a href="https://twitter.com/saiemgilani" target="blank"><img src="https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge" alt="@saiemgilani" /></a>
    <a href="https://github.com/saiemgilani" target="blank"><img src="https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge" alt="@saiemgilani" /></a>
