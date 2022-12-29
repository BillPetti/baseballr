
# 

# `baseballr` <a href='https://billpetti.github.io/baseballr/'><img src='https://raw.githubusercontent.com/billpetti/baseballr/master/logo.png' align="right" width="30%" min-width="100px" /></a>

<!-- badges: start -->

[![CRAN
version](https://img.shields.io/badge/dynamic/json?style=for-the-badge&color=success&label=CRAN%20version&prefix=v&query=%24.Version&url=https%3A%2F%2Fcrandb.r-pkg.org%2Fbaseballr)](https://CRAN.R-project.org/package=baseballr)
[![CRAN
downloads](https://img.shields.io/badge/dynamic/json?style=for-the-badge&color=success&label=Downloads&query=%24%5B0%5D.downloads&url=https%3A%2F%2Fcranlogs.r-pkg.org%2Fdownloads%2Ftotal%2F2021-10-26%3Alast-day%2Fbaseballr)](https://CRAN.R-project.org/package=baseballr)
[![Version-Number](https://img.shields.io/github/r-package/v/BillPetti/baseballr?label=baseballr&logo=R&style=for-the-badge)](https://github.com/BillPetti/baseballr/)
[![R-CMD-check](https://img.shields.io/github/actions/workflow/status/BillPetti/baseballr/R-CMD-check.yaml?branch=master&label=R-CMD-Check&logo=R&logoColor=white&style=for-the-badge)](https://github.com/BillPetti/baseballr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg?style=for-the-badge&logo=github)](https://github.com/BillPetti/baseballr/)
[![Contributors](https://img.shields.io/github/contributors/BillPetti/baseballr?style=for-the-badge)](https://github.com/BillPetti/baseballr/graphs/contributors)
<!-- badges: end -->

`baseballr` is a package written for R focused on baseball analysis. It
includes functions for scraping various data from websites, such as
[FanGraphs.com](https://www.fangraphs.com/),
[Baseball-Reference.com](https://www.baseball-reference.com/), and
[baseballsavant.mlb.com](https://baseballsavant.mlb.com/). It also
includes functions for calculating metrics, such as wOBA, FIP, and
team-level consistency over custom time frames.

You can read more about some of the functions and how to use them at its
[official site](https://billpetti.github.io/baseballr/) as well as this
[Hardball Times
article](https://tht.fangraphs.com/developing-the-baseballr-package-for-r/).

## Installation

You can install the CRAN version of
[**`baseballr`**](https://CRAN.R-project.org/package=baseballr) with:

``` r
install.packages("baseballr")
```

You can install the released version of
[**`baseballr`**](https://github.com/BillPetti/baseballr) from
[GitHub](https://github.com/BillPetti/baseballr) with:

``` r
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("BillPetti/baseballr")
```

``` r
# Alternatively, using the devtools package:
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
devtools::install_github(repo = "BillPetti/baseballr")
```

For experimental functions in development, you can install the
[development
branch](https://github.com/BillPetti/baseballr/tree/development_branch):

``` r
# install.packages("devtools")
devtools::install_github("BillPetti/baseballr", ref = "development_branch")
```

## **Functionality**

The package consists of two main sets of functions: data acquisition and
metric calculation.

For example, if you want to see the standings for a specific MLB
division on a given date, you can use the `bref_standings_on_date()`
function. Just pass the year, month, day, and division you want:

``` r
library(baseballr)
library(dplyr)
bref_standings_on_date("2015-08-01", "NL East", from = FALSE)
```

    ## ── MLB Standings on Date data from baseball-reference.com ─── baseballr 1.3.0 ──

    ## ℹ Data updated: 2022-09-08 20:13:31 EDT

    ## # A tibble: 5 × 8
    ##   Tm        W     L `W-L%` GB       RS    RA `pythW-L%`
    ##   <chr> <int> <int>  <dbl> <chr> <int> <int>      <dbl>
    ## 1 WSN      54    48  0.529 --      422   391      0.535
    ## 2 NYM      54    50  0.519 1.0     368   373      0.494
    ## 3 ATL      46    58  0.442 9.0     379   449      0.423
    ## 4 MIA      42    62  0.404 13.0    370   408      0.455
    ## 5 PHI      41    64  0.39  14.5    386   511      0.374

Right now the function works as far as back as 1994, which is when both
leagues split into three divisions.

You can also pull data for all hitters over a specific date range. Here
are the results for all hitters from August 1st through October 3rd
during the 2015 season:

``` r
data <- bref_daily_batter("2015-08-01", "2015-10-03") 
data %>%
  dplyr::glimpse()
```

    ## Rows: 764
    ## Columns: 30
    ## $ bbref_id <chr> "547989", "554429", "542436", "571431", "501303", "346793", "…
    ## $ season   <int> 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2…
    ## $ Name     <chr> "Manny Machado", "Matt Duffy", "Jose Altuve", "Adam Eaton", "…
    ## $ Age      <dbl> 22, 24, 25, 26, 32, 21, 27, 28, 36, 28, 29, 29, 27, 29, 27, 2…
    ## $ Level    <chr> "Maj-AL", "Maj-NL", "Maj-AL", "Maj-AL", "Maj-AL", "Maj-AL", "…
    ## $ Team     <chr> "Baltimore", "San Francisco", "Houston", "Chicago", "Texas", …
    ## $ G        <dbl> 59, 59, 57, 58, 58, 58, 59, 58, 59, 57, 55, 57, 57, 58, 56, 5…
    ## $ PA       <dbl> 266, 264, 262, 262, 260, 259, 259, 258, 257, 257, 255, 255, 2…
    ## $ AB       <dbl> 237, 248, 244, 230, 211, 224, 239, 235, 231, 233, 213, 218, 2…
    ## $ R        <dbl> 36, 33, 30, 37, 48, 35, 32, 29, 37, 27, 50, 37, 36, 25, 38, 4…
    ## $ H        <dbl> 66, 71, 81, 74, 71, 79, 54, 66, 75, 48, 65, 56, 61, 51, 78, 5…
    ## $ X1B      <dbl> 43, 54, 53, 56, 47, 51, 34, 37, 48, 30, 34, 32, 35, 33, 66, 2…
    ## $ X2B      <dbl> 10, 12, 19, 12, 14, 17, 6, 17, 16, 11, 13, 13, 15, 10, 7, 13,…
    ## $ X3B      <dbl> 0, 2, 3, 1, 1, 4, 1, 0, 2, 1, 2, 4, 0, 1, 3, 0, 4, 0, 1, 1, 0…
    ## $ HR       <dbl> 13, 3, 6, 5, 9, 7, 13, 12, 9, 6, 16, 7, 11, 7, 2, 20, 9, 8, 8…
    ## $ RBI      <dbl> 32, 30, 18, 31, 34, 32, 27, 40, 53, 21, 50, 19, 31, 39, 23, 4…
    ## $ BB       <dbl> 26, 15, 10, 23, 39, 18, 16, 17, 21, 21, 34, 33, 21, 39, 12, 3…
    ## $ IBB      <dbl> 1, 0, 1, 1, 1, 0, 0, 6, 1, 1, 0, 1, 1, 5, 0, 4, 3, 3, 7, 2, 2…
    ## $ uBB      <dbl> 25, 15, 9, 22, 38, 18, 16, 11, 20, 20, 34, 32, 20, 34, 12, 35…
    ## $ SO       <dbl> 42, 35, 28, 55, 51, 38, 68, 56, 29, 53, 46, 62, 41, 48, 27, 7…
    ## $ HBP      <dbl> 2, 0, 4, 5, 8, 1, 3, 5, 1, 1, 2, 3, 3, 1, 1, 6, 1, 3, 4, 1, 0…
    ## $ SH       <dbl> 0, 0, 1, 2, 1, 11, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, …
    ## $ SF       <dbl> 1, 1, 3, 2, 1, 5, 1, 1, 4, 2, 5, 1, 2, 2, 3, 0, 3, 2, 3, 4, 3…
    ## $ GDP      <dbl> 5, 9, 6, 1, 1, 4, 2, 2, 9, 7, 5, 1, 4, 8, 1, 2, 3, 10, 5, 4, …
    ## $ SB       <dbl> 6, 8, 11, 9, 2, 10, 0, 0, 0, 3, 3, 4, 5, 4, 24, 2, 1, 0, 6, 0…
    ## $ CS       <dbl> 4, 0, 4, 4, 0, 2, 0, 0, 0, 1, 0, 1, 3, 2, 7, 2, 3, 0, 2, 0, 0…
    ## $ BA       <dbl> 0.279, 0.286, 0.332, 0.322, 0.337, 0.353, 0.226, 0.281, 0.325…
    ## $ OBP      <dbl> 0.353, 0.326, 0.364, 0.392, 0.456, 0.395, 0.282, 0.341, 0.377…
    ## $ SLG      <dbl> 0.485, 0.387, 0.508, 0.448, 0.540, 0.558, 0.423, 0.506, 0.528…
    ## $ OPS      <dbl> 0.839, 0.713, 0.872, 0.840, 0.996, 0.953, 0.705, 0.848, 0.906…

In terms of metric calculation, the package allows the user to calculate
the consistency of team scoring and run prevention for any year using
`team_consistency()`:

``` r
team_consistency(2015)
```

    ## # A tibble: 30 × 5
    ##    Team  Con_R Con_RA Con_R_Ptile Con_RA_Ptile
    ##    <chr> <dbl>  <dbl>       <dbl>        <dbl>
    ##  1 ARI    0.37   0.36          17           15
    ##  2 ATL    0.41   0.4           88           63
    ##  3 BAL    0.4    0.38          70           42
    ##  4 BOS    0.39   0.4           52           63
    ##  5 CHC    0.38   0.41          30           85
    ##  6 CHW    0.39   0.4           52           63
    ##  7 CIN    0.41   0.36          88           15
    ##  8 CLE    0.41   0.4           88           63
    ##  9 COL    0.35   0.34           7            3
    ## 10 DET    0.39   0.38          52           42
    ## # … with 20 more rows

You can also calculate wOBA per plate appearance and wOBA on contact for
any set of data over any date range, provided you have the data
available.

Simply pass the proper data frame to `woba_plus`:

``` r
data %>%
  dplyr::filter(PA > 200) %>%
  woba_plus %>%
  dplyr::arrange(desc(wOBA)) %>%
  dplyr::select(Name, Team, season, PA, wOBA, wOBA_CON) %>%
  dplyr::glimpse()
```

    ## Rows: 117
    ## Columns: 6
    ## $ Name     <chr> "Edwin Encarnacion", "Bryce Harper", "David Ortiz", "Joey Vot…
    ## $ Team     <chr> "Toronto", "Washington", "Boston", "Cincinnati", "Baltimore",…
    ## $ season   <int> 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2…
    ## $ PA       <dbl> 216, 248, 213, 251, 253, 260, 245, 255, 223, 241, 223, 259, 2…
    ## $ wOBA     <dbl> 0.490, 0.450, 0.449, 0.445, 0.434, 0.430, 0.430, 0.422, 0.410…
    ## $ wOBA_CON <dbl> 0.555, 0.529, 0.541, 0.543, 0.617, 0.495, 0.481, 0.494, 0.459…

You can also generate these wOBA-based stats, as well as FIP, for
pitchers using the `fip_plus()` function:

``` r
bref_daily_pitcher("2015-04-05", "2015-04-30") %>% 
  fip_plus() %>% 
  dplyr::select(season, Name, IP, ERA, SO, uBB, HBP, HR, FIP, wOBA_against, wOBA_CON_against) %>%
  dplyr::arrange(dplyr::desc(IP)) %>% 
  head(10)
```

    ## ── MLB Daily Pitcher data from baseball-reference.com ─────── baseballr 1.3.0 ──

    ## ℹ Data updated: 2022-09-08 20:13:44 EDT

    ## # A tibble: 10 × 11
    ##    season Name            IP   ERA    SO   uBB   HBP    HR   FIP wOBA_…¹ wOBA_…²
    ##     <int> <chr>        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>
    ##  1   2015 Johnny Cueto  37    1.95    38     4     2     3  2.62   0.21    0.276
    ##  2   2015 Dallas Keuc…  37    0.73    22    11     0     0  2.84   0.169   0.151
    ##  3   2015 Sonny Gray    36.1  1.98    25     6     1     1  2.69   0.218   0.239
    ##  4   2015 Mike Leake    35.2  3.03    25     7     0     5  4.16   0.24    0.281
    ##  5   2015 Felix Herna…  34.2  1.82    36     6     3     1  2.2    0.225   0.272
    ##  6   2015 Corey Kluber  34    4.24    36     5     2     2  2.4    0.295   0.391
    ##  7   2015 Jake Odoriz…  33.2  2.41    26     8     1     0  2.38   0.213   0.228
    ##  8   2015 Josh Collme…  32.2  2.76    16     3     0     1  2.82   0.29    0.33 
    ##  9   2015 Bartolo Col…  32.2  3.31    25     1     0     4  3.29   0.28    0.357
    ## 10   2015 Zack Greinke  32.2  1.93    27     7     1     2  3.01   0.24    0.274
    ## # … with abbreviated variable names ¹​wOBA_against, ²​wOBA_CON_against

## **Issues**

Please leave any suggestions or bugs in the [Issues
section](https://github.com/billpetti/baseballr/issues).

## **Pull Requests**

Pull request are welcome, but I cannot guarantee that they will be
accepted or accepted quickly. Please make all pull requests to the
[development
branch](https://github.com/billpetti/baseballr/tree/development_branch)
for review.

## **Breaking Changes**

[**Full News on
Releases**](https://billpetti.github.io/baseballr/news/index.html)

## Follow the [SportsDataverse](https://twitter.com/SportsDataverse) on Twitter and star this repo

[![Twitter
Follow](https://img.shields.io/twitter/follow/SportsDataverse?color=blue&label=%40SportsDataverse&logo=twitter&style=for-the-badge)](https://twitter.com/SportsDataverse)

[![GitHub
stars](https://img.shields.io/github/stars/billpetti/baseballr.svg?color=eee&logo=github&style=for-the-badge&label=Star%20baseballr&maxAge=2592000)](https://github.com/billpetti/baseballr/stargazers/)

## **Our Authors**

-   [Bill Petti](https://twitter.com/BillPetti)  
    <a href="https://twitter.com/BillPetti" target="blank"><img src="https://img.shields.io/twitter/follow/BillPetti?color=blue&label=%40BillPetti&logo=twitter&style=for-the-badge" alt="@BillPetti" /></a>
    <a href="https://github.com/BillPetti" target="blank"><img src="https://img.shields.io/github/followers/BillPetti?color=eee&logo=Github&style=for-the-badge" alt="@BillPetti" /></a>

-   [Saiem Gilani](https://twitter.com/saiemgilani)  
    <a href="https://twitter.com/saiemgilani" target="blank"><img src="https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge" alt="@saiemgilani" /></a>
    <a href="https://github.com/saiemgilani" target="blank"><img src="https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge" alt="@saiemgilani" /></a>

## **Our Contributors (they’re awesome)**

-   [Ben Baumer](https://twitter.com/BaumerBen)  
    <a href="https://twitter.com/BaumerBen" target="blank"><img src="https://img.shields.io/twitter/follow/BaumerBen?color=blue&label=%40BaumerBen&logo=twitter&style=for-the-badge" alt="@BaumerBen" /></a>
    <a href="https://github.com/beanumber" target="blank"><img src="https://img.shields.io/github/followers/beanumber?color=eee&logo=Github&style=for-the-badge" alt="@beanumber" /></a>

-   [Ben Dilday](https://twitter.com/BenDilday)  
    <a href="https://twitter.com/BenDilday" target="blank"><img src="https://img.shields.io/twitter/follow/BenDilday?color=blue&label=%40BenDilday&logo=twitter&style=for-the-badge" alt="@BenDilday" /></a>
    <a href="https://github.com/bdilday" target="blank"><img src="https://img.shields.io/github/followers/bdilday?color=eee&logo=Github&style=for-the-badge" alt="@bdilday" /></a>

-   [Robert Frey](https://twitter.com/RobertFrey40)  
    <a href="https://twitter.com/RobertFrey40" target="blank"><img src="https://img.shields.io/twitter/follow/RobertFrey40?color=blue&label=%40RobertFrey40&logo=twitter&style=for-the-badge" alt="@RobertFrey40" /></a>
    <a href="https://github.com/robert-frey" target="blank"><img src="https://img.shields.io/github/followers/robert-frey?color=eee&logo=Github&style=for-the-badge" alt="@robert-frey" /></a>

-   [Camden Kay](https://twitter.com/k_camden)  
    <a href="https://twitter.com/k_camden" target="blank"><img src="https://img.shields.io/twitter/follow/k_camden?color=blue&label=%40k_camden&logo=twitter&style=for-the-badge" alt="@k_camden" /></a>
    <a href="https://github.com/camdenk" target="blank"><img src="https://img.shields.io/github/followers/camdenk?color=eee&logo=Github&style=for-the-badge" alt="@camdenk" /></a>

## **Citations**

To cite the [**`baseballr`**](https://billpetti.github.io/baseballr/) R
package in publications, use:

BibTex Citation

``` bibtex
@misc{petti_gilani_2021,
  author = {Bill Petti and Saiem Gilani},
  title = {baseballr: The SportsDataverse's R Package for Baseball Data.},
  url = {https://billpetti.github.io/baseballr/},
  year = {2021}
}
```
