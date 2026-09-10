# NCAA Scraping

The latest release of the
[`baseballr`](https://billpetti.github.io/baseballr/) includes a
function for acquiring player statistics from the [NCAA’s
website](http://stats.ncaa.org) for baseball teams across the three
major divisions (I, II, III).

In order to look up teams, you can either load the teams for all
divisions from the `baseballr-data` repository or access them directly
from the NCAA website for a given year and division.

Loading from the baseballr-data repository:

``` r

library(baseballr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
ncaa_teams_df <- load_ncaa_baseball_teams()
```

From the NCAA website:

``` r

try(ncaa_teams(year = most_recent_ncaa_baseball_season(), division = "1"))
#> ── NCAA Baseball Teams data from stats.ncaa.org ───── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-09-10 00:53:49 UTC
#> # A tibble: 308 × 9
#>    team_id team_name    team_url conference_id conference division  year
#>    <chr>   <chr>        <chr>    <chr>         <chr>      <chr>    <dbl>
#>  1 NA      Boston Coll… /teams/… 821           ACC        1         2026
#>  2 NA      California   /teams/… 821           ACC        1         2026
#>  3 NA      Clemson      /teams/… 821           ACC        1         2026
#>  4 NA      Duke         /teams/… 821           ACC        1         2026
#>  5 NA      Florida St.  /teams/… 821           ACC        1         2026
#>  6 NA      Georgia Tech /teams/… 821           ACC        1         2026
#>  7 NA      Louisville   /teams/… 821           ACC        1         2026
#>  8 NA      Miami (FL)   /teams/… 821           ACC        1         2026
#>  9 NA      NC State     /teams/… 821           ACC        1         2026
#> 10 NA      North Carol… /teams/… 821           ACC        1         2026
#> # ℹ 298 more rows
#> # ℹ 2 more variables: season_id <chr>, season_team_id <chr>
```

The function,
[`ncaa_team_player_stats()`](https://billpetti.github.io/baseballr/reference/ncaa_team_player_stats.md),
requires the user to pass values for three parameters for the function
to work:

`team_id`: numerical code used by the NCAA for each school `year`: a
four-digit year `type`: whether to pull data for batters or pitchers

If you want to pull batting statistics for Florida State for the 2026
season, you would use the following:

``` r


team_id <- ncaa_teams_df %>% 
  dplyr::filter(.data$team_name == "Florida St.") %>% 
  dplyr::select("team_id") %>% 
  dplyr::distinct() %>% 
  dplyr::pull("team_id")

year <- most_recent_ncaa_baseball_season()

ncaa_team_player_stats(team_id = team_id, year = year, "batting")
#> ✖ 2026-09-10 00:54:11.025934: NCAA browser fallback failed: Chrome debugging port not open after 10 seconds.
#> ! 2026-09-10 00:54:11.033561: stats.ncaa.org returned an Akamai bot-challenge for the team batting stats endpoint; no data could be retrieved. Install {chromote} + Google Chrome to enable the browser fallback.
#> data frame with 0 columns and 0 rows
```

The same can be done for pitching, just by changing the `type`
parameter:

``` r

ncaa_team_player_stats(team_id = team_id, year = year,  "pitching")
#> ── NCAA Baseball Team Pitching Stats data from stats.ncaa.org ──────────
#> ℹ Data updated: 2026-09-10 00:54:37 UTC
#> # A tibble: 19 × 49
#>     year team_name   team_id conference_id conference division player_id
#>    <int> <chr>         <dbl>         <int> <chr>         <dbl>     <int>
#>  1  2026 Florida St.     234           821 ACC               1   9698671
#>  2  2026 Florida St.     234           821 ACC               1  11248107
#>  3  2026 Florida St.     234           821 ACC               1   9703054
#>  4  2026 Florida St.     234           821 ACC               1   9690665
#>  5  2026 Florida St.     234           821 ACC               1  11248066
#>  6  2026 Florida St.     234           821 ACC               1   9684157
#>  7  2026 Florida St.     234           821 ACC               1  11248122
#>  8  2026 Florida St.     234           821 ACC               1  11248117
#>  9  2026 Florida St.     234           821 ACC               1  11248106
#> 10  2026 Florida St.     234           821 ACC               1  11248103
#> 11  2026 Florida St.     234           821 ACC               1  11248110
#> 12  2026 Florida St.     234           821 ACC               1   9690199
#> 13  2026 Florida St.     234           821 ACC               1   9698044
#> 14  2026 Florida St.     234           821 ACC               1  11248141
#> 15  2026 Florida St.     234           821 ACC               1  11248145
#> 16  2026 Florida St.     234           821 ACC               1  11248150
#> 17  2026 Florida St.     234           821 ACC               1  11248168
#> 18  2026 Florida St.     234           821 ACC               1        NA
#> 19  2026 Florida St.     234           821 ACC               1        NA
#> # ℹ 42 more variables: player_url <chr>, player_name <chr>, Yr <chr>,
#> #   Pos <chr>, Jersey <chr>, Ht <chr>, `B/T` <chr>, App <dbl>,
#> #   GS <dbl>, ERA <dbl>, IP <dbl>, CG <dbl>, H <dbl>, R <dbl>,
#> #   ER <dbl>, BB <dbl>, SO <dbl>, SHO <dbl>, BF <dbl>, `P-OAB` <dbl>,
#> #   `2B-A` <dbl>, `3B-A` <dbl>, Bk <dbl>, `HR-A` <dbl>, WP <dbl>,
#> #   HB <dbl>, IBB <dbl>, `Inh Run` <dbl>, `Inh Run Score` <dbl>,
#> #   SHA <dbl>, SFA <dbl>, Pitches <dbl>, GO <dbl>, FO <dbl>, W <dbl>, …
```

Now, the function is dependent on the user knowing the `team_id` used by
the NCAA website. Given that, I’ve included a `ncaa_school_id_lu`
function so that users can find the `team_id` they need.

Just pass a string to the function and it will return possible matches
based on the school’s name:

``` r

ncaa_school_id_lu("Vand")
#> ── NCAA Baseball Teams Information from baseballr data repository ──────
#> ℹ Data updated: 2026-05-30 01:50:36 UTC
#> # A tibble: 17 × 9
#>    team_id team_name  team_url   conference_id conference division  year
#>      <dbl> <chr>      <chr>              <dbl> <chr>         <dbl> <dbl>
#>  1     736 Vanderbilt /teams/61…           911 SEC               1  2026
#>  2     736 Vanderbilt /teams/59…           911 SEC               1  2025
#>  3     736 Vanderbilt /team/736…           911 SEC               1  2024
#>  4     736 Vanderbilt /team/736…           911 SEC               1  2023
#>  5     736 Vanderbilt /team/736…           911 SEC               1  2022
#>  6     736 Vanderbilt /team/736…           911 SEC               1  2021
#>  7     736 Vanderbilt /team/736…           911 SEC               1  2020
#>  8     736 Vanderbilt /team/736…           911 SEC               1  2019
#>  9     736 Vanderbilt /team/736…           911 SEC               1  2018
#> 10     736 Vanderbilt /team/736…           911 SEC               1  2017
#> 11     736 Vanderbilt /team/736…           911 SEC               1  2016
#> 12     736 Vanderbilt /team/736…           911 SEC               1  2015
#> 13     736 Vanderbilt /team/736…           911 SEC               1  2014
#> 14     736 Vanderbilt /team/736…           911 SEC               1  2013
#> 15     736 Vanderbilt /team/736…           911 SEC               1  2012
#> 16     736 Vanderbilt /team/736…           911 SEC               1  2011
#> 17     736 Vanderbilt /team/736…           911 SEC               1  2010
#> # ℹ 2 more variables: season_id <dbl>, season_team_id <dbl>
```

## **Our Authors**

- [Bill Petti](https://x.com/BillPetti)
  [![@BillPetti](https://img.shields.io/twitter/follow/BillPetti?color=blue&label=%40BillPetti&logo=x&style=for-the-badge)](https://x.com/BillPetti)
  [![@BillPetti](https://img.shields.io/github/followers/BillPetti?color=eee&logo=Github&style=for-the-badge)](https://github.com/BillPetti)
- [Saiem Gilani](https://x.com/saiemgilani)
  [![@saiemgilani](https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=x&style=for-the-badge)](https://x.com/saiemgilani)
  [![@saiemgilani](https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge)](https://github.com/saiemgilani)

### **Our Contributors**

- [Ben Baumer](https://x.com/BaumerBen)
  [![@BaumerBen](https://img.shields.io/twitter/follow/BaumerBen?color=blue&label=%40BaumerBen&logo=x&style=for-the-badge)](https://x.com/BaumerBen)
  [![@beanumber](https://img.shields.io/github/followers/beanumber?color=eee&logo=Github&style=for-the-badge)](https://github.com/beanumber)
- [Ben Dilday](https://x.com/BenDilday)
  [![@BenDilday](https://img.shields.io/twitter/follow/BenDilday?color=blue&label=%40BenDilday&logo=x&style=for-the-badge)](https://x.com/BenDilday)
  [![@bdilday](https://img.shields.io/github/followers/bdilday?color=eee&logo=Github&style=for-the-badge)](https://github.com/bdilday)
- [Robert Frey](https://x.com/RobertFrey40)
  [![@RobertFrey40](https://img.shields.io/twitter/follow/RobertFrey40?color=blue&label=%40RobertFrey40&logo=x&style=for-the-badge)](https://x.com/RobertFrey40)
  [![@robert-frey](https://img.shields.io/github/followers/robert-frey?color=eee&logo=Github&style=for-the-badge)](https://github.com/robert-frey)
- [Camden Kay](https://x.com/k_camden)
  [![@k_camden](https://img.shields.io/twitter/follow/k_camden?color=blue&label=%40k_camden&logo=x&style=for-the-badge)](https://x.com/k_camden)
  [![@camdenk](https://img.shields.io/github/followers/camdenk?color=eee&logo=Github&style=for-the-badge)](https://github.com/camdenk)

### **Citation**

To cite the [**`baseballr`**](https://baseballr.sportsdataverse.org/) R
package in publications, use:

BibTeX Citation

``` bibtex
@misc{baseballr,
  author = {Bill Petti and Saiem Gilani},
  title = {baseballr: An R Package for Baseball Data Acquisition and Analysis},
  url = {https://baseballr.sportsdataverse.org/},
  year = {2026}
}
```

### **Related SportsDataverse packages**

- [**cfbfastR**](https://cfbfastR.sportsdataverse.org/) - college
  football
- [**hoopR**](https://hoopR.sportsdataverse.org/) - men’s basketball
- [**wehoop**](https://wehoop.sportsdataverse.org/) - women’s basketball
- [**baseballr**](https://baseballr.sportsdataverse.org/) - baseball
- [**fastRhockey**](https://fastRhockey.sportsdataverse.org/) - hockey
- [**oddsapiR**](https://oddsapiR.sportsdataverse.org/) - betting odds
- [**sportyR**](https://sportyR.sportsdataverse.org/) - playing surfaces
- [**sportsdataverse-py**](https://py.sportsdataverse.org/) - the Python
  package
- [**sportsdataverse-R**](https://r.sportsdataverse.org/) - the R
  meta-package
