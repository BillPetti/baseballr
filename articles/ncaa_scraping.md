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
#> ℹ Data updated: 2026-06-09 20:48:19 UTC
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
#> ! 2026-06-09 20:48:24.995429: stats.ncaa.org returned an Akamai bot-challenge for the team stats endpoint; no data could be retrieved. This endpoint is gated and cannot be scraped without a browser session.
#> data frame with 0 columns and 0 rows
```

The same can be done for pitching, just by changing the `type`
parameter:

``` r

ncaa_team_player_stats(team_id = team_id, year = year,  "pitching")
#> ! 2026-06-09 20:48:30.190045: stats.ncaa.org returned an Akamai bot-challenge for the team stats endpoint; no data could be retrieved. This endpoint is gated and cannot be scraped without a browser session.
#> data frame with 0 columns and 0 rows
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
