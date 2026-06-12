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
#> ℹ Data updated: 2026-06-12 13:50:15 UTC
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
#> ── NCAA Baseball Team Batting Stats data from stats.ncaa.org ───────────
#> ℹ Data updated: 2026-06-12 13:50:32 UTC
#> # A tibble: 36 × 42
#>     year team_name   team_id conference_id conference division player_id
#>    <int> <chr>         <dbl>         <int> <chr>         <dbl>     <int>
#>  1  2026 Florida St.     234           821 ACC               1   9678004
#>  2  2026 Florida St.     234           821 ACC               1   9680283
#>  3  2026 Florida St.     234           821 ACC               1   9703054
#>  4  2026 Florida St.     234           821 ACC               1   9686071
#>  5  2026 Florida St.     234           821 ACC               1   9699227
#>  6  2026 Florida St.     234           821 ACC               1   9690199
#>  7  2026 Florida St.     234           821 ACC               1   9698044
#>  8  2026 Florida St.     234           821 ACC               1   9702164
#>  9  2026 Florida St.     234           821 ACC               1   9684157
#> 10  2026 Florida St.     234           821 ACC               1   9690665
#> # ℹ 26 more rows
#> # ℹ 35 more variables: player_url <chr>, player_name <chr>, Yr <chr>,
#> #   Pos <chr>, Jersey <chr>, Ht <chr>, `B/T` <chr>, GP <dbl>, GS <dbl>,
#> #   BA <dbl>, OBPct <dbl>, SlgPct <dbl>, R <dbl>, AB <dbl>, H <dbl>,
#> #   `2B` <dbl>, `3B` <dbl>, TB <dbl>, HR <dbl>, RBI <dbl>, BB <dbl>,
#> #   HBP <dbl>, SF <dbl>, SH <dbl>, K <dbl>, DP <dbl>, CS <dbl>,
#> #   Picked <dbl>, SB <dbl>, IBB <dbl>, GDP <dbl>, RBI2out <dbl>, …
```

The same can be done for pitching, just by changing the `type`
parameter:

``` r

ncaa_team_player_stats(team_id = team_id, year = year,  "pitching")
#> ── NCAA Baseball Team Pitching Stats data from stats.ncaa.org ──────────
#> ℹ Data updated: 2026-06-12 13:50:48 UTC
#> # A tibble: 19 × 49
#>     year team_name   team_id conference_id conference division player_id
#>    <int> <chr>         <dbl>         <int> <chr>         <dbl>     <int>
#>  1  2026 Florida St.     234           821 ACC               1   9703054
#>  2  2026 Florida St.     234           821 ACC               1   9690199
#>  3  2026 Florida St.     234           821 ACC               1   9698044
#>  4  2026 Florida St.     234           821 ACC               1   9684157
#>  5  2026 Florida St.     234           821 ACC               1   9690665
#>  6  2026 Florida St.     234           821 ACC               1   9698671
#>  7  2026 Florida St.     234           821 ACC               1  11248066
#>  8  2026 Florida St.     234           821 ACC               1  11248103
#>  9  2026 Florida St.     234           821 ACC               1  11248107
#> 10  2026 Florida St.     234           821 ACC               1  11248110
#> 11  2026 Florida St.     234           821 ACC               1  11248122
#> 12  2026 Florida St.     234           821 ACC               1  11248141
#> 13  2026 Florida St.     234           821 ACC               1  11248145
#> 14  2026 Florida St.     234           821 ACC               1  11248150
#> 15  2026 Florida St.     234           821 ACC               1  11248106
#> 16  2026 Florida St.     234           821 ACC               1  11248117
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
