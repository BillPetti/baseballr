---
layout: page
title: baseballr current release notes
tags: rstats, baseballr
---

## April 9, 2018

The latest release of the [`baseballr`](https://billpetti.github.io/baseballr/) package for `R` (0.4.1) includes a number of enhancements and bug fixes.

There are now two functions that allow users to scrape player game logs from FanGraphs:

- `batter_game_logs_fg`
- `pitcher_game_logs_fg`  

Both functions has two arguments:  `player_id` and `year`. Both will return detailed game logs from FanGraphs for the selected season.

```r
> batter_game_logs_fg(playerid = 10155, year = 2018)
        Date Team  Opp BO Pos PA H X2B X3B HR R RBI SB CS BB_perc K_perc  ISO
1 2018-04-04  LAA  CLE  2  CF  5 0   0   0  0 0   0  0  0    0.40  0.200 .000
2 2018-04-03  LAA  CLE  2  CF  5 1   0   0  1 1   1  0  0    0.20  0.000 .750
3 2018-04-02  LAA  CLE  2  CF  4 0   0   0  0 0   0  0  0    0.25  0.250 .000
4 2018-04-01  LAA @OAK  2  CF  5 2   1   0  0 1   1  0  0    0.00  0.000 .200
5 2018-03-31  LAA @OAK  2  CF  5 3   2   0  0 2   2  1  0    0.00  0.200 .400
6 2018-03-30  LAA @OAK  2  CF  4 1   0   0  1 2   1  0  0    0.00  0.000 .750
7 2018-03-29  LAA @OAK  2  CF  6 0   0   0  0 0   0  0  0    0.00  0.167 .000
  BABIP  AVG  OBP   SLG wOBA wRC_plus
1  .000 .000 .400  .000 .276       78
2  .000 .250 .400 1.000 .559      280
3  .000 .000 .250  .000 .172        5
4  .400 .400 .400  .600 .432      190
5  .750 .600 .600 1.000 .686      371
6  .000 .250 .250 1.000 .526      257
7  .000 .000 .000  .000 .000     -100
```

`viz_gb_on_period` was contributed by [Daniel H](https://github.com/darh78) and allows a user to generate a time series of standings for a division and automatically visualize the data in an interactive chart.

```r
> viz_gb_on_period("2018-03-29", "2018-04-05", "NL East")
   |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed = 16s
# A tibble: 10 x 7
   League  Date       Team      W     L WLpct    GB
   <chr>   <date>     <chr> <int> <int> <dbl> <dbl>
 1 NL East 2018-03-29 NYM       1     0 1.00  0.   
 2 NL East 2018-03-29 ATL       1     0 1.00  0.   
 3 NL East 2018-03-29 WSN       0     0 0.    0.500
 4 NL East 2018-03-29 PHI       0     1 0.    1.00 
 5 NL East 2018-03-29 MIA       0     1 0.    1.00 
 6 NL East 2018-04-05 NYM       5     1 0.833 0.   
 7 NL East 2018-04-05 ATL       4     2 0.667 1.00 
 8 NL East 2018-04-05 WSN       4     3 0.571 1.50 
 9 NL East 2018-04-05 PHI       2     4 0.333 3.00 
10 NL East 2018-04-05 MIA       2     5 0.286 3.50 
```

[Ben Dilday](https://github.com/bdilday) combined he various `scrape_statcast_savant` functions I previously released intro a single function. The single function can pull all data over a given date range for all pitchers or batters or just for specific pitchers or batters.

``r 
> head(scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-15", playerid = 592789, player_type='pitcher'))
[1] "These data are from BaseballSevant and are property of MLB Advanced Media, L.P. All rights reserved."
[1] "Grabbing data, this may take a minute..."
URL read and payload acquired successfully.
  pitch_type  game_date release_speed release_pos_x release_pos_z
1         FF 2016-04-12          97.3       -0.6733        6.4372
2         FF 2016-04-12          97.8       -0.6366        6.4466
3         FF 2016-04-12          97.6       -0.4936        6.4440
4         FF 2016-04-12          97.0       -0.6884        6.5753
5         SL 2016-04-12          91.6       -0.7873        6.4002
6         CH 2016-04-12          88.9       -1.0913        6.2130
       player_name batter pitcher    events          description spin_dir
1 Noah Syndergaard 400085  592789    single        hit_into_play       NA
2 Noah Syndergaard 400085  592789      <NA>        called_strike       NA
3 Noah Syndergaard 425772  592789 field_out        hit_into_play       NA
4 Noah Syndergaard 425772  592789      <NA>        called_strike       NA
5 Noah Syndergaard 588751  592789 field_out        hit_into_play       NA
6 Noah Syndergaard 518618  592789    double hit_into_play_no_out       NA 

>head(scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-06"))
[1] "These data are from BaseballSevant and are property of MLB Advanced Media, L.P. All rights reserved."
[1] "Grabbing data, this may take a minute..."
URL read and payload acquired successfully.
  pitch_type  game_date release_speed release_pos_x release_pos_z     player_name batter pitcher
1         FT 2016-04-06          91.2       -1.9089        6.4077  Jake Marisnick 545350  467100
2         CU 2016-04-06          77.7       -1.7753        6.7376   Carlos Correa 621043  467100
3         CU 2016-04-06          80.3       -1.5339        6.6463   Carlos Correa 621043  467100
4         SL 2016-04-06          84.7       -1.7689        6.4903   Carlos Correa 621043  467100
5         FT 2016-04-06          90.8       -1.8843        6.4235   Carlos Correa 621043  467100
6         FT 2016-04-06          90.2       -1.7467        6.5141 George Springer 543807  467100
     events             description spin_dir spin_rate_deprecated break_angle_deprecated
1 field_out           hit_into_play       NA                   NA                     NA
2 strikeout swinging_strike_blocked       NA                   NA                     NA
3      <NA>                    ball       NA                   NA                     NA
4      <NA>           called_strike       NA                   NA                     NA
5      <NA>           called_strike       NA                   NA                     NA
6      walk                    ball       NA                   NA                     NA
```

Finally, I've added a function for generating spray charts based on my [Interactive Spray Chart Tool](http://billpetti.shinyapps.io/shiny_spraychart/).

`ggspraychart` can generate either a typical spray chart or a density chart for a given hitter. The function takes a data frame with hit coordinates and allows users to customize fill colors and values and the transparency of points. Users can also adjust the bin value when generating density plots.

Here's are point and density examples using data for Jose Altuve:

```r
ggspraychart(data, point_alpha = .6, fill_legend_title = "Hit Type", fill_value = "hit_type", 
               fill_palette = c("1"="#A2C8EC", "2"="#006BA4", "3"="#FF940E",
                                "Out"="#595959", "4"="#C85200")) + 
  facet_wrap(~game_year, nrow = 2) +
  ggtitle("\nJose Altuve") +
  labs(subtitle = "Spray Charts Since 2013\n")
```

<plot>

```r
ggspraychart(data, point_alpha = .2, density = TRUE, bin_size = 30) + 
  facet_wrap(~game_year, nrow = 2) +
  ggtitle("\nJose Altuve") +
  labs(subtitle = "Spray Charts Since 2013\n")
```
<plot>
	
The function is also written in such a way where it can be combined with `gganimate` to create animated plots.








