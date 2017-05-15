---
layout: page
title: Updated for baseballr 0.3.2
tags: rstats, web-scraping, baseballr, statcast	
---

The latest release of the [`baseballr`](https://billpetti.github.io/baseballr/) package for `R` includes a number of enhancement to acquiring data from [Baseball Savant](http://baseballsavant.com) as well as minor grammatical clean up in the documentation.

Previous functions `scrape_statcast_savant_batter` and `scrape_statcast_savant_pitcher` allowed for the acquistion of data from baseballsavant.com for a given player over a user-determined time frame. However, this is somewhat inefficient if you want to acquire data on all players over a given time frame. 

Two new functions have been added, `scrape_statcast_savant_batter_all` and `scrape_statcast_savant_pitcher_all`, that allow a user to acquire data for either all pitchers or all hitters over a given time frame.

Both functions take only two arguments:

`start_date`: the first date for which the user wants records returned
`end_date`: the final date for which the user wants records returned

Remember, baseballsavant.com's csv download option allows for about 50,000 records in a single query. That works out to roughly 10-12 days of games. Longer time frames will take longer to download.

Example: acquire data for all batters from 2017-04-03 through 2017-04-10

```r
> head(scrape_statcast_savant_batter_all('2017-04-03', '2017-04-10'))
[1] "These data are from BaseballSevant and are property of MLB Advanced Media, L.P. All rights reserved."
[1] "Grabbing data, this may take a minute..."
URL read and payload aquired successfully.
  pitch_type  game_date release_speed release_pos_x release_pos_z  player_name
1         FF 2017-04-10          92.7       -1.0367        5.7934   Eric Fryer
2         FF 2017-04-10          93.2       -0.9753        5.6007   Eric Fryer
3         FF 2017-04-10          93.0       -1.1196        5.6958   Eric Fryer
4         FF 2017-04-10          93.1       -0.9952        5.7978   Eric Fryer
5         SL 2017-04-10          83.4       -1.2385        5.8164   Eric Fryer
6         FF 2017-04-10          93.7       -1.0307        5.8740 Aledmys Diaz
  batter pitcher    events     description spin_dir spin_rate_deprecated
1 518700  518875 strikeout swinging_strike       NA                   NA
2 518700  518875      <NA>            ball       NA                   NA
3 518700  518875      <NA>            ball       NA                   NA
4 518700  518875      <NA> swinging_strike       NA                   NA
5 518700  518875      <NA>   called_strike       NA                   NA
6 649557  518875 field_out   hit_into_play       NA                   NA
  break_angle_deprecated break_length_deprecated zone
1                     NA                      NA    5
2                     NA                      NA   12
3                     NA                      NA   12
4                     NA                      NA    3
5                     NA                      NA    6
6                     NA                      NA    6
                                                      des game_type stand
1                      Eric Fryer strikes out swinging.           R     R
2                                                    <NA>         R     R
3                                                    <NA>         R     R
4                                                    <NA>         R     R
5                                                    <NA>         R     R
6 Aledmys Diaz flies out to right fielder Bryce Harper.           R     R
  p_throws home_team away_team type hit_location  bb_type balls strikes
1        R       WSH       STL    S         <NA>     <NA>     2       2
2        R       WSH       STL    B         <NA>     <NA>     1       2
3        R       WSH       STL    B         <NA>     <NA>     0       2
4        R       WSH       STL    S         <NA>     <NA>     0       1
5        R       WSH       STL    S         <NA>     <NA>     0       0
6        R       WSH       STL    X            9 fly_ball     0       1
  game_year   pfx_x  pfx_z plate_x plate_z on_3b on_2b  on_1b outs_when_up
1      2017 -0.4262 1.7261 -0.0042  2.9680    NA    NA 594824            2
2      2017  0.2420 1.3633  1.3747  3.5269    NA    NA 594824            2
3      2017  0.4912 1.6758  0.5389  4.3795    NA    NA 594824            2
4      2017  0.1924 1.7964  0.6868  3.5700    NA    NA 594824            2
5      2017 -0.1604 0.3532  0.6048  2.6308    NA    NA 594824            2
6      2017  0.5956 1.8068  0.4993  3.1386    NA    NA 594824            1
  inning inning_topbot   hc_x   hc_y tfs_deprecated tfs_zulu_deprecated
1      9           Top   <NA>   <NA>             NA                  NA
2      9           Top   <NA>   <NA>             NA                  NA
3      9           Top   <NA>   <NA>             NA                  NA
4      9           Top   <NA>   <NA>             NA                  NA
5      9           Top   <NA>   <NA>             NA                  NA
6      9           Top 186.56 105.27             NA                  NA
  pos2_person_id umpire         sv_id vx0 vy0 vz0 ax ay az sz_top sz_bot
1         446308     NA 170411_025210  NA  NA  NA NA NA NA 3.8420 1.5890
2         446308     NA 170411_025153  NA  NA  NA NA NA NA 3.5602 1.7127
3         446308     NA 170411_025133  NA  NA  NA NA NA NA 3.6761 1.6780
4         446308     NA 170411_025117  NA  NA  NA NA NA NA 3.6760 1.5040
5         446308     NA 170411_025104  NA  NA  NA NA NA NA 3.5139 1.6548
6         446308     NA 170411_025018  NA  NA  NA NA NA NA 3.9500 1.6810
  hit_distance_sc launch_speed launch_angle effective_speed release_spin_rate
1              NA           NA           NA          93.033              2285
2              NA           NA           NA          93.301              2323
3              NA           NA           NA          92.892              2322
4              NA           NA           NA          92.906              2324
5              NA           NA           NA          83.371                NA
6             266         87.5       47.444          93.529              2406
  release_extension game_pk pos1_person_id pos2_person_id.1 pos3_person_id
1             6.248  490201         518875           446308         475582
2             6.265  490201         518875           446308         475582
3             6.281  490201         518875           446308         475582
4             6.187  490201         518875           446308         475582
5             6.155  490201         518875           446308         475582
6             6.269  490201         518875           446308         475582
  pos4_person_id pos5_person_id pos6_person_id pos7_person_id pos8_person_id
1         502517         543685         452220         594809         572191
2         502517         543685         452220         594809         572191
3         502517         543685         452220         594809         572191
4         502517         543685         452220         594809         572191
5         502517         543685         452220         594809         572191
6         502517         543685         452220         594809         572191
  pos9_person_id release_pos_y estimated_ba_using_speedangle
1         547180       54.2491                         0.000
2         547180       54.2319                         0.000
3         547180       54.2163                         0.000
4         547180       54.3096                         0.000
5         547180       54.3420                         0.000
6         547180       54.2282                         0.007
  estimated_woba_using_speedangle woba_value woba_denom babip_value iso_value
1                           0.000       0.00          1           0         0
2                           0.000       <NA>       <NA>        <NA>      <NA>
3                           0.000       <NA>       <NA>        <NA>      <NA>
4                           0.000       <NA>       <NA>        <NA>      <NA>
5                           0.000       <NA>       <NA>        <NA>      <NA>
6                           0.008       0.00          1           0         0
  barrel
1     NA
2     NA
3     NA
4     NA
5     NA
6      0
```