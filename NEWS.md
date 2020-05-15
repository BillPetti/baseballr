# baseballr 0.8 (2020-05-15)

## New Functions

Thanks to [Robert Frey](https://github.com/robert-frey), we've added a new function that allows the user to calculate park factors at the NCAA level. 

`get_ncaa_park_factors` takes two arguments; `teamid`, `years`, and `type`. Users can submit a single year or multiple years (recommended) and uses a team's schedule with results to calculate their home park's factors. Also, if a user selects `conference` as the type, the park factor will be adjusted based on the number of teams in a conference. This has the practical effect of suggesting more regression should be applied to players who play against fewer teams in fewer parks. 

The function will return two versions of the park factor: the `base_pf` and the `final_pf`, which simply takes the `base_pf` and applies an adjustment that is based on [FanGraphs' method](https://library.fangraphs.com/park-factors-5-year-regressed/):

```
get_ncaa_park_factor(736, c(2017:2019),type = "conference")
      school home_game away_game runs_scored_home runs_allowed_home runs_scored_away
1 Vanderbilt       104        91              782               416              591
  runs_allowed_away base_pf home_game_adj final_pf
1               426   1.028         1.013    1.011

get_ncaa_park_factor(736, c(2017:2019),type = "division")
      school home_game away_game runs_scored_home runs_allowed_home runs_scored_away
1 Vanderbilt       104        91              782               416              591
  runs_allowed_away base_pf home_game_adj final_pf
1               426   1.031         1.014    1.011

get_ncaa_park_factor(736, c(2015:2019),type = "division")
      school home_game away_game runs_scored_home runs_allowed_home runs_scored_away
1 Vanderbilt       175       154             1314               658              951
  runs_allowed_away base_pf home_game_adj final_pf
1               680   1.209         1.098    1.093
```

## Fixes and Updates

- [Shane Piesik](https://github.com/shanepiesik) made some changes to the `scrape_statcast_savant` function. The function should now be easier to use in a loop or map when combining payloads, and more importantly the data should read in faster thanks to swapping in `vroom`.

- The `master_ncaa_team_lu` was updated by Robert Frey. For some reason, the NCAA website that had the 2019 Division 1 information has dissappeared, and when I rebuilt the table at the begining of 2020 it led to tons of `NA` values for those teams that year. Thanks to Robert for manually fixing.

## Other

Messages have been added to all functions that pull data from [FanGraphs.com](https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=62a468b8ba&auto-switch=true) and [Baseball-Reference.com](https://stathead.com). The messages ask users to support both sites through their paid subscription services. Please consider supporting both, especially if you are using `baseballr` to pull data from their sites.

# baseballr 0.7 (2020-01-07)

## New Functions

This release of [`baseballr`](https://github.com/BillPetti/baseballr) includes functions   that allow a user to query data through MLB's stats api at the minor league level.

You can view a look up table with `?get_game_pks_mlb` to get the appropriate IDs for each level, but I'll post them here as well (this is not comprehensive):

| 1 | MLB |
|------|----------------------|
| 11 | Triple-A |
| 12 | Double-A |
| 13 | Class A Advanced |
| 14 | Class A |
| 15 | Class A Short Season |
| 5442 | Rookie Advanced |
| 16 | Rookie |
| 17 | Winter League |

Game information can be acquired using the `get_game_pks_mlb` function, along with the level for which you want games:

```
games <- get_game_pks_mlb(date = '2019-05-01',
                          level_ids = c(11, 12))

games %>%
  select(game_pk, gameDate, teams.away.team.name, teams.home.team.name) %>%
  slice(1:10)
  
   game_pk             gameDate      teams.away.team.name  teams.home.team.name
1   579921 2019-05-01T16:05:00Z        Round Rock Express Oklahoma City Dodgers
2   579919 2019-05-01T03:33:00Z        Round Rock Express Oklahoma City Dodgers
3   584340 2019-05-01T21:30:00Z        Midland RockHounds        Tulsa Drillers
4   584269 2019-05-01T03:33:00Z        Midland RockHounds        Tulsa Drillers
5   579571 2019-05-01T21:38:00Z      San Antonio Missions             Iowa Cubs
6   579570 2019-05-01T03:33:00Z      San Antonio Missions             Iowa Cubs
7   571587 2019-05-01T14:30:00Z            Erie SeaWolves         Altoona Curve
8   572288 2019-05-01T14:30:00Z New Hampshire Fisher Cats       Trenton Thunder
9   575163 2019-05-01T14:35:00Z             Norfolk Tides          Durham Bulls
10  575655 2019-05-01T14:35:00Z           Louisville Bats       Toledo Mud Hens
```																	

You can also use the `get_game_info_mlb` function to grab additional info on each game, such as weather and (in some cases) attendance:

```
map_df(.x = games$game_pk[1:10], 
       ~get_game_info_mlb(.x)) %>%
  select(game_date, venue_name, temperature, other_weather, wind)

# A tibble: 10 x 5
   game_date  venue_name                 temperature other_weather wind           
   <chr>      <chr>                      <chr>       <chr>         <chr>          
 1 2019-05-01 Chickasaw Bricktown Ballp… 63          Cloudy        4 mph, R To L  
 2 2019-05-01 Chickasaw Bricktown Ballp… 72          Cloudy        13 mph, R To L 
 3 2019-05-01 ONEOK Field                77          Overcast      14 mph, In Fro…
 4 2019-05-01 ONEOK Field                74          clear         14 mph, In Fro…
 5 2019-05-01 Principal Park             54          Overcast      3 mph, In From…
 6 2019-05-01 Principal Park             53          Overcast      1 mph, Calm    
 7 2019-05-01 Peoples Natural Gas Field  58          Overcast      7 mph, In From…
 8 2019-05-01 ARM & HAMMER Park          55          Overcast      5 mph, L To R  
 9 2019-05-01 Durham Bulls Athletic Park 70          Partly Cloudy 7 mph, In From…
10 2019-05-01 Fifth Third Field          59          Cloudy        7 mph, R To L  
```

Once you have the `game_pk` IDs grabbing the pbp data is very simple. All you need to do is pass the `game_pk` of interest to the `get_pbp_mlb` function. 

Let's say you interested in the Gwinnett Stripers versus the Charlotte Knights:

```
payload <- get_pbp_mlb(575589)
```

The function will return a data frame with 131 columns. Data availability will vary depending on the park and the league level, as most sensor data is not availble in minor league parks via this API. Also note that the column names have mostly been left as-is and there are likely duplicate columns in terms of the information they provide. I plan to clean the output up down the road, but for now I am leaving the majority as-is.

Some of the colums of interest at the minor league level are:

- `pitchNumber` and `atBatIndex`: the pitch number within a given plate appearance and the plate appearance within a given game.
- `pitchData.coordinates.x` and `pitchData.coordinates.y`: the x,z coordinates of the pitch as it crosses the plate. As far as I can tell, these are the pixel coordinates for a location that a stringer manually plots and likely need to be transformed and rotated to get a view of the pitch as it crosses the plate. I am working on figuring out an easy transformation to get them on the same scale as the MLB coordinates, but they appear different by park. I do believe you can multiple both by -1 and that will at least allow you to orient the coordinates correctly (i.e. catcher's view)
- `details.call.code`, `details.call.description`, `result.event`, `result.eventType`, and `result.description`: these are similar to what we find with Statcast data--codes and detailed desriptions for what happened on a pitch or at the end of a plate appearance.
- `count.` variables that tell you how many balls, strikes, and outs before and after the pitch.
- `batter.id` and `pitcher.id`
- `matchup.batSide.code ` and `matchup.pitchHand.code`: handedness of the batter and pitcher.
- A series of columns that tell you what the league and level is of both the home and away teams and includes their parent organizations.
- `batted.ball.result`, `hitData.coordinates.coordX`, `hitData.coordinates.coordY`, `hitData.trajectory`: various information about the batted ball. Of most interest will be the coordinate columns.

# baseballr 0.4 (2019-03-18)

The latest release of the [`baseballr`](https://billpetti.github.io/baseballr/) package for `R` includes a number of enhancements and bug fixes.

## New Functions

`fg_pitch_leaders()`

This function is the compliment to the `fg_bat_leaders()` function, returning leaderboard information for pitchers from FanGraphs.com.

In addition to noting the start and end years, along with qualified/IP requirements, etc., you can note whether you want all pitchers, starters, or just relievers using the `pitcher_type` argument. 

- `pit` wil give you all pitchers that meet your requirements
- `sta` gives you starters only 
- `rel` gives you relievers only

```
fg_pitch_leaders(2018,2018, pitcher_type = "sta") %>% slice (1:10) %>% .[,1:10]

   playerid Seasons  #             Name      Team Age  W  L  ERA  G
1     10954    2018  1     Jacob deGrom      Mets  30 10  9 1.70 32
2     13543    2018  2      Blake Snell      Rays  25 21  5 1.89 31
3     12703    2018  3     Trevor Bauer   Indians  27 12  6 2.26 27
4     16149    2018  4       Aaron Nola  Phillies  25 17  6 2.37 33
5      8700    2018  5 Justin Verlander    Astros  35 16  9 2.52 34
6      3137    2018  6     Max Scherzer Nationals  33 18  7 2.53 33
7      9803    2018  7    Miles Mikolas Cardinals  29 18  4 2.83 32
8     16256    2018  8    Kyle Freeland   Rockies  25 17  7 2.85 33
9     10811    2018  9 Mike Foltynewicz    Braves  26 13 10 2.85 31
10    13125    2018 10      Gerrit Cole    Astros  27 15  5 2.88 32
```

`scrape_statcast_leaderboards()`

The `scrape_statcast_leaderboards()` function can be used to access all of the leaderboards available from [BaseballSavant](https://baseballsavant.mlb.com) as csv downloads. The function isn't doing anything too sophisticated; it simply builds the appropriate url for the csv download based on a series of parameters and then reads the csv into `R`.

Users specificy which leaderboard they want to download using the `leaderboard` argument. The following are currently available:

- `exit_velocity_barrels`
- `expected_statistics`
- `pitch_arsenal`
- `outs_above_average`
- `directional_oaa`
- `catch_probability`
- `pop_time`
- `sprint_speed`
- `running_splits_90_ft`

Each leaderboard has different parameters that can be specific to alter the content of the downloads, but not all parameters work for every leaderboard. (I would check the leaderboard interface on BaseballSavant directly if you are not sure which ones to use.) Some of the leaderboards do not include a variable for the `year` selected, so the function will check if it exists and, if not, it will add a column based on your parameter setting.

Here is an example of the `expected_statistics` leaderboard for pitchers who faced at least 250 batters in 2018:

```
payload <- scrape_savant_leaderboards(leaderboard = "expected_statistics", 
					year = 2018, 
					player_type = "pitcher", 
					min_pa = 250)

payload %>%
	arrange(est_woba) %>% 
	select(year:last_name, pa, woba:est_woba_minus_woba_diff)

    year last_name    pa  woba est_woba est_woba_minus_woba_diff
   <int> <chr>     <int> <dbl>    <dbl>                    <dbl>
 1  2018 Diaz        280 0.214    0.212                    0.002
 2  2018 Hader       306 0.219    0.229                   -0.01 
 3  2018 Treinen     315 0.187    0.23                    -0.043
 4  2018 Ottavino    309 0.231    0.23                     0.001
 5  2018 Sale        617 0.237    0.232                    0.005
 6  2018 Verlander   833 0.26     0.236                    0.024
 7  2018 Betances    272 0.259    0.236                    0.023
 8  2018 Pressly     292 0.267    0.241                    0.026
 9  2018 deGrom      835 0.23     0.243                   -0.013
10  2018 Scherzer    866 0.252    0.246                    0.006
# ... with 264 more rows
```

`milb_batter_game_logs_fg()`
`milb_pitcher_game_logs_fg()`

These functions were contributed by [Mat Adams](https://github.com/matawith1t), and they are similar to the game log functions included in an earlier release except that they will return minor league game logs for the player specified. The functions take only two arguments: `playerid` and `year`. The `playerid` is the minor league ID assigned by FanGrapsh. This can be found in the url slug for a minor league player's page. 

For example, here is the url for Vladimir Guerrero Jr.'s minor league game logs: https://www.fangraphs.com/statsd.aspx?playerid=sa920245&position=3B&gds=&gde=

You will see the `playerid=` portion of the url, and the actual ID follows (i.e. sa920245).

```
milb_batter_game_logs_fg(playerid = "sa920245", year = 2017) %>% slice(1:10)

                    name minor_playerid       Date Team Level  Opp  AVG G AB PA H
1  Vladimir Guerrero Jr.       sa920245 2017-04-07  TOR   (A) @LAD .000 1  1  3 0
2  Vladimir Guerrero Jr.       sa920245 2017-04-07  TOR   (A) @LAD .500 1  4  4 2
3  Vladimir Guerrero Jr.       sa920245 2017-04-08  TOR   (A)  LAD .500 1  4  5 2
4  Vladimir Guerrero Jr.       sa920245 2017-04-09  TOR   (A)  LAD .333 1  3  5 1
5  Vladimir Guerrero Jr.       sa920245 2017-04-10  TOR   (A) @TBR .250 1  4  4 1
6  Vladimir Guerrero Jr.       sa920245 2017-04-12  TOR   (A) @TBR .000 1  3  4 0
7  Vladimir Guerrero Jr.       sa920245 2017-04-12  TOR   (A) @TBR .667 1  3  3 2
8  Vladimir Guerrero Jr.       sa920245 2017-04-13  TOR   (A)  CLE .000 1  3  4 0
9  Vladimir Guerrero Jr.       sa920245 2017-04-14  TOR   (A)  CLE .000 1  3  4 0
10 Vladimir Guerrero Jr.       sa920245 2017-04-15  TOR   (A)  CLE .333 1  3  4 1
```

## Updgrades

`ncaa_scrape()`
- Updated to allow scraping of data for the 2019 season.

`edge_code()`
- Changes made to make function more compatible with [BaseballSavant](https://baseballsavant.mlb.com) data.
## Bug Fixes

`fg_bat_leaders()`
- `playerid` now returned as part of the data returned.  

`run_expectancy_code()`
- Removed filter for `final pitch==1` when not grouping by plate appearances.


# baseballr 0.3.4 (2018-05-29)

* Added a `NEWS.md` file to track changes to the package.

The latest release of the [`baseballr`](https://billpetti.github.io/baseballr/) package for `R` (0.3.4) includes a number of enhancements and bug fixes.

## New Functions

`run_expectancy_code()`

This function formats Baseball Savant data so that users can generate the run expectancy for different base-out or count-base-out states. It will also append the data frame with new variables necessary for generating linear weights (see new function below). The only argument is a data frame downloaded from Baseball Savant

Columns created and appended to Baseball Savant data:

- `final_pitch_game`: whether a pitch was the final one thrown in a game
- `final_pitch_inning`: whether a pitch is the final one thrown in an inning
- `final_pitch_at_bat`: whether a pitch is the final one thrown in an at bat
- `runs_scored_on_pitch`: how many runs scored as a result of the pitch
- `bat_score_start_inning`: the score for the batting team at the beginning of the inning
- `bat_score_end_inning`: the score for the batting team at the end of the inning
- `bat_score_after`: the score for the batting team after the pitch is thrown
- `cum_runs_in_inning`: how many cumulative runs have been scored from the beginning of the inning through the pitch
- `runs_to_end_inning`: how many runs were scored as a result of the pitch through the end of the inning
- `base_out_state` or `count_base_out_state`: the specific combination of base-outs or count-base-outs when the pitch was thrown
- `avg_re`: the average run expectancy of that base-out or count-base-out state
- `next_avg_re`: the average run expectancy of the base-out or count-base-out state that results from the pitch
- `change_re`: the change in run expectancy as a result of the pitch
- `re24`: the total change in run expectancy through the end of the inning resulting from the pitch based on the change in base-out or count-base-out state plus the number of runs scored as a result of the pitch/at bat

Example:

```r
> x2016_statcast_re <- run_expectancy_code(x2016_statcast)

> sample_n(x2016_statcast_re, 10) %>%
    select(final_pitch_inning:re24) %>%
    glimpse()

Observations: 10
Variables: 11
$ final_pitch_inning     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
$ bat_score_start_inning <dbl> 1, 0, 5, 0, 3, 2, 1, 0, 0, 0
$ bat_score_end_inning   <dbl> 2, 0, 5, 1, 3, 2, 5, 0, 0, 2
$ cum_runs_in_inning     <dbl> 1, 0, 0, 0, 0, 0, 2, 0, 0, 1
$ runs_to_end_inning     <dbl> 0, 0, 0, 1, 0, 0, 2, 0, 0, 1
$ base_out_state         <chr> "2  outs,  1b _ _", "0  outs,  _ _ _", "0  outs...
$ avg_re                 <dbl> 0.2149885, 0.5057877, 0.5057877, 0.5057877, 0.5...
$ next_base_out_state    <chr> "2  outs,  1b 2b _", "1  outs,  _ _ _", "1  out...
$ next_avg_re            <dbl> 0.4063525, 0.2718802, 0.2718802, 0.8629357, 0.2...
$ change_re              <dbl> 0.1913640, -0.2339075, -0.2339075, 0.3571479, -...
$ re24                   <dbl> 0.1913640, -0.2339075, -0.2339075, 0.3571479, -...
```

`run_expectancy_table()`

This functions works with the `run_expectancy_code` function and does the work of generating the run expectancy tables that are automatically exported into the Global Environment

Example:

```r
> x2016_statcast_re %>%
  run_expectancy_table() %>%
  print(n=Inf)

base_out_state     avg_re
<chr>               <dbl>
1 0  outs,  1b 2b 3b  2.13 
2 0  outs,  _ 2b 3b   1.95 
3 0  outs,  1b _ 3b   1.76 
4 1  outs,  1b 2b 3b  1.55 
5 0  outs,  1b 2b _   1.42 
6 1  outs,  _ 2b 3b   1.36 
7 0  outs,  _ _ 3b    1.36 
8 1  outs,  1b _ 3b   1.18 
9 0  outs,  _ 2b _    1.14 
10 1  outs,  _ _ 3b    0.951
11 1  outs,  1b 2b _   0.906
12 0  outs,  1b _ _    0.863
13 2  outs,  1b 2b 3b  0.689
14 1  outs,  _ 2b _    0.669
15 2  outs,  _ 2b 3b   0.525
16 1  outs,  1b _ _    0.520
17 0  outs,  _ _ _     0.506
18 2  outs,  1b _ 3b   0.456
19 2  outs,  1b 2b _   0.406
20 2  outs,  _ _ 3b    0.366
21 2  outs,  _ 2b _    0.299
22 1  outs,  _ _ _     0.272
23 2  outs,  1b _ _    0.215
24 2  outs,  _ _ _     0.106
```

`linear_weights_savant()`

This function works in tandem with `run_expectancy_code()` to generate linear weights for offensive events after the Baseball Savant data has been properly formatted. Currently, the function will return linear weights above average and linear weights above outs. It does not apply any scaling to align with league wOBA. Users can do that themselves if they like, or it may be added to a future version of the function.

Example:

```r

> x2016_statcast_re %>%
  linear_weights_savant() %>%
  print(n=Inf)

A tibble: 7 x 3
events       linear_weights_above_average linear_weights_above_outs
 <chr>                               <dbl>                     <dbl>
1 home_run                            1.38                      1.63 
2 triple                              1.00                      1.25 
3 double                              0.730                     0.980
4 single                              0.440                     0.690
5 hit_by_pitch                        0.320                     0.570
6 walk                                0.290                     0.540
7 outs                               -0.250                     0.   
```

I used Baseball Savant data from 2010-2015 and compared the linear weights generated by `baseballr` to those by Tom Tango using retrosheet data. `baseballr`'s weights are generally a little lower than what Tango generated, but that could be due to a number of things, such as the data source, code, etc., but the values appear reasonable enough to be reliable:

| base_out_state     | baseballr_2010_2015 | tango_2010_2015 | diff  | %_diff |
|--------------------|---------------------|-----------------|-------|--------|
| 0  outs,  1b 2b 3b | 2.27                | 2.29            | -0.02 | -1%    |
| 0  outs,  _ 2b 3b  | 1.96                | 1.96            | 0     | 0%     |
| 0  outs,  1b _ 3b  | 1.76                | 1.78            | -0.03 | -1%    |
| 1  outs,  1b 2b 3b | 1.51                | 1.54            | -0.03 | -2%    |
| 0  outs,  1b 2b _  | 1.42                | 1.44            | -0.02 | -1%    |
| 0  outs,  _ _ 3b   | 1.38                | 1.38            | 0     | 0%     |
| 1  outs,  _ 2b 3b  | 1.35                | 1.35            | 0     | 0%     |
| 1  outs,  1b _ 3b  | 1.1                 | 1.13            | -0.03 | -2%    |
| 0  outs,  _ 2b _   | 1.09                | 1.1             | -0.01 | -1%    |
| 1  outs,  _ _ 3b   | 0.93                | 0.95            | -0.02 | -2%    |
| 1  outs,  1b 2b _  | 0.86                | 0.88            | -0.02 | -3%    |
| 0  outs,  1b _ _   | 0.84                | 0.86            | -0.02 | -2%    |
| 2  outs,  1b 2b 3b | 0.71                | 0.75            | -0.04 | -5%    |
| 1  outs,  _ 2b _   | 0.65                | 0.66            | -0.01 | -2%    |
| 2  outs,  _ 2b 3b  | 0.54                | 0.58            | -0.04 | -7%    |
| 1  outs,  1b _ _   | 0.5                 | 0.51            | -0.01 | -2%    |
| 0  outs,  _ _ _    | 0.48                | 0.48            | 0     | -1%    |
| 2  outs,  1b _ 3b  | 0.45                | 0.48            | -0.03 | -7%    |
| 2  outs,  1b 2b _  | 0.41                | 0.43            | -0.02 | -4%    |
| 2  outs,  _ _ 3b   | 0.33                | 0.35            | -0.02 | -6%    |
| 2  outs,  _ 2b _   | 0.31                | 0.32            | -0.01 | -3%    |
| 1  outs,  _ _ _    | 0.25                | 0.25            | 0     | -1%    |
| 2  outs,  1b _ _   | 0.21                | 0.22            | -0.01 | -6%    |
| 2  outs,  _ _ _    | 0.1                 | 0.1             | 0     | -2%    |

We also had some great contributions by others that I've added into this release:

`label_statcast_imputed_data()`

[Ben Dilday](https://github.com/bdilday) again contributes with a cool experimental function meant to tag batted ball cases where significant imputation may have been used to generate some of the Statcast values by MLBAM, i.e. `launch_speed` and `launch_angle`. You can read more about Ben's function [here](https://github.com/BillPetti/baseballr/pull/71).

`fg_park()`

[Sam Boysel](https://github.com/sboysel) updated the park factors function so that it now includes the new columns added by FanGraphs (5-year, 3-year, 1-year park factors) and ensures the column names are correct

## Updgrades

`fg_bat_leaders()`

- `playerid` now returned as part of the data returned.  
- Dozens of additional variables are also returned, including aggregate data from Pitch Info as well as contact type.

## Bug Fixes

`process_statcast_payload()`
- hc_x, hc_y are now converted to numeric  


# baseballr 0.3.3 (2017-05-08)

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

# baseballr 0.3.2 (2017-09-12)
 
The latest release of the [`baseballr`](https://billpetti.github.io/baseballr/) package for `R` includes a number of enhancements and bug fixes.

In terms of new functions, `statline_from_statcast` allows users to take raw pitch-by-pitch data from Statcast/PITCHf/x and calculate aggregated, statline-like output. Examples include count data such as number of singles, doubles, etc., as well as rate metrics like Slugging and wOBA on swings or contact.

The function only has two arguments:

* `df`: a dataframe that includes pitch-by-pitch information. The function assumes the following columns are present: `events`, `description`, `game_date`, and `type`.
* `base`: base indicates what the denomincator should be for the rate stats that are calculated. The function defaults to "swings", but you can also choose to use "contact"

Here is an example using all data from the week of 2017-09-04. Here, we want to see a statline for all hitters based on swings:

```r
test <- scrape_statcast_savant_batter_all("2017-09-04", "2017-09-10")

statline_from_statcast(test)

year swings batted_balls  X1B X2B X3B  HR swing_and_miss swinging_strike_percent    ba
1 2017  13790        10663 1129 352  37 259           3127                   0.227 0.129

obp   slg   ops  woba
1 0.129 0.216 0.345 0.144
```

You can also combine the `statline_from_statcast` function with a loop to create statlines for multiple players at once.

Example: calculate statlines for batters on contact for all games played 2017-09-04 through 2017-09-10:

```r
test <- scrape_statcast_savant_batter_all("2017-09-04", "2017-09-10")

output <- data.frame()

for (i in c("Jose Ramirez", "J.D. Martinez", "Francisco Lindor", "Gary Sanchez", "Rhys Hoskins")) {
  reduced_test <- test %>%
    filter(player_name == i)
  x <- statline_from_statcast(reduced_test, base = "contact")
  x$player <- i
  x <- x %>%
    select(player, everything())
  output <- rbind(output, x) %>%
    arrange(desc(woba))
}

print(output, width = Inf)

# A tibble: 5 x 12
            player  year batted_balls   X1B   X2B   X3B    HR    ba   obp   slg   ops  woba
             <chr> <chr>        <dbl> <dbl> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <dbl> <dbl>
1    J.D. Martinez  2017           17     4     1     0     7 0.706 0.706 2.000 2.706 1.092
2     Gary Sanchez  2017           11     3     1     0     2 0.545 0.545 1.182 1.727 0.710
3 Francisco Lindor  2017           27     4     2     1     3 0.370 0.370 0.852 1.222 0.498
4     Rhys Hoskins  2017           14     2     1     0     2 0.357 0.357 0.857 1.214 0.495
5     Jose Ramirez  2017           16     0     0     0     3 0.188 0.188 0.750 0.938 0.370
```

# baseballr 0.3.1 (2016-11-22)

The latest release of the [`baseballr`](https://billpetti.github.io/baseballr/) includes a function for acquiring player statistics from the [NCAA's website](http://stats.ncaa.org) for baseball teams across the three major divisions (I, II, III).

The function, `ncaa_scrape`, requires the user to pass values for three parameters for the function to work:

`school_id`: numerical code used by the NCAA for each school
`year`: a four-digit year
`type`: whether to pull data for batters or pitchers

If you want to pull batting statistics for Vanderbilt for the 2013 season, you would use the following:

```r
> baseballr::ncaa_scrape(736, 2013, "batting") %>%
+     select(year:OBPct)
   year     school   conference division Jersey            Player Yr Pos GP GS    BA OBPct
1  2013 Vanderbilt Southeastern        1     18 Yastrzemski, Mike Sr  OF 66 66 0.312 0.411
2  2013 Vanderbilt Southeastern        1     20   Harrell, Connor Sr  OF 66 66 0.312 0.418
3  2013 Vanderbilt Southeastern        1      3      Conde, Vince So INF 66 65 0.307 0.380
4  2013 Vanderbilt Southeastern        1      6        Kemp, Tony Jr  OF 66 66 0.391 0.471
5  2013 Vanderbilt Southeastern        1     55    Gregor, Conrad Jr  OF 65 65 0.308 0.440
6  2013 Vanderbilt Southeastern        1      9    Turner, Xavier Fr INF 59 51 0.324 0.387
7  2013 Vanderbilt Southeastern        1      5    Navin, Spencer Jr   C 57 56 0.302 0.430
8  2013 Vanderbilt Southeastern        1     51        Lupo, Jack Sr  OF 57 51 0.297 0.352
9  2013 Vanderbilt Southeastern        1      8    Wiseman, Rhett Fr  OF 54 11 0.289 0.360
10 2013 Vanderbilt Southeastern        1     10     Norwood, John So  OF 33  9 0.328 0.388
11 2013 Vanderbilt Southeastern        1     43      Wiel, Zander So INF 33 15 0.305 0.406
12 2013 Vanderbilt Southeastern        1     44     Harvey, Chris So   C 29 13 0.250 0.328
13 2013 Vanderbilt Southeastern        1     42   McKeithan, Joel Jr INF 25 12 0.220 0.267
14 2013 Vanderbilt Southeastern        1     39       Smith, Kyle Fr INF 23  7 0.250 0.455
15 2013 Vanderbilt Southeastern        1     17    Harris, Andrew Sr INF 21  0 0.125 0.222
16 2013 Vanderbilt Southeastern        1      2   Campbell, Tyler Fr INF 12  2 0.312 0.389
17 2013 Vanderbilt Southeastern        1      7   Swanson, Dansby Fr INF 11  4 0.188 0.435
18 2013 Vanderbilt Southeastern        1     25        Luna, D.J. Jr INF  8  0 0.000 0.333
19 2013 Vanderbilt Southeastern        1     23      Cooper, Will So  OF  4  0 1.000 1.000
20 2013 Vanderbilt Southeastern        1      -            Totals  -   -  -  - 0.313 0.407
21 2013 Vanderbilt Southeastern        1      -   Opponent Totals  -   -  -  - 0.220 0.320
```

The same can be done for pitching, just by changing the `type` parameter:

```r
> baseballr::ncaa_scrape(736, 2013, "pitching") %>%
+     select(year:ERA)
   year     school   conference division Jersey           Player Yr Pos GP App GS  ERA
1  2013 Vanderbilt Southeastern        1     11     Beede, Tyler So   P 37  17 17 2.32
2  2013 Vanderbilt Southeastern        1     33    Miller, Brian So   P 32  32 NA 1.58
3  2013 Vanderbilt Southeastern        1     35    Ziomek, Kevin Jr   P 32  17 17 2.12
4  2013 Vanderbilt Southeastern        1     15   Fulmer, Carson Fr   P 26  26 NA 2.39
5  2013 Vanderbilt Southeastern        1     39      Smith, Kyle Fr INF 23   1 NA 0.00
6  2013 Vanderbilt Southeastern        1     28    Miller, Jared So   P 22  22 NA 2.31
7  2013 Vanderbilt Southeastern        1     19     Rice, Steven Jr   P 21  21 NA 2.57
8  2013 Vanderbilt Southeastern        1     13  Buehler, Walker Fr   P 16  16  9 3.14
9  2013 Vanderbilt Southeastern        1     22  Pfeifer, Philip So   P 15  15 12 3.68
10 2013 Vanderbilt Southeastern        1     12  Ravenelle, Adam So   P 11  11 NA 3.18
11 2013 Vanderbilt Southeastern        1     40   Pecoraro, T.J. Jr   P 10  10  7 5.97
12 2013 Vanderbilt Southeastern        1     45  Ferguson, Tyler Fr   P  8   8  4 4.21
13 2013 Vanderbilt Southeastern        1     27 Kolinsky, Keenan Jr   P  2   2 NA 0.00
14 2013 Vanderbilt Southeastern        1     24    Wilson, Nevin So   P  1   1 NA 0.00
15 2013 Vanderbilt Southeastern        1      -           Totals  -   -  -  NA NA 2.76
16 2013 Vanderbilt Southeastern        1      -  Opponent Totals  -   -  -  NA NA 6.19
```

Now, the function is dependent on the user knowing the `school_id` used by the NCAA website. Given that, I've included a `school_id_lu` function so that users can find the `school_id` they need.

Just pass a string to the function and it will return possible matches based on the school's name:

```r
> school_id_lu("Vand")
# A tibble: 4 × 6
      school   conference school_id  year division conference_id
       <chr>        <chr>     <dbl> <dbl>    <dbl>         <dbl>
1 Vanderbilt Southeastern       736  2013        1           911
2 Vanderbilt Southeastern       736  2014        1           911
3 Vanderbilt Southeastern       736  2015        1           911
4 Vanderbilt Southeastern       736  2016        1           911
```

# baseballr 0.2.1 (2016-10-08)


Updates to functions in this release:

`scrape_statcast_savant_batter`<br>
`scrape_statcast_savant_pitcher`<br>

New functions in this release:

`code_barrel` <br>

The research team at Major League Baseball Advanced Media have developed a way to categorize batted balls that on average having a batting average over .500 and slugging over 1.500. The specific coding criteria can be found in comment #2 [here] (http://tangotiger.com/index.php/site/comments/statcast-lab-barrels#2). 

Now, whenver a user scrapes Statcast data using either the `scrape_statcast_savant_batter` or `scrape_statcast_savant_pitcher` functions the results will include a column `barrel`, where if the batted ball matches the barrel criteria it will code as 1, otherwise 0.

Example:

```r
> scrape_statcast_savant_batter(start_date = "2016-04-06", end_date = "2016-04-15", batterid = 621043) %>% 
+     filter(type == "X") %>%
+     filter(!is.na(barrel)) %>%
+     select(player_name, game_date, hit_angle, hit_speed, barrel) %>%
+     tail()
[1] "Be patient, this may take a few seconds..."
[1] "Data courtesy of Baseball Savant and MLBAM (baseballsavant.mlb.com)"
     player_name  game_date hit_angle hit_speed barrel
25 Carlos Correa 2016-04-07     31.10    103.33      1
26 Carlos Correa 2016-04-07     27.77     87.25      0
27 Carlos Correa 2016-04-06     29.62    103.97      1
28 Carlos Correa 2016-04-06      0.11    105.20      0
29 Carlos Correa 2016-04-06     23.76    113.55      1
30 Carlos Correa 2016-04-06     -2.18    113.39      0
```
If you already have Statcast data--say, in a database that you've been collecting--I've also included a simple function that will take a dataframe and code whether each row contains a barrel or not. All you need to do is pass your dataframe to `code_barrel`.


# baseballr 0.2.0 (2016-08-25)


Functions added to this release:

`scrape_statcast_savant_batter`<br>
`scrape_statcast_savant_pitcher`<br>
`playerid_lookup`

The two savant functions allow a user to retrieve PITCHf/x and Statcast data for either a specific batter or pitcher from [Baseball Savants' Statcast Search] (https://baseballsavant.mlb.com/statcast_search). The user needs to provide a start date, end date, and the batter or pitcher's MLBAMID.

Example:

```r
> scrape_statcast_savant_batter(start_date = "2016-04-06", end_date = "2016-04-15", batterid = 621043) %>% 
     filter(type == "X") %>%
     select(3,7,54:56) %>%
     tail()
[1] "Be patient, this may take a few seconds..."
[1] "Data courtesy of Baseball Savant and MLBAM  (baseballsavant.mlb.com)"     
    game_date   player_name hit_distance_sc hit_speed hit_angle
26 2016-04-07 Carlos Correa             385    103.33     31.10
27 2016-04-07 Carlos Correa             288     87.25     27.77
28 2016-04-06 Carlos Correa             392    103.97     29.62
29 2016-04-06 Carlos Correa             189    105.20      0.11
30 2016-04-06 Carlos Correa             462    113.55     23.76
31 2016-04-06 Carlos Correa             228    113.39     -2.18
```

Since the savant functions require users to pass a valid MLBAMID, a lookup function is included that leverages the Chadwich public register. Users provide a text string and only those players with that string present in their last name will be returned.

Here is an example where the user is looking for players with the last name "Seager":

```r
> playerid_lookup("Seager")
[1] "Be patient, this may take a few seconds..."
[1] "Data courtesy of the Chadwick Bureau Register (https://github.com/chadwickbureau/register)"
  first_name last_name  given_name name_suffix nick_name birth_year mlb_played_first mlbam_id retrosheet_id  bbref_id fangraphs_id
1        Ben    Seager         Ben                               NA               NA       NA                                   NA
2      Corey    Seager  Corey Drew                             1994             2015   608369      seagc001 seageco01        13624
3     Justin    Seager Justin Ryan                             1992               NA   643529                                   NA
4       Kyle    Seager  Kyle Duerr                             1987             2011   572122      seagk001 seageky01         9785
```


# baseballr 0.1.4 (2016-05-24)


Functions added to this release:

`pitcher_boxscore`: This function allows a user to retrieve a boxscore of pitcher statistics for any game played in the PITCHf/x era (2008-current). The function takes a boxscore.xml url as it's only argument and returns boxscore data for both the home and away pitchers.

Example:

```r
> pitcher_boxscore("http://gd2.mlb.com/components/game/mlb/year_2016/month_05/day_21/gid_2016_05_21_milmlb_nynmlb_1/boxscore.xml") %>% select(id:so)
Source: local data frame [9 x 10]

      id      name name_display_first_last   pos   out    bf    er     r     h    so
   (chr)     (chr)                   (chr) (chr) (chr) (chr) (chr) (chr) (chr) (chr)
1 605200    Davies             Zach Davies     P    16    22     4     4     5     5
2 430641     Boyer            Blaine Boyer     P     2     4     0     0     2     0
3 448614 Torres, C           Carlos Torres     P     3     4     0     0     0     1
4 592804 Thornburg         Tyler Thornburg     P     3     3     0     0     0     1
5 518468    Blazek          Michael Blazek     P     1     5     1     1     2     0
6 594798    deGrom            Jacob deGrom     P    15    23     4     4     5     7
7 570663    Robles           Hansel Robles     P     6     7     0     0     0     3
8 592665   Reed, A            Addison Reed     P     3     5     0     0     1     2
9 544727   Familia          Jeurys Familia     P     3     4     0     0     1     1
```

`batter_boxscore`: This function does the same thing as `pitcher_boxscore`, but for batters.

Example:

```r
> batter_boxscore("http://gd2.mlb.com/components/game/mlb/year_2016/month_05/day_21/gid_2016_05_21_milmlb_nynmlb_1/boxscore.xml") %>% select(id:bb)
Source: local data frame [29 x 10]

       id        name name_display_first_last   pos    bo    ab    po     r     a    bb
    (chr)       (chr)                   (chr) (chr) (chr) (chr) (chr) (chr) (chr) (chr)
1  542340      Villar         Jonathan Villar    SS   100     5     1     0     0     0
2  571697     Gennett         Scooter Gennett    2B   200     4     2     0     3     1
3  518960      Lucroy         Jonathan Lucroy     C   300     5     8     0     1     0
4  474892      Carter            Chris Carter    1B   400     4    10     0     2     0
5  543590 Nieuwenhuis        Kirk Nieuwenhuis    CF   500     4     0     0     0     0
6  431094     Hill, A              Aaron Hill    3B   600     1     1     2     4     3
7  502100     Presley            Alex Presley    LF   700     3     0     1     0     1
8  570717   Flores, R            Ramon Flores    RF   800     3     2     1     0     0
9  605200      Davies             Zach Davies     P   900     2     1     0     1     0
10 430641       Boyer            Blaine Boyer     P   901     0     0     0     0     0
```



# baseballr 0.1.3 (2016-03-25)


Functions added to this release:

`edge_code`: This function allows a user to pass their own dataframe and have individual pitches coded according to the scheme provided by Edge%. The dataframe must contain at least three columns of data: `b_height`, `stand`, `px`, and `pz`.

Example (based on data from "2015-04-05"):

```r
> edge_code(df) %>% .[, c(6:7, 27:28, 82)] %>% head(10)
   stand b_height     px    pz    location
1      L      6-3  0.416 2.963 Inside Edge
2      L      6-3 -0.191 2.347       Heart
3      L      6-3 -0.518 3.284  Upper Edge
4      L      6-3 -0.641 1.221 Out of Zone
5      L      6-3 -1.821 2.083 Out of Zone
6      L      6-3  0.627 2.397 Inside Edge
7      L      6-5 -1.088 1.610 Out of Zone
8      L      6-5 -0.257 2.047  Lower Edge
9      L      6-5     NA    NA        <NA>
10     L      6-3 -1.539 1.525 Out of Zone
```

Functions updated for this release:

`standings_on_date_bref`: [JonathanBob](https://github.com/JonathanBob) updated this funtion to allow for records to be returned for the given date or from that date forward. Also, users can input a full date string instead of three separate arguments for the day, month, and year. Users can also choose to pull records for the AL and NL overall, not just for a given division.

Example:

```r
> standings_on_date_bref("2015-08-01", "NL East", from = FALSE)
$`NL East`
   Tm  W  L  W-L%   GB  RS  RA pythW-L%
1 WSN 54 48 0.529   -- 422 391    0.535
2 NYM 54 50 0.519  1.0 368 373    0.494
3 ATL 46 58 0.442  9.0 379 449    0.423
4 MIA 42 62 0.404 13.0 370 408    0.455
5 PHI 41 64 0.390 14.5 386 511    0.374

> standings_on_date_bref("2015-08-01", "NL East", from = TRUE)
$`NL East`
   Tm  W  L  W-L%   GB  RS  RA pythW-L%
1 NYM 36 22 0.621   -- 315 240    0.622
2 MIA 29 29 0.500  7.0 243 270    0.452
3 WSN 29 31 0.483  8.0 281 244    0.564
4 PHI 22 35 0.386 13.5 240 298    0.402
5 ATL 21 37 0.362 15.0 194 311    0.297
```


# baseballr 0.1.2 (2016-03-07)


Functions added to this release:

`edge_scrape_split()`: This function builds of off `edge_scrape()` and adds the ability to view the data split by batter and pitcher handedness. As with `edge_scrape()`, the function returns a dataframe grouped by either pitchers or batters and the percentge of pitches in each of the various Edge zones, but adds in handedness.

Example (Edge% splits by batters with handedness):

```r
> edge_scrape_split("2015-04-05", "2015-04-05", "batter") %>% .[,c(1:5,9:14)]

       batter_name batter p_throws stand All_pitches Upper_Edge Lower_Edge Inside_Edge Outside_Edge Heart Out_of_Zone
             (chr)  (dbl)    (chr) (chr)       (int)      (dbl)      (dbl)       (dbl)        (dbl) (dbl)       (dbl)
1    Matt Holliday 407812        L     R          11      0.000      0.182       0.000        0.182 0.182       0.455
2    Matt Holliday 407812        R     R          10      0.000      0.000       0.000        0.200 0.300       0.500
3       David Ross 424325        R     R           8      0.000      0.000       0.000        0.125 0.625       0.250
4   Jhonny Peralta 425509        L     R           9      0.000      0.111       0.444        0.000 0.111       0.333
5   Jhonny Peralta 425509        R     R           6      0.167      0.000       0.000        0.167 0.167       0.500
6  Adam Wainwright 425794        L     R           8      0.000      0.125       0.000        0.000 0.125       0.750
7  Adam Wainwright 425794        R     R           3      0.000      0.000       0.000        0.333 0.667       0.000
8    Yadier Molina 425877        L     R          13      0.077      0.077       0.000        0.000 0.077       0.769
9    Yadier Molina 425877        R     R           7      0.143      0.000       0.143        0.143 0.143       0.429
10    Jonathan Jay 445055        L     L           9      0.000      0.000       0.222        0.000 0.556       0.222
..             ...    ...      ...   ...         ...        ...        ...         ...          ...   ...         ...
```


# baseballr 0.1.0 (2016-03-01)


Functions added to this release:

`fip_plus()`: This function mimics the functionality in the `woba_plus()` function, except that the unit of analysis is pitchers. The function will generate Fielding Indepedent Pitching (FIP) for each pitcher in the data set that is passed to the function, along with wOBA against and wOBA against on contact.

Example:

```r
> daily_pitcher_bref("2015-04-05", "2015-04-30") %>% fip_plus() %>% select(season, Name, IP, ERA, SO, uBB, HBP, HR, FIP, wOBA_against, wOBA_CON_against) %>% arrange(desc(IP)) %>% head(10)
   season            Name   IP  ERA SO uBB HBP HR  FIP wOBA_against wOBA_CON_against
1    2015    Johnny Cueto 37.0 1.95 38   4   2  3 2.62        0.210            0.276
2    2015  Dallas Keuchel 37.0 0.73 22  11   0  0 2.84        0.169            0.151
3    2015      Sonny Gray 36.1 1.98 25   6   1  1 2.69        0.218            0.239
4    2015      Mike Leake 35.2 3.03 25   7   0  5 4.16        0.240            0.281
5    2015 Felix Hernandez 34.2 1.82 36   6   3  1 2.20        0.225            0.272
6    2015    Corey Kluber 34.0 4.24 36   5   2  2 2.40        0.295            0.391
7    2015   Jake Odorizzi 33.2 2.41 26   8   1  0 2.38        0.213            0.228
8    2015 Josh Collmenter 32.2 2.76 16   3   0  1 2.82        0.290            0.330
9    2015   Bartolo Colon 32.2 3.31 25   1   0  4 3.29        0.280            0.357
10   2015    Zack Greinke 32.2 1.93 27   7   1  2 3.01        0.240            0.274
```

`edge_scrape()`: This function allows the user to scrape PITCHf/x data from the GameDay application using Carson Sievert's [pitchRx](https://github.com/cpsievert/pitchRx) package and to calculate metrics associated with [Edge%](https://billpetti.shinyapps.io/edge_shiny/). The function returns a data.frame grouped by either pitchers or batters and the percentge of pitches in each of the various Edge zones.

Example (pitchers):

```r
> edge_scrape("2015-04-06", "2015-04-07", "pitcher") %>% .[, c(1:3,7:12)] %>% head(10)
     pitcher_name pitcher All_pitches Upper_Edge Lower_Edge Inside_Edge Outside_Edge Heart Out_of_Zone
            (chr)   (dbl)       (int)      (dbl)      (dbl)       (dbl)        (dbl) (dbl)       (dbl)
1   Bartolo Colon  112526          86      0.035      0.081       0.058        0.151 0.209       0.465
2  LaTroy Hawkins  115629          12      0.000      0.333       0.000        0.000 0.083       0.583
3      Joe Nathan  150274           4      0.000      0.000       0.000        0.000 0.000       1.000
4   Buddy Carlyle  234194           9      0.000      0.222       0.000        0.000 0.333       0.444
5    Jason Grilli  276351          14      0.000      0.000       0.214        0.000 0.286       0.500
6     Kevin Gregg  276514          17      0.000      0.000       0.118        0.176 0.235       0.471
7  Joaquin Benoit  276542          19      0.053      0.053       0.105        0.000 0.158       0.632
8  Ryan Vogelsong  285064          99      0.010      0.051       0.141        0.061 0.182       0.556
9  Jeremy Affeldt  346793           5      0.000      0.000       0.200        0.000 0.000       0.800
10  Grant Balfour  346797          21      0.095      0.000       0.000        0.048 0.333       0.524
```

Example (batters):

```r
> edge_scrape("2015-04-06", "2015-04-07", "batter") %>% .[, c(1:3,7:12)] %>% head(10)
       batter_name batter All_pitches Upper_Edge Lower_Edge Inside_Edge Outside_Edge Heart Out_of_Zone
             (chr)  (dbl)       (int)      (dbl)      (dbl)       (dbl)        (dbl) (dbl)       (dbl)
1    Bartolo Colon 112526           7      0.000      0.000       0.429        0.000 0.143       0.429
2     Torii Hunter 116338          19      0.000      0.105       0.105        0.105 0.000       0.684
3      David Ortiz 120074          18      0.056      0.000       0.111        0.056 0.222       0.556
4   Alex Rodriguez 121347          17      0.000      0.000       0.353        0.000 0.118       0.529
5   Aramis Ramirez 133380          23      0.000      0.087       0.261        0.000 0.261       0.391
6    Adrian Beltre 134181          26      0.000      0.038       0.154        0.115 0.231       0.462
7   Carlos Beltran 136860          22      0.136      0.045       0.136        0.000 0.136       0.545
8  Michael Cuddyer 150212          14      0.000      0.214       0.214        0.000 0.214       0.357
9    Jimmy Rollins 276519          41      0.024      0.122       0.049        0.049 0.220       0.537
10  Ryan Vogelsong 285064          10      0.000      0.200       0.300        0.000 0.200       0.300
```
