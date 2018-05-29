---
layout: page
title: baseballr current release notes
tags: rstats, baseballr
---
## May 29, 2018

The latest release of the [`baseballr`](https://billpetti.github.io/baseballr/) package for `R` (0.5) includes a number of enhancements and bug fixes.

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