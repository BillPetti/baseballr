## Update Notes for baseballr package: Version 0.1.2 
### March 7, 2016

Functions added to this release:

`edge_scrape_split()`: This function builds of off `edge_scrape()` and adds the ability to view the data split by batter and pitcher handedness. As with `edge_scrape()`, the function returns a dataframe grouped by either pitchers or batters and the percentge of pitches in each of the various Edge zones, but adds in handedness.

Example (Edge% splits by batters with handedness):

```r
> edge_scrape_split("2015-04-05", "2015-04-05", "batter")

        batter_name batter p_throws stand All_pitches All_calls Called_Strike Called_strike_rate Upper_Edge
             (chr)  (dbl)    (chr) (chr)       (int)     (dbl)         (dbl)              (dbl)      (dbl)
1    Matt Holliday 407812        L     R          11         5             1              0.200      0.000
2    Matt Holliday 407812        R     R          10         5             1              0.200      0.000
3       David Ross 424325        R     R           8         2             0              0.000      0.000
4   Jhonny Peralta 425509        L     R           9         4             2              0.500      0.000
5   Jhonny Peralta 425509        R     R           6         1             0              0.000      0.167
6  Adam Wainwright 425794        L     R           8         2             1              0.500      0.000
7  Adam Wainwright 425794        R     R           3         0             0                NaN      0.000
8    Yadier Molina 425877        L     R          13         9             0              0.000      0.077
9    Yadier Molina 425877        R     R           7         2             0              0.000      0.143
10    Jonathan Jay 445055        L     L           9         3             1              0.333      0.000
..             ...    ...      ...   ...         ...       ...           ...                ...        ...
Variables not shown: Lower_Edge (dbl), Inside_Edge (dbl), Outside_Edge (dbl), Heart (dbl), Out_of_Zone (dbl)
```

