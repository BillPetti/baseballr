## Update Notes for baseballr package: Version 0.1.2 
### March 7, 2016

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

