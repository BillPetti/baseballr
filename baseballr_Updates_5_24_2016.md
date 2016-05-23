## Update Notes for baseballr package: Version 0.1.4 
### May 24, 2016

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