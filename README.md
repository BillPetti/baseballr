# baseballr 0.4 <br>
**(latest version released 2018-04-09)**

`baseballr` is a package written for R focused on baseball analysis. It includes functions for scraping various data from websites, such as FanGraphs.com, Baseball-Reference.com, and baseballsavant.com. It also includes functions for calculating metrics, such as wOBA, FIP, and team-level consistency over custom time frames.

You can read more about some of the functions and how to use them at its [official site](http://billpetti.github.io/baseballr/) as well as this [Hardball Times article](http://www.hardballtimes.com/developing-the-baseballr-package-for-r/).

It can be installed by using [`devtools`](https://github.com/hadley/devtools):

```R
require(devtools)
install_github("BillPetti/baseballr")
require(baseballr)
```
The package consists of two main sets of functions: data acquisition and metric calculation.

For example, if you want to see the standings for a specific MLB division on a given date, you can use the `standings_on_date_bref()` function. Just pass the year, month, day, and division you want:

```R
> standings_on_date_bref("2015-08-01", "NL East", from = FALSE)
$`NL East`
   Tm  W  L  W-L%   GB  RS  RA pythW-L%
1 WSN 54 48 0.529   -- 422 391    0.535
2 NYM 54 50 0.519  1.0 368 373    0.494
3 ATL 46 58 0.442  9.0 379 449    0.423
4 MIA 42 62 0.404 13.0 370 408    0.455
5 PHI 41 64 0.390 14.5 386 511    0.374
```
Right now the function works as far as back as 1994, which is when both leagues split into three divisions.

You can also pull data for all hitters over a specific date range. Here are the results for all hitters from August 1st through October 3rd during the 2015 season:

```R
> head(daily_batter_bref("2015-08-01", "2015-10-03"))
  season             Name Age  Level          Team  G  PA  AB  R  H X1B X2B X3B HR RBI BB IBB uBB SO HBP SH SF GDP SB CS    BA   OBP
1   2015    Manny Machado  22 MLB-AL     Baltimore 59 266 237 36 66  43  10   0 13  32 26   1  25 42   2  0  1   5  6  4 0.278 0.353
2   2015       Matt Duffy  24 MLB-NL San Francisco 59 264 248 33 71  54  12   2  3  30 15   0  15 35   0  0  1   9  8  0 0.286 0.326
3   2015      Jose Altuve  25 MLB-AL       Houston 57 262 244 30 81  53  19   3  6  18 10   1   9 28   4  1  3   6 11  4 0.332 0.364
4   2015       Adam Eaton  26 MLB-AL       Chicago 58 262 230 37 74  56  12   1  5  31 23   1  22 55   5  2  2   1  9  4 0.322 0.392
5   2015    Shin-Soo Choo  32 MLB-AL         Texas 58 260 211 48 71  47  14   1  9  34 39   1  38 51   8  1  1   1  2  0 0.336 0.456
6   2015 Francisco Lindor  21 MLB-AL     Cleveland 58 259 224 35 79  51  17   4  7  32 18   0  18 38   1 11  5   4 10  2 0.353 0.395
    SLG   OPS
1 0.485 0.839
2 0.387 0.713
3 0.508 0.872
4 0.448 0.840
5 0.540 0.996
6 0.558 0.953
```

In terms of metric calculation, the package allows the user to calculate the consistency of team scoring and run prevention for any year using `team_consistency()`:

```R
> team_consistency(2015)
Source: local data frame [30 x 5]

    Team Con_R Con_RA Con_R_Ptile Con_RA_Ptile
   (chr) (dbl)  (dbl)       (dbl)        (dbl)
1    ARI  0.37   0.36          22           15
2    ATL  0.41   0.40          87           67
3    BAL  0.40   0.38          70           42
4    BOS  0.39   0.40          52           67
5    CHC  0.38   0.41          33           88
6    CHW  0.39   0.40          52           67
7    CIN  0.41   0.36          87           15
8    CLE  0.41   0.40          87           67
9    COL  0.35   0.34           7            3
10   DET  0.39   0.38          52           42
..   ...   ...    ...         ...          ...
```

You can also calculate wOBA per plate appearance and wOBA on contact for any set of data over any date range, provided you have the data available.

Simply pass the proper data frame to `woba_plus`:

```R
> x <- woba_plus(df)
> head(x)[,c(1,2,24,26,27)]
              Name         Team season  wOBA wOBA_CON
1     Bryce Harper    Nationals   2015 0.464    0.554
2       Joey Votto         Reds   2015 0.428    0.485
3 Paul Goldschmidt Diamondbacks   2015 0.422    0.517
4       Mike Trout       Angels   2015 0.418    0.519
5   Miguel Cabrera       Tigers   2015 0.415    0.462
6   Josh Donaldson    Blue Jays   2015 0.404    0.467
```
You can also generate these wOBA-based stats, as well as FIP, for pitchers using the `fip_plus()` function:

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

The `edge_scrape()` function allows the user to scrape PITCHf/x data from the GameDay application using Carson Sievert's [pitchRx](https://github.com/cpsievert/pitchRx) package and to calculate metrics associated with [Edge%](https://billpetti.shinyapps.io/edge_shiny/). The function returns a dataframe grouped by either pitchers or batters and the percentge of pitches in each of the various Edge zones.

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

More functionality will be added soon. Please leave any suggestions or bugs in the [Issues section](https://github.com/BillPetti/baseballr/issues).
