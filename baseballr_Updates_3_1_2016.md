## Update Notes for baseballr package: Version 0.1.0 
### March 1, 2016

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

`edge_scrape()`: This function allows the user to scrape PITCHf/x data from the GameDay application using Carson Sievert's pitchRx package and to calculate metrics associated with Edge%. The function returns a data.frame grouped by either pitchers or batters and the percentge of pitches in each of the various Edge zones.

Example (pitchers):

```r
> edge_scrape(start = "2015-04-06", end = "2015-04-07", "pitcher") %>% .[, c(1:3,7:12)] %>% head(10)
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
> edge_scrape(start = "2015-04-06", end = "2015-04-07", "batter") %>% .[, c(1:3,7:12)] %>% head(10)
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