# baseballr

`baseballr` is a package written for R focused on baseball analysis. It includes functions for scraping various data from websites, such as FanGraphs.com and Baseball-Reference.com. It also includes functions for calculating metrics, such as wOBA, FIP, and team-level consistency over custom time frames.

You can read more about some of the functions and how to use them [here](http://www.hardballtimes.com/developing-the-baseballr-package-for-r/).

It can be installed by using [`devtools`](https://github.com/hadley/devtools):

```R
require(devtools)
install_github("BillPetti/baseballr")
require(baseballr)
```
The package consists of two main sets of functions: data acquisition and metric calculation.

For example, if you want to see the standings for a specific MLB division on a given date, you can use the `standings_on_date_bref()` function. Just pass the year, month, day, and division you want:

```R
> standings_on_date_bref(2015, 10, 1, "NL EAST")
   Tm  W  L  W-L%   GB  RS  RA pythW-L%
1 NYM 89 70 0.560   -- 681 608    0.552
2 WSN 81 78 0.509  8.0 698 633    0.545
3 MIA 69 90 0.434 20.0 599 663    0.454
4 ATL 64 95 0.403 25.0 561 760    0.365
5 PHI 62 97 0.390 27.0 611 795    0.382
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
More functionality will be added soon.
