## Update Notes for baseballr package: Version 0.1.3 
### March 25, 2016

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
