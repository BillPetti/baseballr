## Update Notes for baseballr package: Version 0.2.1
### October 8, 2016

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

