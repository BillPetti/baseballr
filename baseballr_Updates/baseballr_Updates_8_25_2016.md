## Update Notes for baseballr package: Version 0.2.0
### August 25, 2016

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
