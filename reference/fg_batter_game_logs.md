# **Scrape Batter Game Logs from FanGraphs**

This function allows you to scrape game logs by year for a batter from
FanGraphs.com.

## Usage

``` r
fg_batter_game_logs(playerid, year)
```

## Arguments

- playerid:

  This is the playerid used by FanGraphs for a given player

- year:

  The season for which game logs should be returned (use the YYYY
  format)

## Value

A data frame of batter game logs, one row per game, with the following
columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| PlayerName | character | Player name. |
| playerid | integer | FanGraphs player ID. |
| Date | character | Calendar date of the game (YYYY-MM-DD). |
| Team | character | Team the player appeared for. |
| Opp | character | Opponent team; leading @ indicates a road game. |
| season | integer | Season year. |
| Age | integer | Player age during the season. |
| BatOrder | character | Spot in the batting order. |
| Pos | character | Fielding position played. |
| G | numeric | Games (1 per row). |
| AB | numeric | At-bats. |
| PA | numeric | Plate appearances. |
| H | numeric | Hits. |
| 1B | numeric | Singles. |
| 2B | numeric | Doubles. |
| 3B | numeric | Triples. |
| HR | numeric | Home runs. |
| R | numeric | Runs scored. |
| RBI | numeric | Runs batted in. |
| BB | numeric | Walks (bases on balls). |
| IBB | numeric | Intentional walks. |
| SO | numeric | Strikeouts. |
| HBP | numeric | Times hit by pitch. |
| SF | numeric | Sacrifice flies. |
| SH | numeric | Sacrifice hits (bunts). |
| GDP | numeric | Grounded into double plays. |
| SB | numeric | Stolen bases. |
| CS | numeric | Times caught stealing. |
| AVG | numeric | Batting average. |
| GB | numeric | Ground balls. |
| FB | numeric | Fly balls. |
| LD | numeric | Line drives. |
| IFFB | numeric | Infield fly balls. |
| Pitches | numeric | Pitches seen. |
| Balls | numeric | Balls. |
| Strikes | numeric | Strikes. |
| IFH | numeric | Infield hits. |
| BU | numeric | Bunts. |
| BUH | numeric | Bunt hits. |
| BB% | numeric | Walk rate. |
| K% | numeric | Strikeout rate. |
| BB/K | numeric | Walk-to-strikeout ratio. |
| OBP | numeric | On-base percentage. |
| SLG | numeric | Slugging percentage. |
| OPS | numeric | On-base plus slugging. |
| ISO | numeric | Isolated power. |
| BABIP | numeric | Batting average on balls in play. |
| GB/FB | numeric | Ground-ball to fly-ball ratio. |
| LD% | numeric | Line-drive rate. |
| GB% | numeric | Ground-ball rate. |
| FB% | numeric | Fly-ball rate. |
| IFFB% | numeric | Infield-fly-ball rate. |
| HR/FB | numeric | Home-run-per-fly-ball rate. |
| IFH% | numeric | Infield-hit rate. |
| BUH% | numeric | Bunt-hit rate. |
| wOBA | numeric | Weighted on-base average. |
| wRAA | numeric | Weighted runs above average. |
| wRC | numeric | Weighted runs created. |
| Spd | numeric | Bill James Speed Score. |
| wRC+ | numeric | Weighted runs created plus (100 = average). |
| wBSR | numeric | Weighted base running runs. |
| WPA | numeric | Win probability added. |
| -WPA | numeric | Negative win probability added. |
| +WPA | numeric | Positive win probability added. |
| RE24 | numeric | Run expectancy based on 24 base-out states. |
| REW | numeric | Run expectancy wins. |
| pLI | numeric | Average leverage index. |
| PH | numeric | Pinch-hit appearances. |
| WPA/LI | numeric | Context-neutral win probability added. |
| Clutch | numeric | Clutch performance score. |
| FB%1 | numeric | Fastball usage rate. |
| FBv | numeric | Average four-seam fastball velocity (mph). |
| SL% | numeric | slider usage rate. |
| SLv | numeric | Average slider velocity (mph). |
| CT% | numeric | cutter usage rate. |
| CTv | numeric | Average cutter velocity (mph). |
| CB% | numeric | curveball usage rate. |
| CBv | numeric | Average curveball velocity (mph). |
| CH% | numeric | changeup usage rate. |
| CHv | numeric | Average changeup velocity (mph). |
| SF% | numeric | split-finger fastball usage rate. |
| SFv | numeric | Average split-finger fastball velocity (mph). |
| XX% | numeric | unknown/other pitch usage rate. |
| wFB | numeric | Total runs above average on the fastball. |
| wSL | numeric | Total runs above average on the slider. |
| wCT | numeric | Total runs above average on the cutter. |
| wCB | numeric | Total runs above average on the curveball. |
| wCH | numeric | Total runs above average on the changeup. |
| wSF | numeric | Total runs above average on the split-finger fastball. |
| wFB/C | numeric | Runs above average per 100 fastballs. |
| wSL/C | numeric | Runs above average per 100 sliders. |
| wCT/C | numeric | Runs above average per 100 cutters. |
| wCB/C | numeric | Runs above average per 100 curveballs. |
| wCH/C | numeric | Runs above average per 100 changeups. |
| wSF/C | numeric | Runs above average per 100 split-finger fastballs. |
| O-Swing% | numeric | Swing rate on pitches outside the zone. |
| Z-Swing% | numeric | Swing rate on pitches in the zone. |
| Swing% | numeric | Overall swing rate. |
| O-Contact% | numeric | Contact rate on pitches outside the zone. |
| Z-Contact% | numeric | Contact rate on pitches in the zone. |
| Contact% | numeric | Overall contact rate. |
| Zone% | numeric | Rate of pitches in the strike zone. |
| F-Strike% | numeric | First-pitch strike rate. |
| SwStr% | numeric | Swinging-strike rate. |
| Pull | numeric | Pulled batted balls. |
| Cent | numeric | Batted balls hit up the middle. |
| Oppo | numeric | Opposite-field batted balls. |
| Soft | numeric | Soft-hit batted balls. |
| Med | numeric | Medium-hit batted balls. |
| Hard | numeric | Hard-hit batted balls. |
| bipCount | numeric | Balls in play. |
| Pull% | numeric | Pulled batted-ball rate. |
| Cent% | numeric | Up-the-middle batted-ball rate. |
| Oppo% | numeric | Opposite-field batted-ball rate. |
| Soft% | numeric | Soft-contact rate. |
| Med% | numeric | Medium-contact rate. |
| Hard% | numeric | Hard-contact rate. |
| xwOBA | numeric | Expected weighted on-base average. |
| xAVG | numeric | Expected batting average. |
| xSLG | numeric | Expected slugging percentage. |
| pfxFA% | numeric | PITCHf/x four-seam fastball usage rate. |
| pfxFC% | numeric | PITCHf/x cutter usage rate. |
| pfxFS% | numeric | PITCHf/x splitter usage rate. |
| pfxFO% | numeric | PITCHf/x forkball usage rate. |
| pfxSI% | numeric | PITCHf/x sinker usage rate. |
| pfxSL% | numeric | PITCHf/x slider usage rate. |
| pfxCU% | numeric | PITCHf/x curveball usage rate. |
| pfxKC% | numeric | PITCHf/x knuckle curve usage rate. |
| pfxCH% | numeric | PITCHf/x changeup usage rate. |
| pfxSLO% | numeric | PITCHf/x slurve usage rate. |
| pfxST% | numeric | PITCHf/x sweeper usage rate. |
| pfxCUO% | numeric | PITCHf/x other curve usage rate. |
| pfxCV% | numeric | PITCHf/x curve usage rate. |
| pfxvFA | numeric | PITCHf/x average four-seam fastball velocity (mph). |
| pfxvFC | numeric | PITCHf/x average cutter velocity (mph). |
| pfxvFS | numeric | PITCHf/x average splitter velocity (mph). |
| pfxvFO | numeric | PITCHf/x average forkball velocity (mph). |
| pfxvSI | numeric | PITCHf/x average sinker velocity (mph). |
| pfxvSL | numeric | PITCHf/x average slider velocity (mph). |
| pfxvCU | numeric | PITCHf/x average curveball velocity (mph). |
| pfxvKC | numeric | PITCHf/x average knuckle curve velocity (mph). |
| pfxvCH | numeric | PITCHf/x average changeup velocity (mph). |
| pfxvSLO | numeric | PITCHf/x average slurve velocity (mph). |
| pfxvST | numeric | PITCHf/x average sweeper velocity (mph). |
| pfxvCUO | numeric | PITCHf/x average other curve velocity (mph). |
| pfxvCV | numeric | PITCHf/x average curve velocity (mph). |
| pfxFA-X | numeric | PITCHf/x horizontal movement of the four-seam fastball (inches). |
| pfxFC-X | numeric | PITCHf/x horizontal movement of the cutter (inches). |
| pfxFS-X | numeric | PITCHf/x horizontal movement of the splitter (inches). |
| pfxFO-X | numeric | PITCHf/x horizontal movement of the forkball (inches). |
| pfxSI-X | numeric | PITCHf/x horizontal movement of the sinker (inches). |
| pfxSL-X | numeric | PITCHf/x horizontal movement of the slider (inches). |
| pfxCU-X | numeric | PITCHf/x horizontal movement of the curveball (inches). |
| pfxKC-X | numeric | PITCHf/x horizontal movement of the knuckle curve (inches). |
| pfxCH-X | numeric | PITCHf/x horizontal movement of the changeup (inches). |
| pfxSLO-X | numeric | PITCHf/x horizontal movement of the slurve (inches). |
| pfxST-X | numeric | PITCHf/x horizontal movement of the sweeper (inches). |
| pfxCUO-X | numeric | PITCHf/x horizontal movement of the other curve (inches). |
| pfxCV-X | numeric | PITCHf/x horizontal movement of the curve (inches). |
| pfxFA-Z | numeric | PITCHf/x vertical movement of the four-seam fastball (inches). |
| pfxFC-Z | numeric | PITCHf/x vertical movement of the cutter (inches). |
| pfxFS-Z | numeric | PITCHf/x vertical movement of the splitter (inches). |
| pfxFO-Z | numeric | PITCHf/x vertical movement of the forkball (inches). |
| pfxSI-Z | numeric | PITCHf/x vertical movement of the sinker (inches). |
| pfxSL-Z | numeric | PITCHf/x vertical movement of the slider (inches). |
| pfxCU-Z | numeric | PITCHf/x vertical movement of the curveball (inches). |
| pfxKC-Z | numeric | PITCHf/x vertical movement of the knuckle curve (inches). |
| pfxCH-Z | numeric | PITCHf/x vertical movement of the changeup (inches). |
| pfxSLO-Z | numeric | PITCHf/x vertical movement of the slurve (inches). |
| pfxST-Z | numeric | PITCHf/x vertical movement of the sweeper (inches). |
| pfxCUO-Z | numeric | PITCHf/x vertical movement of the other curve (inches). |
| pfxCV-Z | numeric | PITCHf/x vertical movement of the curve (inches). |
| pfxwFA | numeric | PITCHf/x total runs above average on the four-seam fastball. |
| pfxwFC | numeric | PITCHf/x total runs above average on the cutter. |
| pfxwFS | numeric | PITCHf/x total runs above average on the splitter. |
| pfxwFO | numeric | PITCHf/x total runs above average on the forkball. |
| pfxwSI | numeric | PITCHf/x total runs above average on the sinker. |
| pfxwSL | numeric | PITCHf/x total runs above average on the slider. |
| pfxwCU | numeric | PITCHf/x total runs above average on the curveball. |
| pfxwKC | numeric | PITCHf/x total runs above average on the knuckle curve. |
| pfxwCH | numeric | PITCHf/x total runs above average on the changeup. |
| pfxwSLO | numeric | PITCHf/x total runs above average on the slurve. |
| pfxwST | numeric | PITCHf/x total runs above average on the sweeper. |
| pfxwCUO | numeric | PITCHf/x total runs above average on the other curve. |
| pfxwCV | numeric | PITCHf/x total runs above average on the curve. |
| pfxwFA/C | numeric | PITCHf/x runs above average per 100 four-seam fastballs. |
| pfxwFC/C | numeric | PITCHf/x runs above average per 100 cutters. |
| pfxwFS/C | numeric | PITCHf/x runs above average per 100 splitters. |
| pfxwFO/C | numeric | PITCHf/x runs above average per 100 forkballs. |
| pfxwSI/C | numeric | PITCHf/x runs above average per 100 sinkers. |
| pfxwSL/C | numeric | PITCHf/x runs above average per 100 sliders. |
| pfxwCU/C | numeric | PITCHf/x runs above average per 100 curveballs. |
| pfxwKC/C | numeric | PITCHf/x runs above average per 100 knuckle curves. |
| pfxwCH/C | numeric | PITCHf/x runs above average per 100 changeups. |
| pfxwSLO/C | numeric | PITCHf/x runs above average per 100 slurves. |
| pfxwST/C | numeric | PITCHf/x runs above average per 100 sweepers. |
| pfxwCUO/C | numeric | PITCHf/x runs above average per 100 other curves. |
| pfxwCV/C | numeric | PITCHf/x runs above average per 100 curves. |
| pfxaaFA | numeric | PITCHf/x average spin/active component on the four-seam fastball. |
| pfxaaFC | numeric | PITCHf/x average spin/active component on the cutter. |
| pfxaaFS | numeric | PITCHf/x average spin/active component on the splitter. |
| pfxaaFO | numeric | PITCHf/x average spin/active component on the forkball. |
| pfxaaSI | numeric | PITCHf/x average spin/active component on the sinker. |
| pfxaaSL | numeric | PITCHf/x average spin/active component on the slider. |
| pfxaaCU | numeric | PITCHf/x average spin/active component on the curveball. |
| pfxaaKC | numeric | PITCHf/x average spin/active component on the knuckle curve. |
| pfxaaCH | numeric | PITCHf/x average spin/active component on the changeup. |
| pfxaaSLO | numeric | PITCHf/x average spin/active component on the slurve. |
| pfxaaST | numeric | PITCHf/x average spin/active component on the sweeper. |
| pfxaaCUO | numeric | PITCHf/x average spin/active component on the other curve. |
| pfxaaCV | numeric | PITCHf/x average spin/active component on the curve. |
| pfxspFA | numeric | PITCHf/x average spin rate on the four-seam fastball (rpm). |
| pfxspFC | numeric | PITCHf/x average spin rate on the cutter (rpm). |
| pfxspFS | numeric | PITCHf/x average spin rate on the splitter (rpm). |
| pfxspFO | numeric | PITCHf/x average spin rate on the forkball (rpm). |
| pfxspSI | numeric | PITCHf/x average spin rate on the sinker (rpm). |
| pfxspSL | numeric | PITCHf/x average spin rate on the slider (rpm). |
| pfxspCU | numeric | PITCHf/x average spin rate on the curveball (rpm). |
| pfxspKC | numeric | PITCHf/x average spin rate on the knuckle curve (rpm). |
| pfxspCH | numeric | PITCHf/x average spin rate on the changeup (rpm). |
| pfxspSLO | numeric | PITCHf/x average spin rate on the slurve (rpm). |
| pfxspST | numeric | PITCHf/x average spin rate on the sweeper (rpm). |
| pfxspCUO | numeric | PITCHf/x average spin rate on the other curve (rpm). |
| pfxspCV | numeric | PITCHf/x average spin rate on the curve (rpm). |
| pfxO-Swing% | numeric | PITCHf/x Swing rate on pitches outside the zone. |
| pfxZ-Swing% | numeric | PITCHf/x Swing rate on pitches in the zone. |
| pfxSwing% | numeric | PITCHf/x Overall swing rate. |
| pfxO-Contact% | numeric | PITCHf/x Contact rate on pitches outside the zone. |
| pfxZ-Contact% | numeric | PITCHf/x Contact rate on pitches in the zone. |
| pfxContact% | numeric | PITCHf/x Overall contact rate. |
| pfxZone% | numeric | PITCHf/x Rate of pitches in the strike zone. |
| pfxPace | numeric | PITCHf/x Average seconds between pitches. |
| AvgBatSpeed | numeric | Average bat speed (mph). |
| FastSwing% | numeric | Rate of fast swings. |
| SwingLength | numeric | Average swing length (feet). |
| SquaredUpContact% | numeric | Squared-up rate per contact. |
| SquaredUpSwing% | numeric | Squared-up rate per swing. |
| BlastContact% | numeric | Blast rate per contact. |
| BlastSwing% | numeric | Blast rate per swing. |
| Swords | numeric | Swords (especially ugly swinging strikeouts). |
| CompetitiveSwings | numeric | Competitive swings. |
| Tilt | numeric | Average swing tilt (degrees). |
| AttackAngle | numeric | Average attack angle (degrees). |
| AttackDirection | numeric | Average attack direction (degrees). |
| IdealAttackAngle% | numeric | Rate of swings in the ideal attack-angle range. |
| DepthInBox | numeric | Average depth in the batter's box (inches). |
| DistanceOffPlate | numeric | Average distance off the plate (inches). |
| scH-Swing% | numeric | Statcast heart-zone swing rate. |
| scH-Contact% | numeric | Statcast heart-zone contact rate. |
| scH-Zone% | numeric | Statcast heart-zone zone rate. |
| scS-Swing% | numeric | Statcast shadow-zone swing rate. |
| scS-Contact% | numeric | Statcast shadow-zone contact rate. |
| scS-Zone% | numeric | Statcast shadow-zone zone rate. |
| scC-Swing% | numeric | Statcast chase-zone swing rate. |
| scC-Contact% | numeric | Statcast chase-zone contact rate. |
| scC-Zone% | numeric | Statcast chase-zone zone rate. |
| scW-Swing% | numeric | Statcast waste-zone swing rate. |
| scW-Contact% | numeric | Statcast waste-zone contact rate. |
| scW-Zone% | numeric | Statcast waste-zone zone rate. |
| scSI-Swing% | numeric | Statcast in-zone strike-zone swing rate. |
| scSI-Contact% | numeric | Statcast in-zone strike-zone contact rate. |
| scSI-Zone% | numeric | Statcast in-zone strike-zone zone rate. |
| scSO-Swing% | numeric | Statcast out-of-zone strike-zone swing rate. |
| scSO-Contact% | numeric | Statcast out-of-zone strike-zone contact rate. |
| scSO-Zone% | numeric | Statcast out-of-zone strike-zone zone rate. |
| scO-Swing% | numeric | Statcast out-of-zone-zone swing rate. |
| scO-Contact% | numeric | Statcast out-of-zone-zone contact rate. |
| scO-Zone% | numeric | Statcast out-of-zone-zone zone rate. |
| scZ-Swing% | numeric | Statcast in-zone-zone swing rate. |
| scZ-Contact% | numeric | Statcast in-zone-zone contact rate. |
| scZ-Zone% | numeric | Statcast in-zone-zone zone rate. |
| piCH% | numeric | PITCHInfo changeup usage rate. |
| piCS% | numeric | PITCHInfo slow curve usage rate. |
| piCU% | numeric | PITCHInfo curveball usage rate. |
| piFA% | numeric | PITCHInfo four-seam fastball usage rate. |
| piFC% | numeric | PITCHInfo cutter usage rate. |
| piFS% | numeric | PITCHInfo splitter usage rate. |
| piSI% | numeric | PITCHInfo sinker usage rate. |
| piSL% | numeric | PITCHInfo slider usage rate. |
| piXX% | numeric | PITCHInfo unknown/other pitch usage rate. |
| pivCH | numeric | PITCHInfo average changeup velocity (mph). |
| pivCS | numeric | PITCHInfo average slow curve velocity (mph). |
| pivCU | numeric | PITCHInfo average curveball velocity (mph). |
| pivFA | numeric | PITCHInfo average four-seam fastball velocity (mph). |
| pivFC | numeric | PITCHInfo average cutter velocity (mph). |
| pivFS | numeric | PITCHInfo average splitter velocity (mph). |
| pivSI | numeric | PITCHInfo average sinker velocity (mph). |
| pivSL | numeric | PITCHInfo average slider velocity (mph). |
| pivXX | numeric | PITCHInfo average unknown/other pitch velocity (mph). |
| piCH-X | numeric | PITCHInfo horizontal movement of the changeup (inches). |
| piCS-X | numeric | PITCHInfo horizontal movement of the slow curve (inches). |
| piCU-X | numeric | PITCHInfo horizontal movement of the curveball (inches). |
| piFA-X | numeric | PITCHInfo horizontal movement of the four-seam fastball (inches). |
| piFC-X | numeric | PITCHInfo horizontal movement of the cutter (inches). |
| piFS-X | numeric | PITCHInfo horizontal movement of the splitter (inches). |
| piSI-X | numeric | PITCHInfo horizontal movement of the sinker (inches). |
| piSL-X | numeric | PITCHInfo horizontal movement of the slider (inches). |
| piXX-X | numeric | PITCHInfo horizontal movement of the unknown/other pitch (inches). |
| piCH-Z | numeric | PITCHInfo vertical movement of the changeup (inches). |
| piCS-Z | numeric | PITCHInfo vertical movement of the slow curve (inches). |
| piCU-Z | numeric | PITCHInfo vertical movement of the curveball (inches). |
| piFA-Z | numeric | PITCHInfo vertical movement of the four-seam fastball (inches). |
| piFC-Z | numeric | PITCHInfo vertical movement of the cutter (inches). |
| piFS-Z | numeric | PITCHInfo vertical movement of the splitter (inches). |
| piSI-Z | numeric | PITCHInfo vertical movement of the sinker (inches). |
| piSL-Z | numeric | PITCHInfo vertical movement of the slider (inches). |
| piXX-Z | numeric | PITCHInfo vertical movement of the unknown/other pitch (inches). |
| piwCH | numeric | PITCHInfo total runs above average on the changeup. |
| piwCS | numeric | PITCHInfo total runs above average on the slow curve. |
| piwCU | numeric | PITCHInfo total runs above average on the curveball. |
| piwFA | numeric | PITCHInfo total runs above average on the four-seam fastball. |
| piwFC | numeric | PITCHInfo total runs above average on the cutter. |
| piwFS | numeric | PITCHInfo total runs above average on the splitter. |
| piwSI | numeric | PITCHInfo total runs above average on the sinker. |
| piwSL | numeric | PITCHInfo total runs above average on the slider. |
| piwXX | numeric | PITCHInfo total runs above average on the unknown/other pitch. |
| piwCH/C | numeric | PITCHInfo runs above average per 100 changeups. |
| piwCS/C | numeric | PITCHInfo runs above average per 100 slow curves. |
| piwCU/C | numeric | PITCHInfo runs above average per 100 curveballs. |
| piwFA/C | numeric | PITCHInfo runs above average per 100 four-seam fastballs. |
| piwFC/C | numeric | PITCHInfo runs above average per 100 cutters. |
| piwFS/C | numeric | PITCHInfo runs above average per 100 splitters. |
| piwSI/C | numeric | PITCHInfo runs above average per 100 sinkers. |
| piwSL/C | numeric | PITCHInfo runs above average per 100 sliders. |
| piwXX/C | numeric | PITCHInfo runs above average per 100 unknown/other pitchs. |
| piO-Swing% | numeric | PITCHInfo Swing rate on pitches outside the zone. |
| piZ-Swing% | numeric | PITCHInfo Swing rate on pitches in the zone. |
| piSwing% | numeric | PITCHInfo Overall swing rate. |
| piO-Contact% | numeric | PITCHInfo Contact rate on pitches outside the zone. |
| piZ-Contact% | numeric | PITCHInfo Contact rate on pitches in the zone. |
| piContact% | numeric | PITCHInfo Overall contact rate. |
| piZone% | numeric | PITCHInfo Rate of pitches in the strike zone. |
| Events | numeric | Batted-ball events with Statcast data. |
| EV | numeric | Average exit velocity (mph). |
| LA | numeric | Average launch angle (degrees). |
| Barrels | numeric | Barreled batted balls. |
| Barrel% | numeric | Barrel rate (per batted-ball event). |
| maxEV | numeric | Maximum exit velocity (mph). |
| HardHit | numeric | Hard-hit batted balls (95+ mph). |
| HardHit% | numeric | Hard-hit rate (95+ mph). |
| gamedate | character | Game date as parsed from the source feed. |
| dh | integer | Doubleheader game indicator (0 = single game). |

## Examples

``` r
# \donttest{
  try(fg_batter_game_logs(playerid = 19755, year = 2023))
#> ── MLB Batter Game Logs data from FanGraphs.com ───── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 04:39:11 UTC
#> # A tibble: 135 × 342
#>    PlayerName    playerid Date   Team  Opp   season   Age BatOrder Pos  
#>    <chr>            <int> <chr>  <chr> <chr>  <int> <int> <chr>    <chr>
#>  1 Shohei Ohtani    19755 2023-… LAA   @OAK    2023    28 2        DH   
#>  2 Shohei Ohtani    19755 2023-… LAA   @OAK    2023    28 2        DH   
#>  3 Shohei Ohtani    19755 2023-… LAA   @OAK    2023    28 3        DH   
#>  4 Shohei Ohtani    19755 2023-… LAA   @PHI    2023    28 3        DH   
#>  5 Shohei Ohtani    19755 2023-… LAA   @PHI    2023    28 2        DH   
#>  6 Shohei Ohtani    19755 2023-… LAA   @PHI    2023    28 2        DH   
#>  7 Shohei Ohtani    19755 2023-… LAA   @NYM    2023    28 2        DH   
#>  8 Shohei Ohtani    19755 2023-… LAA   @NYM    2023    28 2        DH   
#>  9 Shohei Ohtani    19755 2023-… LAA   @NYM    2023    28 2        DH   
#> 10 Shohei Ohtani    19755 2023-… LAA   CIN     2023    28 2        DH   
#> # ℹ 125 more rows
#> # ℹ 333 more variables: G <dbl>, AB <dbl>, PA <dbl>, H <dbl>,
#> #   `1B` <dbl>, `2B` <dbl>, `3B` <dbl>, HR <dbl>, R <dbl>, RBI <dbl>,
#> #   BB <dbl>, IBB <dbl>, SO <dbl>, HBP <dbl>, SF <dbl>, SH <dbl>,
#> #   GDP <dbl>, SB <dbl>, CS <dbl>, AVG <dbl>, GB <dbl>, FB <dbl>,
#> #   LD <dbl>, IFFB <dbl>, Pitches <dbl>, Balls <dbl>, Strikes <dbl>,
#> #   IFH <dbl>, BU <dbl>, BUH <dbl>, `BB%` <dbl>, `K%` <dbl>, …
# }
```
