# **Scrape Pitcher Game Logs from FanGraphs**

This function allows you to scrape game logs by year for a pitcher from
FanGraphs.com.

## Usage

``` r
fg_pitcher_game_logs(playerid, year)
```

## Arguments

- playerid:

  This is the playerid used by FanGraphs for a given player

- year:

  The season for which game logs should be returned (use the YYYY
  format)

## Value

A data frame of pitcher game logs, one row per game, with the following
columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| PlayerName | character | Player name. |
| playerid | integer | FanGraphs player ID. |
| Date | character | Calendar date of the game (YYYY-MM-DD). |
| Opp | character | Opponent team; leading @ indicates a road game. |
| teamid | integer | FanGraphs team ID. |
| season | integer | Season year. |
| Team | character | Team the player appeared for. |
| HomeAway | character | Home/away indicator for the game. |
| Age | integer | Player age during the season. |
| W | numeric | Wins. |
| L | numeric | Losses. |
| ERA | numeric | Earned run average. |
| G | numeric | Games (1 per row). |
| GS | numeric | Games started. |
| QS | numeric | Quality starts. |
| CG | numeric | Complete games. |
| ShO | numeric | Shutouts. |
| SV | numeric | Saves. |
| HLD | numeric | Holds. |
| BS | numeric | Blown saves. |
| IP | numeric | Innings pitched. |
| TBF | numeric | Total batters faced. |
| H | numeric | Hits. |
| R | numeric | Runs scored. |
| ER | numeric | Earned runs allowed. |
| HR | numeric | Home runs. |
| BB | numeric | Walks (bases on balls). |
| IBB | numeric | Intentional walks. |
| HBP | numeric | Times hit by pitch. |
| WP | numeric | Wild pitches. |
| BK | numeric | Balks. |
| SO | numeric | Strikeouts. |
| K/9 | numeric | Strikeouts per 9 innings. |
| BB/9 | numeric | Walks per 9 innings. |
| H/9 | numeric | Hits per 9 innings. |
| K/BB | numeric | Strikeout-to-walk ratio. |
| IFH% | numeric | Infield-hit rate. |
| BUH% | numeric | Bunt-hit rate. |
| GB | numeric | Ground balls. |
| FB | numeric | Fly balls. |
| LD | numeric | Line drives. |
| IFFB | numeric | Infield fly balls. |
| IFH | numeric | Infield hits. |
| BU | numeric | Bunts. |
| BUH | numeric | Bunt hits. |
| K% | numeric | Strikeout rate. |
| BB% | numeric | Walk rate. |
| K-BB% | numeric | Strikeout rate minus walk rate. |
| SIERA | numeric | Skill-interactive ERA. |
| HR/9 | numeric | Home runs per 9 innings. |
| AVG | numeric | Batting average. |
| WHIP | numeric | Walks plus hits per inning pitched. |
| BABIP | numeric | Batting average on balls in play. |
| LOB% | numeric | Left-on-base percentage (strand rate). |
| FIP | numeric | Fielding independent pitching. |
| E-F | numeric | ERA minus FIP. |
| xFIP | numeric | Expected fielding independent pitching. |
| ERA- | numeric | ERA scaled to league/park (100 = average). |
| FIP- | numeric | FIP scaled to league/park (100 = average). |
| xFIP- | numeric | xFIP scaled to league/park (100 = average). |
| GB/FB | numeric | Ground-ball to fly-ball ratio. |
| LD% | numeric | Line-drive rate. |
| GB% | numeric | Ground-ball rate. |
| FB% | numeric | Fly-ball rate. |
| IFFB% | numeric | Infield-fly-ball rate. |
| HR/FB | numeric | Home-run-per-fly-ball rate. |
| RS | numeric | Run support. |
| RS/9 | numeric | Run support per 9 innings. |
| Balls | numeric | Balls. |
| Strikes | numeric | Strikes. |
| Pitches | numeric | Pitches seen. |
| WPA | numeric | Win probability added. |
| -WPA | numeric | Negative win probability added. |
| +WPA | numeric | Positive win probability added. |
| RE24 | numeric | Run expectancy based on 24 base-out states. |
| REW | numeric | Run expectancy wins. |
| pLI | numeric | Average leverage index. |
| inLI | numeric | Average leverage index entering games. |
| gmLI | numeric | Average leverage index at game entry. |
| exLI | numeric | Average leverage index exiting games. |
| Pulls | numeric | Times pulled from games. |
| Games | numeric | Games appeared in. |
| WPA/LI | numeric | Context-neutral win probability added. |
| Clutch | numeric | Clutch performance score. |
| SD | numeric | Shutdowns. |
| MD | numeric | Meltdowns. |
| FB%1 | numeric | Fastball usage rate. |
| FBv | numeric | Average four-seam fastball velocity (mph). |
| SL% | numeric | slider usage rate. |
| SLv | numeric | Average slider velocity (mph). |
| CT% | numeric | cutter usage rate. |
| CTv | numeric | Average cutter velocity (mph). |
| CB% | numeric | curveball usage rate. |
| CBv | numeric | Average curveball velocity (mph). |
| SF% | numeric | split-finger fastball usage rate. |
| SFv | numeric | Average split-finger fastball velocity (mph). |
| XX% | numeric | unknown/other pitch usage rate. |
| wFB | numeric | Total runs above average on the fastball. |
| wSL | numeric | Total runs above average on the slider. |
| wCT | numeric | Total runs above average on the cutter. |
| wCB | numeric | Total runs above average on the curveball. |
| wSF | numeric | Total runs above average on the split-finger fastball. |
| wFB/C | numeric | Runs above average per 100 fastballs. |
| wSL/C | numeric | Runs above average per 100 sliders. |
| wCT/C | numeric | Runs above average per 100 cutters. |
| wCB/C | numeric | Runs above average per 100 curveballs. |
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
| tERA | numeric | True ERA (batted-ball based). |
| GSv2 | numeric | Game Score (version 2). |
| pb_o_CU | numeric | PitchingBot overall grade for the curveball. |
| pb_s_CU | numeric | PitchingBot stuff grade for the curveball. |
| pb_c_CU | numeric | PitchingBot command grade for the curveball. |
| pb_o_FF | numeric | PitchingBot overall grade for the four-seam fastball. |
| pb_s_FF | numeric | PitchingBot stuff grade for the four-seam fastball. |
| pb_c_FF | numeric | PitchingBot command grade for the four-seam fastball. |
| pb_o_SI | numeric | PitchingBot overall grade for the sinker. |
| pb_s_SI | numeric | PitchingBot stuff grade for the sinker. |
| pb_c_SI | numeric | PitchingBot command grade for the sinker. |
| pb_o_SL | numeric | PitchingBot overall grade for the slider. |
| pb_s_SL | numeric | PitchingBot stuff grade for the slider. |
| pb_c_SL | numeric | PitchingBot command grade for the slider. |
| pb_o_FC | numeric | PitchingBot overall grade for the cutter. |
| pb_s_FC | numeric | PitchingBot stuff grade for the cutter. |
| pb_c_FC | numeric | PitchingBot command grade for the cutter. |
| pb_o_FS | numeric | PitchingBot overall grade for the splitter. |
| pb_s_FS | numeric | PitchingBot stuff grade for the splitter. |
| pb_c_FS | numeric | PitchingBot command grade for the splitter. |
| pb_overall | numeric | PitchingBot overall grade. |
| pb_stuff | numeric | PitchingBot stuff grade. |
| pb_command | numeric | PitchingBot command grade. |
| pb_xRV100 | numeric | PitchingBot expected run value per 100 pitches. |
| pb_ERA | numeric | PitchingBot ERA estimate. |
| sp_s_CU | numeric | Stuff+ grade for the curveball (100 = average). |
| sp_l_CU | numeric | Location+ grade for the curveball (100 = average). |
| sp_p_CU | numeric | Pitching+ grade for the curveball (100 = average). |
| sp_s_FF | numeric | Stuff+ grade for the four-seam fastball (100 = average). |
| sp_l_FF | numeric | Location+ grade for the four-seam fastball (100 = average). |
| sp_p_FF | numeric | Pitching+ grade for the four-seam fastball (100 = average). |
| sp_s_SI | numeric | Stuff+ grade for the sinker (100 = average). |
| sp_l_SI | numeric | Location+ grade for the sinker (100 = average). |
| sp_p_SI | numeric | Pitching+ grade for the sinker (100 = average). |
| sp_s_SL | numeric | Stuff+ grade for the slider (100 = average). |
| sp_l_SL | numeric | Location+ grade for the slider (100 = average). |
| sp_p_SL | numeric | Pitching+ grade for the slider (100 = average). |
| sp_s_FC | numeric | Stuff+ grade for the cutter (100 = average). |
| sp_l_FC | numeric | Location+ grade for the cutter (100 = average). |
| sp_p_FC | numeric | Pitching+ grade for the cutter (100 = average). |
| sp_s_FS | numeric | Stuff+ grade for the splitter (100 = average). |
| sp_l_FS | numeric | Location+ grade for the splitter (100 = average). |
| sp_p_FS | numeric | Pitching+ grade for the splitter (100 = average). |
| sp_stuff | numeric | Overall Stuff+ grade (100 = average). |
| sp_location | numeric | Overall Location+ grade (100 = average). |
| sp_pitching | numeric | Overall Pitching+ grade (100 = average). |
| PPTV | numeric | Pitch-type and framing run value component. |
| CPTV | numeric | Pitch-type and framing run value component. |
| BPTV | numeric | Pitch-type and framing run value component. |
| DSV | numeric | Pitch-type and framing run value component. |
| DGV | numeric | Pitch-type and framing run value component. |
| BTV | numeric | Pitch-type and framing run value component. |
| rPPTV | numeric | Pitch-type and framing run value component. |
| rBPTV | numeric | Pitch-type and framing run value component. |
| EBV | numeric | Pitch-type and framing run value component. |
| ESV | numeric | Pitch-type and framing run value component. |
| rFTeamV | numeric | Pitch-type and framing run value component. |
| rBTeamV | numeric | Pitch-type and framing run value component. |
| rTV | numeric | Pitch-type and framing run value component. |
| xERA | numeric | Expected ERA. |
| pfxFA% | numeric | PITCHf/x four-seam fastball usage rate. |
| pfxFC% | numeric | PITCHf/x cutter usage rate. |
| pfxFS% | numeric | PITCHf/x splitter usage rate. |
| pfxSI% | numeric | PITCHf/x sinker usage rate. |
| pfxSL% | numeric | PITCHf/x slider usage rate. |
| pfxCU% | numeric | PITCHf/x curveball usage rate. |
| pfxSLO% | numeric | PITCHf/x slurve usage rate. |
| pfxST% | numeric | PITCHf/x sweeper usage rate. |
| pfxCUO% | numeric | PITCHf/x other curve usage rate. |
| pfxvFA | numeric | PITCHf/x average four-seam fastball velocity (mph). |
| pfxvFC | numeric | PITCHf/x average cutter velocity (mph). |
| pfxvFS | numeric | PITCHf/x average splitter velocity (mph). |
| pfxvSI | numeric | PITCHf/x average sinker velocity (mph). |
| pfxvSL | numeric | PITCHf/x average slider velocity (mph). |
| pfxvCU | numeric | PITCHf/x average curveball velocity (mph). |
| pfxvSLO | numeric | PITCHf/x average slurve velocity (mph). |
| pfxvST | numeric | PITCHf/x average sweeper velocity (mph). |
| pfxvCUO | numeric | PITCHf/x average other curve velocity (mph). |
| pfxFA-X | numeric | PITCHf/x horizontal movement of the four-seam fastball (inches). |
| pfxFC-X | numeric | PITCHf/x horizontal movement of the cutter (inches). |
| pfxFS-X | numeric | PITCHf/x horizontal movement of the splitter (inches). |
| pfxSI-X | numeric | PITCHf/x horizontal movement of the sinker (inches). |
| pfxSL-X | numeric | PITCHf/x horizontal movement of the slider (inches). |
| pfxCU-X | numeric | PITCHf/x horizontal movement of the curveball (inches). |
| pfxSLO-X | numeric | PITCHf/x horizontal movement of the slurve (inches). |
| pfxST-X | numeric | PITCHf/x horizontal movement of the sweeper (inches). |
| pfxCUO-X | numeric | PITCHf/x horizontal movement of the other curve (inches). |
| pfxFA-Z | numeric | PITCHf/x vertical movement of the four-seam fastball (inches). |
| pfxFC-Z | numeric | PITCHf/x vertical movement of the cutter (inches). |
| pfxFS-Z | numeric | PITCHf/x vertical movement of the splitter (inches). |
| pfxSI-Z | numeric | PITCHf/x vertical movement of the sinker (inches). |
| pfxSL-Z | numeric | PITCHf/x vertical movement of the slider (inches). |
| pfxCU-Z | numeric | PITCHf/x vertical movement of the curveball (inches). |
| pfxSLO-Z | numeric | PITCHf/x vertical movement of the slurve (inches). |
| pfxST-Z | numeric | PITCHf/x vertical movement of the sweeper (inches). |
| pfxCUO-Z | numeric | PITCHf/x vertical movement of the other curve (inches). |
| pfxwFA | numeric | PITCHf/x total runs above average on the four-seam fastball. |
| pfxwFC | numeric | PITCHf/x total runs above average on the cutter. |
| pfxwFS | numeric | PITCHf/x total runs above average on the splitter. |
| pfxwSI | numeric | PITCHf/x total runs above average on the sinker. |
| pfxwSL | numeric | PITCHf/x total runs above average on the slider. |
| pfxwCU | numeric | PITCHf/x total runs above average on the curveball. |
| pfxwSLO | numeric | PITCHf/x total runs above average on the slurve. |
| pfxwST | numeric | PITCHf/x total runs above average on the sweeper. |
| pfxwCUO | numeric | PITCHf/x total runs above average on the other curve. |
| pfxwFA/C | numeric | PITCHf/x runs above average per 100 four-seam fastballs. |
| pfxwFC/C | numeric | PITCHf/x runs above average per 100 cutters. |
| pfxwFS/C | numeric | PITCHf/x runs above average per 100 splitters. |
| pfxwSI/C | numeric | PITCHf/x runs above average per 100 sinkers. |
| pfxwSL/C | numeric | PITCHf/x runs above average per 100 sliders. |
| pfxwCU/C | numeric | PITCHf/x runs above average per 100 curveballs. |
| pfxwSLO/C | numeric | PITCHf/x runs above average per 100 slurves. |
| pfxwST/C | numeric | PITCHf/x runs above average per 100 sweepers. |
| pfxwCUO/C | numeric | PITCHf/x runs above average per 100 other curves. |
| pfxaaFA | numeric | PITCHf/x average spin/active component on the four-seam fastball. |
| pfxaaFC | numeric | PITCHf/x average spin/active component on the cutter. |
| pfxaaFS | numeric | PITCHf/x average spin/active component on the splitter. |
| pfxaaSI | numeric | PITCHf/x average spin/active component on the sinker. |
| pfxaaSL | numeric | PITCHf/x average spin/active component on the slider. |
| pfxaaCU | numeric | PITCHf/x average spin/active component on the curveball. |
| pfxaaSLO | numeric | PITCHf/x average spin/active component on the slurve. |
| pfxaaST | numeric | PITCHf/x average spin/active component on the sweeper. |
| pfxaaCUO | numeric | PITCHf/x average spin/active component on the other curve. |
| pfxspFA | numeric | PITCHf/x average spin rate on the four-seam fastball (rpm). |
| pfxspFC | numeric | PITCHf/x average spin rate on the cutter (rpm). |
| pfxspFS | numeric | PITCHf/x average spin rate on the splitter (rpm). |
| pfxspSI | numeric | PITCHf/x average spin rate on the sinker (rpm). |
| pfxspSL | numeric | PITCHf/x average spin rate on the slider (rpm). |
| pfxspCU | numeric | PITCHf/x average spin rate on the curveball (rpm). |
| pfxspSLO | numeric | PITCHf/x average spin rate on the slurve (rpm). |
| pfxspST | numeric | PITCHf/x average spin rate on the sweeper (rpm). |
| pfxspCUO | numeric | PITCHf/x average spin rate on the other curve (rpm). |
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
| piCU% | numeric | PITCHInfo curveball usage rate. |
| piFA% | numeric | PITCHInfo four-seam fastball usage rate. |
| piFC% | numeric | PITCHInfo cutter usage rate. |
| piFS% | numeric | PITCHInfo splitter usage rate. |
| piSI% | numeric | PITCHInfo sinker usage rate. |
| piSL% | numeric | PITCHInfo slider usage rate. |
| piXX% | numeric | PITCHInfo unknown/other pitch usage rate. |
| pivCU | numeric | PITCHInfo average curveball velocity (mph). |
| pivFA | numeric | PITCHInfo average four-seam fastball velocity (mph). |
| pivFC | numeric | PITCHInfo average cutter velocity (mph). |
| pivFS | numeric | PITCHInfo average splitter velocity (mph). |
| pivSI | numeric | PITCHInfo average sinker velocity (mph). |
| pivSL | numeric | PITCHInfo average slider velocity (mph). |
| pivXX | numeric | PITCHInfo average unknown/other pitch velocity (mph). |
| piCU-X | numeric | PITCHInfo horizontal movement of the curveball (inches). |
| piFA-X | numeric | PITCHInfo horizontal movement of the four-seam fastball (inches). |
| piFC-X | numeric | PITCHInfo horizontal movement of the cutter (inches). |
| piFS-X | numeric | PITCHInfo horizontal movement of the splitter (inches). |
| piSI-X | numeric | PITCHInfo horizontal movement of the sinker (inches). |
| piSL-X | numeric | PITCHInfo horizontal movement of the slider (inches). |
| piXX-X | numeric | PITCHInfo horizontal movement of the unknown/other pitch (inches). |
| piCU-Z | numeric | PITCHInfo vertical movement of the curveball (inches). |
| piFA-Z | numeric | PITCHInfo vertical movement of the four-seam fastball (inches). |
| piFC-Z | numeric | PITCHInfo vertical movement of the cutter (inches). |
| piFS-Z | numeric | PITCHInfo vertical movement of the splitter (inches). |
| piSI-Z | numeric | PITCHInfo vertical movement of the sinker (inches). |
| piSL-Z | numeric | PITCHInfo vertical movement of the slider (inches). |
| piXX-Z | numeric | PITCHInfo vertical movement of the unknown/other pitch (inches). |
| piwCU | numeric | PITCHInfo total runs above average on the curveball. |
| piwFA | numeric | PITCHInfo total runs above average on the four-seam fastball. |
| piwFC | numeric | PITCHInfo total runs above average on the cutter. |
| piwFS | numeric | PITCHInfo total runs above average on the splitter. |
| piwSI | numeric | PITCHInfo total runs above average on the sinker. |
| piwSL | numeric | PITCHInfo total runs above average on the slider. |
| piwXX | numeric | PITCHInfo total runs above average on the unknown/other pitch. |
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
  try(fg_pitcher_game_logs(playerid = "19755", year = 2023))
#> ── MLB Pitcher Game Log data from FanGraphs.com ───── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:24:06 UTC
#> # A tibble: 23 × 367
#>    PlayerName    playerid Date  Opp   teamid season Team  HomeAway   Age
#>    <chr>            <int> <chr> <chr>  <int>  <int> <chr> <chr>    <int>
#>  1 Shohei Ohtani    19755 2023… CIN        1   2023 LAA   H           28
#>  2 Shohei Ohtani    19755 2023… SFG        1   2023 LAA   H           28
#>  3 Shohei Ohtani    19755 2023… SEA        1   2023 LAA   H           28
#>  4 Shohei Ohtani    19755 2023… @DET       1   2023 LAA   A           28
#>  5 Shohei Ohtani    19755 2023… PIT        1   2023 LAA   H           28
#>  6 Shohei Ohtani    19755 2023… HOU        1   2023 LAA   H           28
#>  7 Shohei Ohtani    19755 2023… @SDP       1   2023 LAA   A           28
#>  8 Shohei Ohtani    19755 2023… CHW        1   2023 LAA   H           28
#>  9 Shohei Ohtani    19755 2023… LAD        1   2023 LAA   H           28
#> 10 Shohei Ohtani    19755 2023… @TEX       1   2023 LAA   A           28
#> # ℹ 13 more rows
#> # ℹ 358 more variables: W <dbl>, L <dbl>, ERA <dbl>, G <dbl>, GS <dbl>,
#> #   QS <dbl>, CG <dbl>, ShO <dbl>, SV <dbl>, HLD <dbl>, BS <dbl>,
#> #   IP <dbl>, TBF <dbl>, H <dbl>, R <dbl>, ER <dbl>, HR <dbl>,
#> #   BB <dbl>, IBB <dbl>, HBP <dbl>, WP <dbl>, BK <dbl>, SO <dbl>,
#> #   `K/9` <dbl>, `BB/9` <dbl>, `H/9` <dbl>, `K/BB` <dbl>, `IFH%` <dbl>,
#> #   `BUH%` <dbl>, GB <dbl>, FB <dbl>, LD <dbl>, IFFB <dbl>, …
# }
```
