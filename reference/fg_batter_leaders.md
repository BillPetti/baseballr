# **Scrape Batter Leaderboards from FanGraphs**

This function allows you to scrape all leaderboard statistics (basic and
advanced) from FanGraphs.com.

## Usage

``` r
fg_batter_leaders(
  age = "",
  pos = "all",
  stats = "bat",
  lg = "all",
  qual = "0",
  startseason = "2023",
  endseason = "2023",
  startdate = "",
  enddate = "",
  month = "0",
  hand = "",
  team = "0",
  pageitems = "10000",
  pagenum = "1",
  ind = "0",
  rost = "0",
  players = "",
  type = "8",
  postseason = "",
  sortdir = "default",
  sortstat = "WAR"
)
```

## Arguments

- age:

  (integer) Age of players

- pos:

  (character) Position of players, defaults to "all". To exclude
  pitchers, use "np".

- stats:

  (character) Statistic to return. Defaults to "bat".

- lg:

  (character) League to return. Defaults to "all". Options are "al",
  "nl", or "all".

- qual:

  (character) Whether you want only batters/pitchers that qualified in a
  given season, or the minimum number of plate appearances for
  inclusion. If you only want qualified hitters, use qual. If a minimum
  number of plate appearaces/innings pitched, use the number desired.
  Defaults to "y".

- startseason:

  (character) Season for which you want to scrape the data.

- endseason:

  (character) Last season for which you want data.

- startdate:

  (character) Start date for which you want data.

- enddate:

  (character) End date for which you want data.

- month:

  (character) Month for which you want data.

- hand:

  (character) Handedness of batter. Options are "L", "R", or "B". Empty
  string returns all.

- team:

  (character) Teams for which you want data, comma separated.

- pageitems:

  (character) Number of items per page.

- pagenum:

  (character) Page number.

- ind:

  (character) Whether or not to break the seasons out individual, or
  roll them up together. 1 = split seasons, 0 = aggregate seasons.

- rost:

  (character) Whether or not to include players on the roster. 1 =
  include, 0 = exclude.

- players:

  (character) Whether or not to include players on the roster. 1 =
  include only active roster players, 0 = exclude.

- type:

  (character) Defaults to 8, which is the standard leaderboard. The
  values for the leaderboards appear to go to from type = 0 to 48+,
  which correspond to links on the leaderboard page.

- postseason:

  (logical) Whether or not to include postseason data. TRUE = include
  postseason, FALSE = exclude postseason.

- sortdir:

  (character) Sort direction. Options are "asc" or "desc" or "default".

- sortstat:

  (character) Sort by stat. Default is "WAR".

## Value

A data frame of batter data.

|  |  |  |
|----|----|----|
| col_name | types | description |
| Season | integer | Season (YYYY). |
| team_name | character | Team name. |
| Bats | character | Batting handedness (L/R/S). |
| xMLBAMID | integer | MLBAM player ID. |
| PlayerNameRoute | character | Player name URL slug used by FanGraphs. |
| PlayerName | character | Player name. |
| playerid | integer | FanGraphs player ID. |
| Age | integer | Player age. |
| AgeRng | character | Player age range across the span. |
| SeasonMin | integer | First season in the queried span. |
| SeasonMax | integer | Last season in the queried span. |
| G | integer | Games played. |
| AB | integer | At-bats. |
| PA | integer | Plate appearances. |
| H | integer | Hits. |
| 1B | integer | Singles. |
| 2B | integer | Doubles. |
| 3B | integer | Triples. |
| HR | integer | Home runs. |
| R | integer | Runs scored. |
| RBI | integer | Runs batted in. |
| BB | integer | Walks (bases on balls). |
| IBB | integer | Intentional walks. |
| SO | integer | Strikeouts. |
| HBP | integer | Hit by pitch. |
| SF | integer | Sacrifice flies. |
| SH | integer | Sacrifice hits (bunts). |
| GDP | integer | Grounded into double plays. |
| SB | integer | Stolen bases. |
| CS | integer | Caught stealing. |
| AVG | numeric | Batting average (or opponent average for pitchers). |
| GB | integer | Ground balls. |
| FB | integer | Fly balls. |
| LD | integer | Line drives. |
| IFFB | integer | Infield fly balls. |
| Pitches | integer | Total pitches seen or thrown. |
| Balls | integer | Total balls. |
| Strikes | integer | Total strikes. |
| IFH | integer | Infield hits. |
| BU | integer | Bunts. |
| BUH | integer | Bunt hits. |
| BB_pct | numeric | Walk rate. |
| K_pct | numeric | Strikeout rate. |
| BB_K | numeric | Walk-to-strikeout ratio. |
| OBP | numeric | On-base percentage. |
| SLG | numeric | Slugging percentage. |
| OPS | numeric | On-base plus slugging. |
| ISO | numeric | Isolated power. |
| BABIP | numeric | Batting average on balls in play. |
| GB_FB | numeric | Ground-ball to fly-ball ratio. |
| LD_pct | numeric | Line-drive percentage. |
| GB_pct | numeric | Ground-ball percentage. |
| FB_pct | numeric | Fly-ball percentage. |
| IFFB_pct | numeric | Infield-fly-ball percentage. |
| HR_FB | numeric | Home-run-per-fly-ball rate. |
| IFH_pct | numeric | Infield-hit percentage. |
| BUH_pct | numeric | Bunt-hit percentage. |
| TTO_pct | numeric | Three-true-outcomes percentage (BB, K, HR). |
| wOBA | numeric | Weighted On-Base Average. |
| wRAA | numeric | Weighted Runs Above Average. |
| wRC | numeric | Weighted Runs Created. |
| Batting | numeric | Batting runs above average. |
| Fielding | numeric | Fielding runs above average. |
| Replacement | numeric | Replacement-level runs. |
| Positional | numeric | Positional adjustment runs. |
| wLeague | numeric | League adjustment runs. |
| Defense | numeric | Total defensive value (runs above average). |
| Offense | numeric | Total offensive value (runs above average). |
| RAR | numeric | Runs Above Replacement. |
| WAR | numeric | Wins Above Replacement. |
| WAROld | numeric | Wins Above Replacement (legacy formulation). |
| Dollars | numeric | Estimated market value of production. |
| BaseRunning | numeric | Base-running runs above average. |
| Spd | numeric | Speed Score. |
| wRC_plus | numeric | Weighted Runs Created Plus (park/league-adjusted, 100 = average). |
| wBsR | numeric | Weighted Base Running runs. |
| WPA | numeric | Win Probability Added. |
| WPA_minus | numeric | Negative Win Probability Added. |
| WPA_plus | numeric | Positive Win Probability Added. |
| RE24 | numeric | Run Expectancy based on the 24 base-out states. |
| REW | numeric | Run Expectancy Wins. |
| pLI | numeric | Average Leverage Index. |
| PH | integer | Pinch-hit appearances. |
| WPA_LI | numeric | Situational Win Probability Added (WPA divided by Leverage Index). |
| Clutch | numeric | Clutch performance relative to context-neutral performance. |
| FB_pct1 | numeric | Overall fastball percentage (pitch usage). |
| FBv | numeric | Average fastball velocity. |
| SL_pct | numeric | Slider percentage (pitch usage). |
| SLv | numeric | Average slider velocity. |
| CT_pct | numeric | Cutter percentage (pitch usage). |
| CTv | numeric | Average cutter velocity. |
| CB_pct | numeric | Curveball percentage (pitch usage). |
| CBv | numeric | Average curveball velocity. |
| CH_pct | numeric | Changeup percentage (pitch usage). |
| CHv | numeric | Average changeup velocity. |
| SF_pct | numeric | Splitter percentage (pitch usage). |
| SFv | numeric | Average splitter velocity. |
| XX_pct | numeric | Unidentified-pitch percentage (pitch usage). |
| wFB | numeric | Fastball pitch-type linear weight runs. |
| wSL | numeric | Slider pitch-type linear weight runs. |
| wCT | numeric | Cutter pitch-type linear weight runs. |
| wCB | numeric | Curveball pitch-type linear weight runs. |
| wCH | numeric | Changeup pitch-type linear weight runs. |
| wSF | numeric | Splitter pitch-type linear weight runs. |
| wFB_C | numeric | Fastball linear weight runs per 100 pitches. |
| wSL_C | numeric | Slider linear weight runs per 100 pitches. |
| wCT_C | numeric | Cutter linear weight runs per 100 pitches. |
| wCB_C | numeric | Curveball linear weight runs per 100 pitches. |
| wCH_C | numeric | Changeup linear weight runs per 100 pitches. |
| wSF_C | numeric | Splitter linear weight runs per 100 pitches. |
| O-Swing_pct | numeric | Swing percentage on pitches outside the zone (chase rate). |
| Z-Swing_pct | numeric | Swing percentage on pitches inside the zone. |
| Swing_pct | numeric | Overall swing percentage. |
| O-Contact_pct | numeric | Contact percentage on pitches outside the zone. |
| Z-Contact_pct | numeric | Contact percentage on pitches inside the zone. |
| Contact_pct | numeric | Overall contact percentage. |
| Zone_pct | numeric | Percentage of pitches in the strike zone. |
| F-Strike_pct | numeric | First-pitch strike percentage. |
| SwStr_pct | numeric | Swinging-strike percentage. |
| CStr_pct | numeric | Called-strike percentage. |
| C+SwStr_pct | numeric | Combined called- plus swinging-strike percentage. |
| Pull | integer | Balls in play hit to the pull field. |
| Cent | integer | Balls in play hit up the middle. |
| Oppo | integer | Balls in play hit to the opposite field. |
| Soft | integer | Soft-contact balls in play. |
| Med | integer | Medium-contact balls in play. |
| Hard | integer | Hard-contact balls in play. |
| bipCount | integer | Count of balls in play with contact-quality data. |
| Pull_pct | numeric | Pull-field percentage. |
| Cent_pct | numeric | Up-the-middle percentage. |
| Oppo_pct | numeric | Opposite-field percentage. |
| Soft_pct | numeric | Soft-contact percentage. |
| Med_pct | numeric | Medium-contact percentage. |
| Hard_pct | numeric | Hard-contact percentage. |
| UBR | numeric | Ultimate Base Running runs. |
| GDPRuns | numeric | Runs from grounded-into-double-play avoidance. |
| AVG+ | numeric | Park/league-adjusted batting average (100 = average). |
| BB_pct+ | numeric | Park/league-adjusted walk rate (100 = average). |
| K_pct+ | numeric | Park/league-adjusted strikeout rate (100 = average). |
| OBP+ | numeric | Park/league-adjusted on-base percentage (100 = average). |
| SLG+ | numeric | Park/league-adjusted slugging percentage (100 = average). |
| ISO+ | numeric | Park/league-adjusted isolated power (100 = average). |
| BABIP+ | numeric | Park/league-adjusted BABIP (100 = average). |
| LD_pct+ | numeric | Park/league-adjusted line-drive rate (100 = average). |
| GB_pct+ | numeric | Park/league-adjusted ground-ball rate (100 = average). |
| FB_pct+ | numeric | Park/league-adjusted fly-ball rate (100 = average). |
| HRFB_pct+ | numeric | Park/league-adjusted home-run-per-fly-ball rate (100 = average). |
| Pull_pct+ | numeric | Park/league-adjusted pull rate (100 = average). |
| Cent_pct+ | numeric | Park/league-adjusted up-the-middle rate (100 = average). |
| Oppo_pct+ | numeric | Park/league-adjusted opposite-field rate (100 = average). |
| Soft_pct+ | numeric | Park/league-adjusted soft-contact rate (100 = average). |
| Med_pct+ | numeric | Park/league-adjusted medium-contact rate (100 = average). |
| Hard_pct+ | numeric | Park/league-adjusted hard-contact rate (100 = average). |
| xwOBA | numeric | Expected Weighted On-Base Average (Statcast). |
| xAVG | numeric | Expected batting average (Statcast). |
| xSLG | numeric | Expected slugging percentage (Statcast). |
| XBR | numeric | Extra bases taken on the bases, runs above average. |
| PPTV | integer | Pitch-type pitch values (raw component). |
| CPTV | integer | Catcher pitch-type value (raw component). |
| BPTV | integer | Batter pitch-type value (raw component). |
| DSV | integer | Defensive stuff value (raw component). |
| DGV | integer | Defensive game value (raw component). |
| BTV | integer | Base-state team value (raw component). |
| rPPTV | numeric | Regressed pitch-type pitch values. |
| rBPTV | numeric | Regressed batter pitch-type value. |
| EBV | integer | Earned-base value (raw component). |
| ESV | integer | Earned-strike value (raw component). |
| rFTeamV | numeric | Regressed fielding team value. |
| rBTeamV | numeric | Regressed base-running team value. |
| rTV | numeric | Regressed total team value. |
| pfx_FA_pct | numeric | PITCHf/x four-seam fastballs usage percentage. |
| pfx_FC_pct | numeric | PITCHf/x cutters usage percentage. |
| pfx_FS_pct | numeric | PITCHf/x splitters usage percentage. |
| pfx_FO_pct | numeric | PITCHf/x forkballs/pitch-outs usage percentage. |
| pfx_SI_pct | numeric | PITCHf/x sinkers usage percentage. |
| pfx_SL_pct | numeric | PITCHf/x sliders usage percentage. |
| pfx_CU_pct | numeric | PITCHf/x curveballs usage percentage. |
| pfx_KC_pct | numeric | PITCHf/x knuckle-curves usage percentage. |
| pfx_EP_pct | numeric | PITCHf/x eephus pitches usage percentage. |
| pfx_CH_pct | numeric | PITCHf/x changeups usage percentage. |
| pfx_SC_pct | numeric | PITCHf/x screwballs usage percentage. |
| pfx_SLO_pct | numeric | PITCHf/x slurves usage percentage. |
| pfx_ST_pct | numeric | PITCHf/x sweepers usage percentage. |
| pfx_CUO_pct | numeric | PITCHf/x slurves usage percentage. |
| pfx_CV_pct | numeric | PITCHf/x slow curves usage percentage. |
| pfx_vFA | numeric | PITCHf/x average four-seam fastballs velocity. |
| pfx_vFC | numeric | PITCHf/x average cutters velocity. |
| pfx_vFS | numeric | PITCHf/x average splitters velocity. |
| pfx_vFO | numeric | PITCHf/x average forkballs/pitch-outs velocity. |
| pfx_vSI | numeric | PITCHf/x average sinkers velocity. |
| pfx_vSL | numeric | PITCHf/x average sliders velocity. |
| pfx_vCU | numeric | PITCHf/x average curveballs velocity. |
| pfx_vKC | numeric | PITCHf/x average knuckle-curves velocity. |
| pfx_vEP | numeric | PITCHf/x average eephus pitches velocity. |
| pfx_vCH | numeric | PITCHf/x average changeups velocity. |
| pfx_vSC | numeric | PITCHf/x average screwballs velocity. |
| pfx_vSLO | numeric | PITCHf/x average slurves velocity. |
| pfx_vST | numeric | PITCHf/x average sweepers velocity. |
| pfx_vCUO | numeric | PITCHf/x average slurves velocity. |
| pfx_vCV | numeric | PITCHf/x average slow curves velocity. |
| pfx_FA-X | numeric | PITCHf/x average horizontal movement on four-seam fastballs. |
| pfx_FC-X | numeric | PITCHf/x average horizontal movement on cutters. |
| pfx_FS-X | numeric | PITCHf/x average horizontal movement on splitters. |
| pfx_FO-X | numeric | PITCHf/x average horizontal movement on forkballs/pitch-outs. |
| pfx_SI-X | numeric | PITCHf/x average horizontal movement on sinkers. |
| pfx_SL-X | numeric | PITCHf/x average horizontal movement on sliders. |
| pfx_CU-X | numeric | PITCHf/x average horizontal movement on curveballs. |
| pfx_KC-X | numeric | PITCHf/x average horizontal movement on knuckle-curves. |
| pfx_EP-X | numeric | PITCHf/x average horizontal movement on eephus pitches. |
| pfx_CH-X | numeric | PITCHf/x average horizontal movement on changeups. |
| pfx_SC-X | numeric | PITCHf/x average horizontal movement on screwballs. |
| pfx_SLO-X | numeric | PITCHf/x average horizontal movement on slurves. |
| pfx_ST-X | numeric | PITCHf/x average horizontal movement on sweepers. |
| pfx_CUO-X | numeric | PITCHf/x average horizontal movement on slurves. |
| pfx_CV-X | numeric | PITCHf/x average horizontal movement on slow curves. |
| pfx_FA-Z | numeric | PITCHf/x average vertical movement on four-seam fastballs. |
| pfx_FC-Z | numeric | PITCHf/x average vertical movement on cutters. |
| pfx_FS-Z | numeric | PITCHf/x average vertical movement on splitters. |
| pfx_FO-Z | numeric | PITCHf/x average vertical movement on forkballs/pitch-outs. |
| pfx_SI-Z | numeric | PITCHf/x average vertical movement on sinkers. |
| pfx_SL-Z | numeric | PITCHf/x average vertical movement on sliders. |
| pfx_CU-Z | numeric | PITCHf/x average vertical movement on curveballs. |
| pfx_KC-Z | numeric | PITCHf/x average vertical movement on knuckle-curves. |
| pfx_EP-Z | numeric | PITCHf/x average vertical movement on eephus pitches. |
| pfx_CH-Z | numeric | PITCHf/x average vertical movement on changeups. |
| pfx_SC-Z | numeric | PITCHf/x average vertical movement on screwballs. |
| pfx_SLO-Z | numeric | PITCHf/x average vertical movement on slurves. |
| pfx_ST-Z | numeric | PITCHf/x average vertical movement on sweepers. |
| pfx_CUO-Z | numeric | PITCHf/x average vertical movement on slurves. |
| pfx_CV-Z | numeric | PITCHf/x average vertical movement on slow curves. |
| pfx_wFA | numeric | PITCHf/x four-seam fastballs linear weight runs. |
| pfx_wFC | numeric | PITCHf/x cutters linear weight runs. |
| pfx_wFS | numeric | PITCHf/x splitters linear weight runs. |
| pfx_wFO | numeric | PITCHf/x forkballs/pitch-outs linear weight runs. |
| pfx_wSI | numeric | PITCHf/x sinkers linear weight runs. |
| pfx_wSL | numeric | PITCHf/x sliders linear weight runs. |
| pfx_wCU | numeric | PITCHf/x curveballs linear weight runs. |
| pfx_wKC | numeric | PITCHf/x knuckle-curves linear weight runs. |
| pfx_wEP | numeric | PITCHf/x eephus pitches linear weight runs. |
| pfx_wCH | numeric | PITCHf/x changeups linear weight runs. |
| pfx_wSC | numeric | PITCHf/x screwballs linear weight runs. |
| pfx_wSLO | numeric | PITCHf/x slurves linear weight runs. |
| pfx_wST | numeric | PITCHf/x sweepers linear weight runs. |
| pfx_wCUO | numeric | PITCHf/x slurves linear weight runs. |
| pfx_wCV | numeric | PITCHf/x slow curves linear weight runs. |
| pfx_wFA_C | numeric | PITCHf/x four-seam fastballs linear weight runs per 100 pitches. |
| pfx_wFC_C | numeric | PITCHf/x cutters linear weight runs per 100 pitches. |
| pfx_wFS_C | numeric | PITCHf/x splitters linear weight runs per 100 pitches. |
| pfx_wFO_C | numeric | PITCHf/x forkballs/pitch-outs linear weight runs per 100 pitches. |
| pfx_wSI_C | numeric | PITCHf/x sinkers linear weight runs per 100 pitches. |
| pfx_wSL_C | numeric | PITCHf/x sliders linear weight runs per 100 pitches. |
| pfx_wCU_C | numeric | PITCHf/x curveballs linear weight runs per 100 pitches. |
| pfx_wKC_C | numeric | PITCHf/x knuckle-curves linear weight runs per 100 pitches. |
| pfx_wEP_C | numeric | PITCHf/x eephus pitches linear weight runs per 100 pitches. |
| pfx_wCH_C | numeric | PITCHf/x changeups linear weight runs per 100 pitches. |
| pfx_wSC_C | numeric | PITCHf/x screwballs linear weight runs per 100 pitches. |
| pfx_wSLO_C | numeric | PITCHf/x slurves linear weight runs per 100 pitches. |
| pfx_wST_C | numeric | PITCHf/x sweepers linear weight runs per 100 pitches. |
| pfx_wCUO_C | numeric | PITCHf/x slurves linear weight runs per 100 pitches. |
| pfx_wCV_C | numeric | PITCHf/x slow curves linear weight runs per 100 pitches. |
| pfx_aaFA | numeric | PITCHf/x average arsenal value on four-seam fastballs. |
| pfx_aaFC | numeric | PITCHf/x average arsenal value on cutters. |
| pfx_aaFS | numeric | PITCHf/x average arsenal value on splitters. |
| pfx_aaFO | numeric | PITCHf/x average arsenal value on forkballs/pitch-outs. |
| pfx_aaSI | numeric | PITCHf/x average arsenal value on sinkers. |
| pfx_aaSL | numeric | PITCHf/x average arsenal value on sliders. |
| pfx_aaCU | numeric | PITCHf/x average arsenal value on curveballs. |
| pfx_aaKC | numeric | PITCHf/x average arsenal value on knuckle-curves. |
| pfx_aaEP | numeric | PITCHf/x average arsenal value on eephus pitches. |
| pfx_aaCH | numeric | PITCHf/x average arsenal value on changeups. |
| pfx_aaSC | numeric | PITCHf/x average arsenal value on screwballs. |
| pfx_aaSLO | numeric | PITCHf/x average arsenal value on slurves. |
| pfx_aaST | numeric | PITCHf/x average arsenal value on sweepers. |
| pfx_aaCUO | numeric | PITCHf/x average arsenal value on slurves. |
| pfx_aaCV | numeric | PITCHf/x average arsenal value on slow curves. |
| pfx_spFA | numeric | PITCHf/x Stuff+ value on four-seam fastballs. |
| pfx_spFC | numeric | PITCHf/x Stuff+ value on cutters. |
| pfx_spFS | numeric | PITCHf/x Stuff+ value on splitters. |
| pfx_spFO | numeric | PITCHf/x Stuff+ value on forkballs/pitch-outs. |
| pfx_spSI | numeric | PITCHf/x Stuff+ value on sinkers. |
| pfx_spSL | numeric | PITCHf/x Stuff+ value on sliders. |
| pfx_spCU | numeric | PITCHf/x Stuff+ value on curveballs. |
| pfx_spKC | numeric | PITCHf/x Stuff+ value on knuckle-curves. |
| pfx_spEP | numeric | PITCHf/x Stuff+ value on eephus pitches. |
| pfx_spCH | numeric | PITCHf/x Stuff+ value on changeups. |
| pfx_spSC | numeric | PITCHf/x Stuff+ value on screwballs. |
| pfx_spSLO | numeric | PITCHf/x Stuff+ value on slurves. |
| pfx_spST | numeric | PITCHf/x Stuff+ value on sweepers. |
| pfx_spCUO | numeric | PITCHf/x Stuff+ value on slurves. |
| pfx_spCV | numeric | PITCHf/x Stuff+ value on slow curves. |
| pfx_O-Swing_pct | numeric | PITCHf/x swing percentage on pitches outside the zone. |
| pfx_Z-Swing_pct | numeric | PITCHf/x swing percentage on pitches inside the zone. |
| pfx_Swing_pct | numeric | PITCHf/x swing percentage. |
| pfx_O-Contact_pct | numeric | PITCHf/x contact percentage on pitches outside the zone. |
| pfx_Z-Contact_pct | numeric | PITCHf/x contact percentage on pitches inside the zone. |
| pfx_Contact_pct | numeric | PITCHf/x contact percentage. |
| pfx_Zone_pct | numeric | PITCHf/x percentage of pitches in the strike zone. |
| pfx_Pace | numeric | PITCHf/x pace (seconds between pitches). |
| AvgBatSpeed | numeric | Average bat speed (mph). |
| FastSwing_pct | numeric | Fast-swing percentage (75+ mph bat speed). |
| SwingLength | numeric | Average swing length (feet). |
| SquaredUpContact_pct | numeric | Squared-up percentage per batted-ball contact. |
| SquaredUpSwing_pct | numeric | Squared-up percentage per swing. |
| BlastContact_pct | numeric | Blast percentage per batted-ball contact. |
| BlastSwing_pct | numeric | Blast percentage per swing. |
| Swords | integer | Swords (awkward, defensive swinging strikes induced). |
| CompetitiveSwings | integer | Competitive swings tracked. |
| Tilt | numeric | Average swing tilt (degrees). |
| AttackAngle | numeric | Average attack angle (degrees). |
| AttackDirection | numeric | Average attack direction (degrees). |
| IdealAttackAngle_pct | numeric | Percentage of swings in the ideal attack-angle range. |
| DepthInBox | numeric | Average batter depth in the box (inches). |
| DistanceOffPlate | numeric | Average batter distance off the plate (inches). |
| scH-Swing_pct | numeric | Statcast swing percentage on hard-hit pitches. |
| scH-Contact_pct | numeric | Statcast contact percentage on hard-hit pitches. |
| scH-Zone_pct | numeric | Statcast in-zone percentage on hard-hit pitches. |
| scS-Swing_pct | numeric | Statcast swing percentage on slider pitches. |
| scS-Contact_pct | numeric | Statcast contact percentage on slider pitches. |
| scS-Zone_pct | numeric | Statcast in-zone percentage on slider pitches. |
| scC-Swing_pct | numeric | Statcast swing percentage on curveball pitches. |
| scC-Contact_pct | numeric | Statcast contact percentage on curveball pitches. |
| scC-Zone_pct | numeric | Statcast in-zone percentage on curveball pitches. |
| scW-Swing_pct | numeric | Statcast swing percentage on offspeed pitches. |
| scW-Contact_pct | numeric | Statcast contact percentage on offspeed pitches. |
| scW-Zone_pct | numeric | Statcast in-zone percentage on offspeed pitches. |
| scSI-Swing_pct | numeric | Statcast swing percentage on sinker pitches. |
| scSI-Contact_pct | numeric | Statcast contact percentage on sinker pitches. |
| scSI-Zone_pct | numeric | Statcast in-zone percentage on sinker pitches. |
| scSO-Swing_pct | numeric | Statcast swing percentage on softly-hit pitches. |
| scSO-Contact_pct | numeric | Statcast contact percentage on softly-hit pitches. |
| scSO-Zone_pct | numeric | Statcast in-zone percentage on softly-hit pitches. |
| scO-Swing_pct | numeric | Statcast swing percentage on out-of-zone pitches. |
| scO-Contact_pct | numeric | Statcast contact percentage on out-of-zone pitches. |
| scO-Zone_pct | numeric | Statcast in-zone percentage on out-of-zone pitches. |
| scZ-Swing_pct | numeric | Statcast swing percentage on in-zone pitches. |
| scZ-Contact_pct | numeric | Statcast contact percentage on in-zone pitches. |
| scZ-Zone_pct | numeric | Statcast in-zone percentage on in-zone pitches. |
| pi_CH_pct | numeric | PITCHInfo changeups usage percentage. |
| pi_CU_pct | numeric | PITCHInfo curveballs usage percentage. |
| pi_FA_pct | numeric | PITCHInfo four-seam fastballs usage percentage. |
| pi_FC_pct | numeric | PITCHInfo cutters usage percentage. |
| pi_FS_pct | numeric | PITCHInfo splitters usage percentage. |
| pi_SB_pct | numeric | PITCHInfo slow-balls usage percentage. |
| pi_SI_pct | numeric | PITCHInfo sinkers usage percentage. |
| pi_SL_pct | numeric | PITCHInfo sliders usage percentage. |
| pi_vCH | numeric | PITCHInfo average changeups velocity. |
| pi_vCU | numeric | PITCHInfo average curveballs velocity. |
| pi_vFA | numeric | PITCHInfo average four-seam fastballs velocity. |
| pi_vFC | numeric | PITCHInfo average cutters velocity. |
| pi_vFS | numeric | PITCHInfo average splitters velocity. |
| pi_vSB | numeric | PITCHInfo average slow-balls velocity. |
| pi_vSI | numeric | PITCHInfo average sinkers velocity. |
| pi_vSL | numeric | PITCHInfo average sliders velocity. |
| pi_CH-X | numeric | PITCHInfo average horizontal movement on changeups. |
| pi_CU-X | numeric | PITCHInfo average horizontal movement on curveballs. |
| pi_FA-X | numeric | PITCHInfo average horizontal movement on four-seam fastballs. |
| pi_FC-X | numeric | PITCHInfo average horizontal movement on cutters. |
| pi_FS-X | numeric | PITCHInfo average horizontal movement on splitters. |
| pi_SB-X | numeric | PITCHInfo average horizontal movement on slow-balls. |
| pi_SI-X | numeric | PITCHInfo average horizontal movement on sinkers. |
| pi_SL-X | numeric | PITCHInfo average horizontal movement on sliders. |
| pi_CH-Z | numeric | PITCHInfo average vertical movement on changeups. |
| pi_CU-Z | numeric | PITCHInfo average vertical movement on curveballs. |
| pi_FA-Z | numeric | PITCHInfo average vertical movement on four-seam fastballs. |
| pi_FC-Z | numeric | PITCHInfo average vertical movement on cutters. |
| pi_FS-Z | numeric | PITCHInfo average vertical movement on splitters. |
| pi_SB-Z | numeric | PITCHInfo average vertical movement on slow-balls. |
| pi_SI-Z | numeric | PITCHInfo average vertical movement on sinkers. |
| pi_SL-Z | numeric | PITCHInfo average vertical movement on sliders. |
| pi_wCH | numeric | PITCHInfo changeups linear weight runs. |
| pi_wCU | numeric | PITCHInfo curveballs linear weight runs. |
| pi_wFA | numeric | PITCHInfo four-seam fastballs linear weight runs. |
| pi_wFC | numeric | PITCHInfo cutters linear weight runs. |
| pi_wFS | numeric | PITCHInfo splitters linear weight runs. |
| pi_wSB | numeric | PITCHInfo slow-balls linear weight runs. |
| pi_wSI | numeric | PITCHInfo sinkers linear weight runs. |
| pi_wSL | numeric | PITCHInfo sliders linear weight runs. |
| pi_wCH_C | numeric | PITCHInfo changeups linear weight runs per 100 pitches. |
| pi_wCU_C | numeric | PITCHInfo curveballs linear weight runs per 100 pitches. |
| pi_wFA_C | numeric | PITCHInfo four-seam fastballs linear weight runs per 100 pitches. |
| pi_wFC_C | numeric | PITCHInfo cutters linear weight runs per 100 pitches. |
| pi_wFS_C | numeric | PITCHInfo splitters linear weight runs per 100 pitches. |
| pi_wSB_C | numeric | PITCHInfo slow-balls linear weight runs per 100 pitches. |
| pi_wSI_C | numeric | PITCHInfo sinkers linear weight runs per 100 pitches. |
| pi_wSL_C | numeric | PITCHInfo sliders linear weight runs per 100 pitches. |
| pi_O-Swing_pct | numeric | PITCHInfo swing percentage on pitches outside the zone. |
| pi_Z-Swing_pct | numeric | PITCHInfo swing percentage on pitches inside the zone. |
| pi_Swing_pct | numeric | PITCHInfo swing percentage. |
| pi_O-Contact_pct | numeric | PITCHInfo contact percentage on pitches outside the zone. |
| pi_Z-Contact_pct | numeric | PITCHInfo contact percentage on pitches inside the zone. |
| pi_Contact_pct | numeric | PITCHInfo contact percentage. |
| pi_Zone_pct | numeric | PITCHInfo percentage of pitches in the strike zone. |
| pi_Pace | numeric | PITCHInfo pace (seconds between pitches). |
| Events | integer | Batted-ball events with Statcast measurement. |
| EV | numeric | Average exit velocity (mph). |
| LA | numeric | Average launch angle (degrees). |
| Barrels | integer | Barreled batted balls. |
| Barrel_pct | numeric | Barrel percentage. |
| maxEV | numeric | Maximum exit velocity (mph). |
| HardHit | integer | Hard-hit batted balls (95+ mph). |
| HardHit_pct | numeric | Hard-hit percentage (95+ mph). |
| Q | numeric | Quality of contact / quality score. |
| TG | integer | Total games in the span. |
| TPA | integer | Total plate appearances in the span. |
| positionDB | character | Position code from the database. |
| position | character | Position played. |
| team_name_abb | character | Team name abbreviation. |
| teamid | integer | FanGraphs team ID. |
| playerTeamId | integer | FanGraphs player-team ID. |
| Pos | numeric | Primary position. |
| EV90 | numeric | 90th-percentile exit velocity (mph). |
| pi_XX_pct | numeric | PITCHInfo unidentified pitches usage percentage. |
| pi_vXX | numeric | PITCHInfo average unidentified pitches velocity. |
| pi_XX-X | numeric | PITCHInfo average horizontal movement on unidentified pitches. |
| pi_XX-Z | numeric | PITCHInfo average vertical movement on unidentified pitches. |
| pi_wXX | numeric | PITCHInfo unidentified pitches linear weight runs. |
| pi_wXX_C | numeric | PITCHInfo unidentified pitches linear weight runs per 100 pitches. |
| phLI | numeric | Average Leverage Index in pinch-hit appearances. |
| pi_CS_pct | numeric | PITCHInfo slow curves usage percentage. |
| pi_vCS | numeric | PITCHInfo average slow curves velocity. |
| pi_CS-X | numeric | PITCHInfo average horizontal movement on slow curves. |
| pi_CS-Z | numeric | PITCHInfo average vertical movement on slow curves. |
| pi_wCS | numeric | PITCHInfo slow curves linear weight runs. |
| pi_wCS_C | numeric | PITCHInfo slow curves linear weight runs per 100 pitches. |
| rBTV | numeric | Regressed base-state team value. |
| KN_pct | numeric | Knuckleball percentage (pitch usage). |
| KNv | numeric | Average knuckleball velocity. |
| wKN | numeric | Knuckleball pitch-type linear weight runs. |
| wKN_C | numeric | Knuckleball linear weight runs per 100 pitches. |
| pfx_KN_pct | numeric | PITCHf/x knuckleballs usage percentage. |
| pfx_vKN | numeric | PITCHf/x average knuckleballs velocity. |
| pfx_KN-X | numeric | PITCHf/x average horizontal movement on knuckleballs. |
| pfx_KN-Z | numeric | PITCHf/x average vertical movement on knuckleballs. |
| pfx_wKN | numeric | PITCHf/x knuckleballs linear weight runs. |
| pfx_wKN_C | numeric | PITCHf/x knuckleballs linear weight runs per 100 pitches. |
| pfx_aaKN | numeric | PITCHf/x average arsenal value on knuckleballs. |
| pfx_spKN | numeric | PITCHf/x Stuff+ value on knuckleballs. |
| pi_KN_pct | numeric | PITCHInfo knuckleballs usage percentage. |
| pi_vKN | numeric | PITCHInfo average knuckleballs velocity. |
| pi_KN-X | numeric | PITCHInfo average horizontal movement on knuckleballs. |
| pi_KN-Z | numeric | PITCHInfo average vertical movement on knuckleballs. |
| pi_wKN | numeric | PITCHInfo knuckleballs linear weight runs. |
| pi_wKN_C | numeric | PITCHInfo knuckleballs linear weight runs per 100 pitches. |
| rCPTV | numeric | Regressed catcher pitch-type value. |
| CFraming | numeric | Catcher framing runs. |
| rDGV | numeric | Regressed defensive game value. |
| rDSV | numeric | Regressed defensive stuff value. |

## Examples

``` r
# \donttest{
  try(fg_batter_leaders(startseason = 2023, endseason = 2023))
#> ── MLB Player Batting Leaders data from FanGraphs.com ──────────────────
#> ℹ Data updated: 2026-06-08 04:39:15 UTC
#> # A tibble: 1,457 × 447
#>    Season team_name Bats  xMLBAMID PlayerNameRoute  PlayerName  playerid
#>     <int> <chr>     <chr>    <int> <chr>            <chr>          <int>
#>  1   2023 ATL       R       660670 Ronald Acuna Jr. Ronald Acu…    18401
#>  2   2023 LAD       L       518692 Freddie Freeman  Freddie Fr…     5361
#>  3   2023 LAD       R       605141 Mookie Betts     Mookie Bet…    13611
#>  4   2023 ATL       L       621566 Matt Olson       Matt Olson     14344
#>  5   2023 LAA       L       660271 Shohei Ohtani    Shohei Oht…    19755
#>  6   2023 TEX       R       543760 Marcus Semien    Marcus Sem…    12533
#>  7   2023 TEX       L       608369 Corey Seager     Corey Seag…    13624
#>  8   2023 SDP       L       665742 Juan Soto        Juan Soto      20123
#>  9   2023 KCR       R       677951 Bobby Witt Jr.   Bobby Witt…    25764
#> 10   2023 SEA       R       677594 Julio Rodriguez  Julio Rodr…    23697
#> # ℹ 1,447 more rows
#> # ℹ 440 more variables: Age <int>, AgeRng <chr>, SeasonMin <int>,
#> #   SeasonMax <int>, G <int>, AB <int>, PA <int>, H <int>, `1B` <int>,
#> #   `2B` <int>, `3B` <int>, HR <int>, R <int>, RBI <int>, BB <int>,
#> #   IBB <int>, SO <int>, HBP <int>, SF <int>, SH <int>, GDP <int>,
#> #   SB <int>, CS <int>, AVG <dbl>, GB <int>, FB <int>, LD <int>,
#> #   IFFB <int>, Pitches <int>, Balls <int>, Strikes <int>, IFH <int>, …
# }
```
