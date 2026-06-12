# **Acquire pitch-by-pitch data for Major and Minor League games**

**Acquire pitch-by-pitch data for Major and Minor League games**

## Usage

``` r
mlb_pbp(game_pk)
```

## Arguments

- game_pk:

  The date for which you want to find game_pk values for MLB games

## Value

Returns a tibble that includes over 100 columns of data provided by the
MLB Stats API at a pitch level.

Some data will vary depending on the park and the league level, as most
sensor data is not available in minor league parks via this API. Note
that the column names have mostly been left as-is and there are likely
duplicate columns in terms of the information they provide. I plan to
clean the output up down the road, but for now I am leaving the majority
as-is.

Both major and minor league pitch-by-pitch data can be pulled with this
function.

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_pk | numeric | MLB game primary key. |
| game_date | character | Game date (YYYY-MM-DD). |
| index | integer | Index of the play event within the at-bat. |
| startTime | character | Event start timestamp (ISO 8601). |
| endTime | character | Event end timestamp (ISO 8601). |
| isPitch | logical | Whether the event is a pitch. |
| type | character | Play event type (e.g. 'pitch', 'action'). |
| playId | character | Unique play event identifier (UUID). |
| pitchNumber | integer | Pitch number within the at-bat. |
| details.description | character | Pitch/event description (e.g. 'Swinging Strike'). |
| details.event | character | Event name for non-pitch actions. |
| details.awayScore | integer | Away score recorded at the event. |
| details.homeScore | integer | Home score recorded at the event. |
| details.isScoringPlay | logical | Whether the event is a scoring play. |
| details.hasReview | logical | Whether the event was reviewed. |
| details.code | character | Pitch/result code (e.g. 'S', 'C', 'B'). |
| details.ballColor | character | Gameday ball color rgba string. |
| details.isInPlay | logical | Whether the ball was put in play. |
| details.isStrike | logical | Whether the pitch was a strike. |
| details.isBall | logical | Whether the pitch was a ball. |
| details.call.code | character | Umpire call code. |
| details.call.description | character | Umpire call description. |
| count.balls.start | integer | Ball count before the pitch. |
| count.strikes.start | integer | Strike count before the pitch. |
| count.outs.start | integer | Out count before the pitch. |
| player.id | integer | Player id for action-event participant. |
| player.link | character | API relative link to the action player. |
| pitchData.strikeZoneTop | numeric | Top of the batter strike zone (feet). |
| pitchData.strikeZoneBottom | numeric | Bottom of the batter strike zone (feet). |
| details.fromCatcher | logical | Whether the event originated from the catcher. |
| pitchData.coordinates.x | numeric | Pitch x pixel coordinate (Gameday). |
| pitchData.coordinates.y | numeric | Pitch y pixel coordinate (Gameday). |
| hitData.trajectory | character | Batted ball trajectory. |
| hitData.hardness | character | Batted ball hardness. |
| hitData.location | character | Fielding position where the ball was hit. |
| hitData.coordinates.coordX | numeric | Batted ball landing x coordinate. |
| hitData.coordinates.coordY | numeric | Batted ball landing y coordinate. |
| actionPlayId | character | Identifier of an associated action play. |
| details.eventType | character | Event type slug for non-pitch actions. |
| details.runnerGoing | logical | Whether a runner was going on the pitch. |
| position.code | character | Fielding position code for the player. |
| position.name | character | Fielding position name. |
| position.type | character | Fielding position type. |
| position.abbreviation | character | Fielding position abbreviation. |
| battingOrder | character | Batting order slot. |
| atBatIndex | character | At-bat index within the game (factor). |
| result.type | character | Result type (e.g. 'atBat'). |
| result.event | character | At-bat result event (e.g. 'Strikeout'). |
| result.eventType | character | At-bat result event slug. |
| result.description | character | Narrative description of the at-bat result. |
| result.rbi | integer | Runs batted in on the at-bat. |
| result.awayScore | integer | Away score after the at-bat. |
| result.homeScore | integer | Home score after the at-bat. |
| about.atBatIndex | integer | At-bat index (numeric). |
| about.halfInning | character | Half inning ('top' or 'bottom'). |
| about.inning | integer | Inning number. |
| about.startTime | character | At-bat start timestamp (ISO 8601). |
| about.endTime | character | At-bat end timestamp (ISO 8601). |
| about.isComplete | logical | Whether the at-bat is complete. |
| about.isScoringPlay | logical | Whether the at-bat is a scoring play. |
| about.hasReview | logical | Whether the at-bat had a review. |
| about.hasOut | logical | Whether the at-bat produced an out. |
| about.captivatingIndex | integer | MLB captivating index for the play. |
| count.balls.end | integer | Ball count after the pitch. |
| count.strikes.end | integer | Strike count after the pitch. |
| count.outs.end | integer | Out count after the pitch. |
| matchup.batter.id | integer | Batter player id. |
| matchup.batter.fullName | character | Batter full name (factor). |
| matchup.batter.link | character | API relative link to the batter. |
| matchup.batSide.code | character | Batter handedness code (e.g. 'L', 'R'). |
| matchup.batSide.description | character | Batter handedness description. |
| matchup.pitcher.id | integer | Pitcher player id. |
| matchup.pitcher.fullName | character | Pitcher full name (factor). |
| matchup.pitcher.link | character | API relative link to the pitcher. |
| matchup.pitchHand.code | character | Pitcher throwing hand code (e.g. 'R'). |
| matchup.pitchHand.description | character | Pitcher throwing hand description. |
| matchup.splits.batter | character | Batter platoon split (e.g. 'vs_RHP'). |
| matchup.splits.pitcher | character | Pitcher platoon split (e.g. 'vs_LHB'). |
| matchup.splits.menOnBase | character | Men on base split (e.g. 'Empty', 'Men_On'). |
| batted.ball.result | factor | Categorized batted ball result. |
| home_team | character | Home team name. |
| home_level_id | integer | Home team level/sport id (1 for MLB). |
| home_level_name | character | Home team level/sport name. |
| home_parentOrg_id | integer | Home team parent organization id (minors). |
| home_parentOrg_name | character | Home team parent organization name (minors). |
| home_league_id | integer | Home team league id. |
| home_league_name | character | Home team league name. |
| away_team | character | Away team name. |
| away_level_id | integer | Away team level/sport id (1 for MLB). |
| away_level_name | character | Away team level/sport name. |
| away_parentOrg_id | integer | Away team parent organization id (minors). |
| away_parentOrg_name | character | Away team parent organization name (minors). |
| away_league_id | integer | Away team league id. |
| away_league_name | character | Away team league name. |
| batting_team | character | Team batting on the play (factor). |
| fielding_team | character | Team fielding on the play (factor). |
| last.pitch.of.ab | character | Whether the pitch was the last of the at-bat (factor). |
| pfxId | character | Pitch f/x tracking identifier. |
| details.trailColor | character | Gameday pitch trail color rgba string. |
| details.type.code | character | Pitch type code (e.g. 'CU', 'SI'). |
| details.type.description | character | Pitch type description (e.g. 'Curveball'). |
| pitchData.startSpeed | numeric | Pitch release speed (mph). |
| pitchData.endSpeed | numeric | Pitch speed crossing the plate (mph). |
| pitchData.zone | integer | Strike zone region of the pitch. |
| pitchData.typeConfidence | numeric | Pitch type classification confidence. |
| pitchData.plateTime | numeric | Time from release to plate (seconds). |
| pitchData.extension | numeric | Pitcher release extension (feet). |
| pitchData.coordinates.aY | numeric | Pitch acceleration in y (ft/s^2). |
| pitchData.coordinates.aZ | numeric | Pitch acceleration in z (ft/s^2). |
| pitchData.coordinates.pfxX | numeric | Horizontal pitch movement (inches). |
| pitchData.coordinates.pfxZ | numeric | Vertical pitch movement (inches). |
| pitchData.coordinates.pX | numeric | Horizontal pitch location at plate (feet). |
| pitchData.coordinates.pZ | numeric | Vertical pitch location at plate (feet). |
| pitchData.coordinates.vX0 | numeric | Pitch initial velocity in x (ft/s). |
| pitchData.coordinates.vY0 | numeric | Pitch initial velocity in y (ft/s). |
| pitchData.coordinates.vZ0 | numeric | Pitch initial velocity in z (ft/s). |
| pitchData.coordinates.x0 | numeric | Pitch initial x position (feet). |
| pitchData.coordinates.y0 | numeric | Pitch initial y position (feet). |
| pitchData.coordinates.z0 | numeric | Pitch initial z position (feet). |
| pitchData.coordinates.aX | numeric | Pitch acceleration in x (ft/s^2). |
| pitchData.breaks.breakAngle | numeric | Pitch break angle (degrees). |
| pitchData.breaks.breakLength | numeric | Pitch break length (inches). |
| pitchData.breaks.breakY | numeric | Distance from plate where break is measured. |
| pitchData.breaks.spinRate | integer | Pitch spin rate (RPM). |
| pitchData.breaks.spinDirection | integer | Pitch spin direction (degrees). |
| hitData.launchSpeed | numeric | Batted ball exit velocity (mph). |
| hitData.launchAngle | numeric | Batted ball launch angle (degrees). |
| hitData.totalDistance | numeric | Batted ball total distance (feet). |
| injuryType | character | Injury type when the event is injury-related. |
| umpire.id | integer | Umpire person id for the event. |
| umpire.link | character | API relative link to the umpire. |
| details.isOut | logical | Whether the pitch/event resulted in an out. |
| pitchData.breaks.breakVertical | numeric | Total vertical break (inches). |
| pitchData.breaks.breakVerticalInduced | numeric | Induced vertical break (inches). |
| pitchData.breaks.breakHorizontal | numeric | Horizontal break (inches). |
| details.disengagementNum | integer | Pitcher disengagement number on the play. |
| isBaseRunningPlay | logical | Whether the event is a base running play. |
| isSubstitution | logical | Whether the event is a substitution. |
| replacedPlayer.id | integer | Player id replaced on a substitution. |
| replacedPlayer.link | character | API relative link to the replaced player. |
| result.isOut | logical | Whether the at-bat resulted in an out. |
| about.isTopInning | logical | Whether the play occurred in the top of the inning. |
| matchup.postOnFirst.id | integer | Runner id on first base after the play. |
| matchup.postOnFirst.fullName | character | Runner name on first base after the play. |
| matchup.postOnFirst.link | character | API relative link to the first base runner. |
| matchup.postOnSecond.id | integer | Runner id on second base after the play. |
| matchup.postOnSecond.fullName | character | Runner name on second base after the play. |
| matchup.postOnSecond.link | character | API relative link to the second base runner. |
| matchup.postOnThird.id | integer | Runner id on third base after the play. |
| matchup.postOnThird.fullName | character | Runner name on third base after the play. |
| matchup.postOnThird.link | character | API relative link to the third base runner. |

## Examples

``` r
# \donttest{
  try(mlb_pbp(game_pk = 632970))
#> ── MLB Play-by-Play data from MLB.com ─────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:57:10 UTC
#> # A tibble: 336 × 151
#>    game_pk game_date  index startTime       endTime isPitch type  playId
#>      <dbl> <chr>      <int> <chr>           <chr>   <lgl>   <chr> <chr> 
#>  1  632970 2021-08-08     3 2021-08-09T02:… 2021-0… TRUE    pitch 932a8…
#>  2  632970 2021-08-08     2 2021-08-09T02:… 2021-0… TRUE    pitch e9403…
#>  3  632970 2021-08-08     1 2021-08-09T02:… 2021-0… TRUE    pitch 01e5d…
#>  4  632970 2021-08-08     0 2021-08-09T02:… 2021-0… FALSE   acti… NA    
#>  5  632970 2021-08-08     5 2021-08-09T02:… 2021-0… TRUE    pitch f0450…
#>  6  632970 2021-08-08     4 2021-08-09T02:… 2021-0… TRUE    pitch b4c15…
#>  7  632970 2021-08-08     3 2021-08-09T02:… 2021-0… TRUE    pitch e274e…
#>  8  632970 2021-08-08     2 2021-08-09T02:… 2021-0… TRUE    pitch d5dc0…
#>  9  632970 2021-08-08     1 2021-08-09T02:… 2021-0… TRUE    pitch ddbcc…
#> 10  632970 2021-08-08     0 2021-08-09T02:… 2021-0… TRUE    pitch 69a40…
#> # ℹ 326 more rows
#> # ℹ 143 more variables: pitchNumber <int>, details.description <chr>,
#> #   details.event <chr>, details.awayScore <int>,
#> #   details.homeScore <int>, details.isScoringPlay <lgl>,
#> #   details.hasReview <lgl>, details.code <chr>,
#> #   details.ballColor <chr>, details.isInPlay <lgl>,
#> #   details.isStrike <lgl>, details.isBall <lgl>, …
# }
```
