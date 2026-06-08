# **Get ESPN MLB's Standings**

**Get ESPN MLB's Standings**

## Usage

``` r
espn_mlb_standings(year)
```

## Arguments

- year:

  Either numeric or character (YYYY)

## Value

A standings data frame

|                     |           |                              |
|---------------------|-----------|------------------------------|
| col_name            | types     | description                  |
| team_id             | integer   | Unique ESPN team identifier. |
| team                | character | Team.                        |
| otlosses            | character | Otlosses.                    |
| otwins              | character | Otwins.                      |
| avgpointsagainst    | numeric   | Avgpointsagainst.            |
| avgpointsfor        | numeric   | Avgpointsfor.                |
| clincher            | numeric   | Clincher.                    |
| differential        | numeric   | Differential.                |
| divisionwinpercent  | numeric   | Divisionwinpercent.          |
| gamesbehind         | numeric   | Gamesbehind.                 |
| gamesplayed         | character | Gamesplayed.                 |
| leaguewinpercent    | numeric   | Leaguewinpercent.            |
| losses              | numeric   | Losses.                      |
| playoffseed         | numeric   | Playoffseed.                 |
| pointdifferential   | character | Pointdifferential.           |
| points              | character | Points.                      |
| pointsagainst       | character | Pointsagainst.               |
| pointsfor           | character | Pointsfor.                   |
| streak              | numeric   | Streak.                      |
| ties                | character | Ties.                        |
| winpercent          | numeric   | Winpercent.                  |
| wins                | numeric   | Wins.                        |
| divisiongamesbehind | character | Divisiongamesbehind.         |
| divisionpercent     | character | Divisionpercent.             |
| divisiontied        | character | Divisiontied.                |
| homelosses          | character | Homelosses.                  |
| hometies            | character | Hometies.                    |
| homewins            | character | Homewins.                    |
| magicnumberdivision | character | Magicnumberdivision.         |
| magicnumberwildcard | character | Magicnumberwildcard.         |
| playoffpercent      | character | Playoffpercent.              |
| roadlosses          | character | Roadlosses.                  |
| roadties            | character | Roadties.                    |
| roadwins            | character | Roadwins.                    |
| wildcardpercent     | character | Wildcardpercent.             |
| total               | character | Total.                       |
| home                | character | Home.                        |
| road                | character | Road.                        |
| intradivision       | character | Intradivision.               |
| intraleague         | character | Intraleague.                 |
| lasttengames        | character | Lasttengames.                |

## See also

Other ESPN MLB Functions:
[`espn_mlb`](https://billpetti.github.io/baseballr/reference/espn_mlb.md),
[`espn_mlb_athletes_index()`](https://billpetti.github.io/baseballr/reference/espn_mlb_athletes_index.md),
[`espn_mlb_award()`](https://billpetti.github.io/baseballr/reference/espn_mlb_award.md),
[`espn_mlb_betting()`](https://billpetti.github.io/baseballr/reference/espn_mlb_betting.md),
[`espn_mlb_calendar()`](https://billpetti.github.io/baseballr/reference/espn_mlb_calendar.md),
[`espn_mlb_coach()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coach.md),
[`espn_mlb_coach_record()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coach_record.md),
[`espn_mlb_coach_season()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coach_season.md),
[`espn_mlb_coaches()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coaches.md),
[`espn_mlb_conferences()`](https://billpetti.github.io/baseballr/reference/espn_mlb_conferences.md),
[`espn_mlb_draft()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft.md),
[`espn_mlb_draft_athlete_detail()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_athlete_detail.md),
[`espn_mlb_draft_athletes()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_athletes.md),
[`espn_mlb_draft_pick()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_pick.md),
[`espn_mlb_draft_rounds()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_rounds.md),
[`espn_mlb_draft_status()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_status.md),
[`espn_mlb_franchise()`](https://billpetti.github.io/baseballr/reference/espn_mlb_franchise.md),
[`espn_mlb_franchises()`](https://billpetti.github.io/baseballr/reference/espn_mlb_franchises.md),
[`espn_mlb_freeagents()`](https://billpetti.github.io/baseballr/reference/espn_mlb_freeagents.md),
[`espn_mlb_futures()`](https://billpetti.github.io/baseballr/reference/espn_mlb_futures.md),
[`espn_mlb_game_all()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_all.md),
[`espn_mlb_game_broadcasts()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_broadcasts.md),
[`espn_mlb_game_endpoints`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_endpoints.md),
[`espn_mlb_game_info()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_info.md),
[`espn_mlb_game_odds()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_odds.md),
[`espn_mlb_game_official_detail()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_official_detail.md),
[`espn_mlb_game_officials()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_officials.md),
[`espn_mlb_game_play()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_play.md),
[`espn_mlb_game_play_personnel()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_play_personnel.md),
[`espn_mlb_game_player_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_player_box.md),
[`espn_mlb_game_powerindex()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_powerindex.md),
[`espn_mlb_game_predictor()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_predictor.md),
[`espn_mlb_game_probabilities()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_probabilities.md),
[`espn_mlb_game_probables()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_probables.md),
[`espn_mlb_game_propbets()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_propbets.md),
[`espn_mlb_game_rosters()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_rosters.md),
[`espn_mlb_game_situation()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_situation.md),
[`espn_mlb_game_team_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_leaders.md),
[`espn_mlb_game_team_linescores()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_linescores.md),
[`espn_mlb_game_team_records()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_records.md),
[`espn_mlb_game_team_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_roster.md),
[`espn_mlb_game_team_roster_entry()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_roster_entry.md),
[`espn_mlb_game_team_score()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_score.md),
[`espn_mlb_game_team_statistics()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_statistics.md),
[`espn_mlb_injuries()`](https://billpetti.github.io/baseballr/reference/espn_mlb_injuries.md),
[`espn_mlb_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_leaders.md),
[`espn_mlb_news()`](https://billpetti.github.io/baseballr/reference/espn_mlb_news.md),
[`espn_mlb_pbp()`](https://billpetti.github.io/baseballr/reference/espn_mlb_pbp.md),
[`espn_mlb_player_awards()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_awards.md),
[`espn_mlb_player_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_box.md),
[`espn_mlb_player_career_stats()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_career_stats.md),
[`espn_mlb_player_contract()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_contract.md),
[`espn_mlb_player_contracts()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_contracts.md),
[`espn_mlb_player_endpoints`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_endpoints.md),
[`espn_mlb_player_eventlog()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_eventlog.md),
[`espn_mlb_player_eventlog_v2()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_eventlog_v2.md),
[`espn_mlb_player_gamelog()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_gamelog.md),
[`espn_mlb_player_info()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_info.md),
[`espn_mlb_player_overview()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_overview.md),
[`espn_mlb_player_seasons()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_seasons.md),
[`espn_mlb_player_splits()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_splits.md),
[`espn_mlb_player_statisticslog()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_statisticslog.md),
[`espn_mlb_player_stats()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_stats.md),
[`espn_mlb_player_stats_v3()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_stats_v3.md),
[`espn_mlb_position()`](https://billpetti.github.io/baseballr/reference/espn_mlb_position.md),
[`espn_mlb_positions()`](https://billpetti.github.io/baseballr/reference/espn_mlb_positions.md),
[`espn_mlb_powerindex()`](https://billpetti.github.io/baseballr/reference/espn_mlb_powerindex.md),
[`espn_mlb_scoreboard()`](https://billpetti.github.io/baseballr/reference/espn_mlb_scoreboard.md),
[`espn_mlb_season_awards()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_awards.md),
[`espn_mlb_season_draft()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_draft.md),
[`espn_mlb_season_group()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_group.md),
[`espn_mlb_season_group_children()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_group_children.md),
[`espn_mlb_season_group_teams()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_group_teams.md),
[`espn_mlb_season_groups()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_groups.md),
[`espn_mlb_season_info()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_info.md),
[`espn_mlb_season_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_leaders.md),
[`espn_mlb_season_ranking()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_ranking.md),
[`espn_mlb_season_rankings()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_rankings.md),
[`espn_mlb_season_type()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_type.md),
[`espn_mlb_season_types()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_types.md),
[`espn_mlb_season_week()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_week.md),
[`espn_mlb_season_weeks()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_weeks.md),
[`espn_mlb_seasons()`](https://billpetti.github.io/baseballr/reference/espn_mlb_seasons.md),
[`espn_mlb_team()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team.md),
[`espn_mlb_team_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_box.md),
[`espn_mlb_team_current_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_current_roster.md),
[`espn_mlb_team_depthchart()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_depthchart.md),
[`espn_mlb_team_endpoints`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_endpoints.md),
[`espn_mlb_team_injuries()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_injuries.md),
[`espn_mlb_team_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_leaders.md),
[`espn_mlb_team_news()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_news.md),
[`espn_mlb_team_odds_records()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_odds_records.md),
[`espn_mlb_team_record()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_record.md),
[`espn_mlb_team_record_detail()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_record_detail.md),
[`espn_mlb_team_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_roster.md),
[`espn_mlb_team_schedule()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_schedule.md),
[`espn_mlb_team_season_profile()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_season_profile.md),
[`espn_mlb_team_season_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_season_roster.md),
[`espn_mlb_team_season_statistics()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_season_statistics.md),
[`espn_mlb_team_stats()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_stats.md),
[`espn_mlb_teams()`](https://billpetti.github.io/baseballr/reference/espn_mlb_teams.md),
[`espn_mlb_tournament()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournament.md),
[`espn_mlb_tournament_season()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournament_season.md),
[`espn_mlb_tournament_seasons()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournament_seasons.md),
[`espn_mlb_tournaments()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournaments.md),
[`espn_mlb_transactions()`](https://billpetti.github.io/baseballr/reference/espn_mlb_transactions.md),
[`espn_mlb_venues()`](https://billpetti.github.io/baseballr/reference/espn_mlb_venues.md),
[`espn_mlb_week_ranking()`](https://billpetti.github.io/baseballr/reference/espn_mlb_week_ranking.md),
[`espn_mlb_week_rankings()`](https://billpetti.github.io/baseballr/reference/espn_mlb_week_rankings.md),
[`espn_mlb_wp()`](https://billpetti.github.io/baseballr/reference/espn_mlb_wp.md)

## Examples

``` r
# \donttest{
try(espn_mlb_standings(year = 2021))
#> ── ESPN MLB Standings Information from ESPN.com ───── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 04:38:58 UTC
#> # A tibble: 30 × 41
#>    team_id team   otlosses otwins avgpointsagainst avgpointsfor clincher
#>      <int> <chr>  <chr>    <chr>             <dbl>        <dbl>    <dbl>
#>  1      26 San F… 9        9                  3.67         4.96        5
#>  2      19 Los A… 12       6                  3.46         5.12        2
#>  3      30 Tampa… 12       7                  4.02         5.29        5
#>  4       8 Milwa… 8        10                 3.85         4.56        1
#>  5      18 Houst… 8        9                  4.06         5.33        1
#>  6       4 Chica… 6        5                  3.93         4.91        1
#>  7       2 Bosto… 5        10                 4.62         5.12        2
#>  8      10 New Y… 8        10                 4.13         4.39        2
#>  9      14 Toron… 8        3                  4.09         5.22        4
#> 10      24 St. L… 2        7                  4.15         4.36        2
#> # ℹ 20 more rows
#> # ℹ 34 more variables: differential <dbl>, divisionwinpercent <dbl>,
#> #   gamesbehind <dbl>, gamesplayed <chr>, leaguewinpercent <dbl>,
#> #   losses <dbl>, playoffseed <dbl>, pointdifferential <chr>,
#> #   points <chr>, pointsagainst <chr>, pointsfor <chr>, streak <dbl>,
#> #   ties <chr>, winpercent <dbl>, wins <dbl>,
#> #   divisiongamesbehind <chr>, divisionpercent <chr>, …
# }
```
