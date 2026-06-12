# **Retrieve game linescores for major and minor league games**

**Retrieve game linescores for major and minor league games**

## Usage

``` r
mlb_game_linescore(game_pk)
```

## Arguments

- game_pk:

  The unique game_pk identifier for the game

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_pk | numeric | Unique game identifier. |
| home_team_id | character | Home team MLB ID. |
| home_team_name | character | Home team name. |
| away_team_id | character | Away team MLB ID. |
| away_team_name | character | Away team name. |
| num | integer | Inning number. |
| ordinal_num | character | Inning ordinal label (e.g. 1st). |
| home_runs | integer | Home runs scored in the inning. |
| home_hits | integer | Home hits in the inning. |
| home_errors | integer | Home errors in the inning. |
| home_left_on_base | integer | Home runners left on base in the inning. |
| away_runs | integer | Away runs scored in the inning. |
| away_hits | integer | Away hits in the inning. |
| away_errors | integer | Away errors in the inning. |
| away_left_on_base | integer | Away runners left on base in the inning. |
| home_team_all_star_status | character | Home team all-star status flag. |
| home_team_link | character | MLB Stats API relative home team link. |
| home_team_season | character | Home team season (YYYY). |
| home_team_venue_id | character | Home team venue ID. |
| home_team_venue_name | character | Home team venue name. |
| home_team_venue_link | character | MLB Stats API relative home venue link. |
| home_team_team_code | character | Home team code. |
| home_team_file_code | character | Home team file code. |
| home_team_abbreviation | character | Home team abbreviation. |
| home_team_team_name | character | Home team nickname. |
| home_team_location_name | character | Home team location/city name. |
| home_team_first_year_of_play | character | Home franchise first year of play. |
| home_team_league_id | character | Home team league ID. |
| home_team_league_name | character | Home team league name. |
| home_team_league_link | character | MLB Stats API relative home league link. |
| home_team_division_id | character | Home team division ID. |
| home_team_division_name | character | Home team division name. |
| home_team_division_link | character | MLB Stats API relative home division link. |
| home_team_sport_id | character | Home team sport ID. |
| home_team_sport_link | character | MLB Stats API relative home sport link. |
| home_team_sport_name | character | Home team sport name. |
| home_team_short_name | character | Home team short name. |
| home_team_record_games_played | character | Home team games played. |
| home_team_record_wild_card_games_back | character | Home team games back in the wild card. |
| home_team_record_league_games_back | character | Home team games back in the league. |
| home_team_record_spring_league_games_back | character | Home team games back in the spring league. |
| home_team_record_sport_games_back | character | Home team games back in the sport. |
| home_team_record_division_games_back | character | Home team games back in the division. |
| home_team_record_conference_games_back | character | Home team games back in the conference. |
| home_team_record_league_record_wins | character | Home team league-record wins. |
| home_team_record_league_record_losses | character | Home team league-record losses. |
| home_team_record_league_record_ties | character | Home team league-record ties. |
| home_team_record_league_record_pct | character | Home team winning percentage. |
| home_team_record_division_leader | character | Whether the home team leads its division. |
| home_team_record_wins | character | Home team wins. |
| home_team_record_losses | character | Home team losses. |
| home_team_record_winning_percentage | character | Home team winning percentage. |
| home_team_franchise_name | character | Home franchise name. |
| home_team_club_name | character | Home club name. |
| home_team_active | character | Whether the home team is active. |
| away_team_all_star_status | character | Away team all-star status flag. |
| away_team_link | character | MLB Stats API relative away team link. |
| away_team_season | character | Away team season (YYYY). |
| away_team_venue_id | character | Away team venue ID. |
| away_team_venue_name | character | Away team venue name. |
| away_team_venue_link | character | MLB Stats API relative away venue link. |
| away_team_team_code | character | Away team code. |
| away_team_file_code | character | Away team file code. |
| away_team_abbreviation | character | Away team abbreviation. |
| away_team_team_name | character | Away team nickname. |
| away_team_location_name | character | Away team location/city name. |
| away_team_first_year_of_play | character | Away franchise first year of play. |
| away_team_league_id | character | Away team league ID. |
| away_team_league_name | character | Away team league name. |
| away_team_league_link | character | MLB Stats API relative away league link. |
| away_team_division_id | character | Away team division ID. |
| away_team_division_name | character | Away team division name. |
| away_team_division_link | character | MLB Stats API relative away division link. |
| away_team_sport_id | character | Away team sport ID. |
| away_team_sport_link | character | MLB Stats API relative away sport link. |
| away_team_sport_name | character | Away team sport name. |
| away_team_short_name | character | Away team short name. |
| away_team_record_games_played | character | Away team games played. |
| away_team_record_wild_card_games_back | character | Away team games back in the wild card. |
| away_team_record_league_games_back | character | Away team games back in the league. |
| away_team_record_spring_league_games_back | character | Away team games back in the spring league. |
| away_team_record_sport_games_back | character | Away team games back in the sport. |
| away_team_record_division_games_back | character | Away team games back in the division. |
| away_team_record_conference_games_back | character | Away team games back in the conference. |
| away_team_record_league_record_wins | character | Away team league-record wins. |
| away_team_record_league_record_losses | character | Away team league-record losses. |
| away_team_record_league_record_ties | character | Away team league-record ties. |
| away_team_record_league_record_pct | character | Away team winning percentage. |
| away_team_record_division_leader | character | Whether the away team leads its division. |
| away_team_record_wins | character | Away team wins. |
| away_team_record_losses | character | Away team losses. |
| away_team_record_winning_percentage | character | Away team winning percentage. |
| away_team_franchise_name | character | Away franchise name. |
| away_team_club_name | character | Away club name. |
| away_team_active | character | Whether the away team is active. |

## Examples

``` r
# \donttest{
  try(mlb_game_linescore(game_pk = 566001))
#> ── MLB Game Linescore data from MLB.com ───────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:15:51 UTC
#> # A tibble: 9 × 95
#>   game_pk home_team_id home_team_name away_team_id away_team_name    num
#>     <dbl> <chr>        <chr>          <chr>        <chr>           <int>
#> 1  566001 121          New York Mets  113          Cincinnati Reds     1
#> 2  566001 121          New York Mets  113          Cincinnati Reds     2
#> 3  566001 121          New York Mets  113          Cincinnati Reds     3
#> 4  566001 121          New York Mets  113          Cincinnati Reds     4
#> 5  566001 121          New York Mets  113          Cincinnati Reds     5
#> 6  566001 121          New York Mets  113          Cincinnati Reds     6
#> 7  566001 121          New York Mets  113          Cincinnati Reds     7
#> 8  566001 121          New York Mets  113          Cincinnati Reds     8
#> 9  566001 121          New York Mets  113          Cincinnati Reds     9
#> # ℹ 89 more variables: ordinal_num <chr>, home_runs <int>,
#> #   home_hits <int>, home_errors <int>, home_left_on_base <int>,
#> #   away_runs <int>, away_hits <int>, away_errors <int>,
#> #   away_left_on_base <int>, home_team_all_star_status <chr>,
#> #   home_team_link <chr>, home_team_season <chr>,
#> #   home_team_venue_id <chr>, home_team_venue_name <chr>,
#> #   home_team_venue_link <chr>, home_team_team_code <chr>, …
# }
```
