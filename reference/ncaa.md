# **NCAA Functions Overview**

- [`ncaa_team_player_stats()`](https://billpetti.github.io/baseballr/reference/ncaa_team_player_stats.md):
  This function allows the user to obtain batting, pitching, or fielding
  statistics for any school affiliated with the NCAA at the division I,
  II, or III levels. The function acquires data from the NCAA's website
  (stats.ncaa.org) and returns a tibble.

- [`ncaa_pbp()`](https://billpetti.github.io/baseballr/reference/ncaa_pbp.md):
  Get Play-By-Play Data for NCAA Baseball Games.

- [`ncaa_roster()`](https://billpetti.github.io/baseballr/reference/ncaa_roster.md):
  Get NCAA Baseball Rosters.

- [`ncaa_game_logs()`](https://billpetti.github.io/baseballr/reference/ncaa_game_logs.md):
  Get NCAA Baseball Game Logs.

- [`ncaa_lineups()`](https://billpetti.github.io/baseballr/reference/ncaa_lineups.md):
  Get NCAA Baseball Game Lineups.

- [`ncaa_park_factor()`](https://billpetti.github.io/baseballr/reference/ncaa_park_factor.md):
  Get Park Effects for NCAA Baseball Teams.

- [`ncaa_schedule_info()`](https://billpetti.github.io/baseballr/reference/ncaa_schedule_info.md):
  Get Schedule and Results for NCAA Baseball Teams.

- [`ncaa_school_id_lu()`](https://billpetti.github.io/baseballr/reference/ncaa_school_id_lu.md):
  Lookup NCAA School IDs (Division I, II, and III)

- [`ncaa_teams()`](https://billpetti.github.io/baseballr/reference/ncaa_teams.md):
  Lookup NCAA Teams by Division (I, II, and III) and Season

## Details

### **Scrape NCAA baseball data (Division I, II, and III)**

      ncaa_team_player_stats(team_id = 255, year = 2013, type = "batting")

### **Get Play-By-Play Data for NCAA Baseball Games**

      x <- ncaa_schedule_info(736, 2021)$game_info_url[2]
      ncaa_pbp(game_info_url = x)

### **Get NCAA Baseball Rosters**

      ncaa_roster(team_id = 104, year = 2021)

### **Get NCAA Baseball Game Logs**

      ncaa_game_logs(player_id = 2113782, year = 2021, type = "pitching", span = "game")

### **Get NCAA Baseball Game Lineups**

      ncaa_lineups(game_info_url="https://stats.ncaa.org/game/index/4587474?org_id=528",year=2018)

### **Get Park Effects for NCAA Baseball Teams**

       ncaa_park_factor(team_id = 736, years = c(2017:2019), type = "conference")

### **Get Schedule and Results for NCAA Baseball Teams**

      ncaa_schedule_info(team_id = 736, year = 2021)

### **Lookup NCAA School IDs (Division I, II, and III)**

      ncaa_school_id_lu("VAN")

### **Scrape NCAA baseball Teams (Division I, II, and III)**

      ncaa_teams(year = 2023, division = 1)
