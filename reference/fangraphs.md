# **FanGraphs Functions Overview**

- [`fg_pitcher_game_logs()`](https://billpetti.github.io/baseballr/reference/fg_pitcher_game_logs.md):
  Scrape Pitcher Game Logs from FanGraphs.

- [`fg_batter_game_logs()`](https://billpetti.github.io/baseballr/reference/fg_batter_game_logs.md):
  Scrape Batter Game Logs from FanGraphs.

- [`fg_milb_pitcher_game_logs()`](https://billpetti.github.io/baseballr/reference/fg_milb_pitcher_game_logs.md):
  Scrape MiLB game logs for pitchers from Fangraphs, combining
  'standard' and 'advanced' tabs.

- [`fg_milb_batter_game_logs()`](https://billpetti.github.io/baseballr/reference/fg_milb_batter_game_logs.md):
  Scrape MiLB game logs for batters from Fangraphs, combining 'standard'
  and 'advanced' tabs.

- [`fg_batter_leaders()`](https://billpetti.github.io/baseballr/reference/fg_batter_leaders.md):
  Scrape Batter Leaderboards from FanGraphs.

- [`fg_pitcher_leaders()`](https://billpetti.github.io/baseballr/reference/fg_pitcher_leaders.md):
  Scrape Pitcher Leaderboards from FanGraphs.

- [`fg_fielder_leaders()`](https://billpetti.github.io/baseballr/reference/fg_fielder_leaders.md):
  Scrape Fielder Leaderboards from FanGraphs.

- [`fg_team_batter()`](https://billpetti.github.io/baseballr/reference/fg_team_batter.md):
  Scrape Team Batter Leaderboards from FanGraphs.

- [`fg_team_pitcher()`](https://billpetti.github.io/baseballr/reference/fg_team_pitcher.md):
  Scrape Team Pitcher Leaderboards from FanGraphs.

- [`fg_team_fielder()`](https://billpetti.github.io/baseballr/reference/fg_team_fielder.md):
  Scrape Team Fielder Leaderboards from FanGraphs.

- [`fg_guts()`](https://billpetti.github.io/baseballr/reference/fg_guts.md):
  Scrape FanGraphs.com Guts!.

- [`fg_park()`](https://billpetti.github.io/baseballr/reference/fg_park.md):
  Scrape Park Factors from FanGraphs.com.

- [`fg_park_hand()`](https://billpetti.github.io/baseballr/reference/fg_park.md):
  Scrape Park Factors by handedness from FanGraphs.com.

## Details

### **Scrape Pitcher Game Logs from FanGraphs**

      fg_pitcher_game_logs(playerid = 104, year = 2006)

### **Scrape Batter Game Logs from FanGraphs**

      fg_batter_game_logs(playerid = 6184, year = 2017)

### **Scrape MiLB game logs for pitchers from Fangraphs**

      fg_milb_pitcher_game_logs(playerid = "sa3004210", year=2017)

### **Scrape MiLB game logs for batters from Fangraphs**

      fg_milb_batter_game_logs(playerid = "sa917940", year=2018)

### **Scrape Batter Leaderboards from FanGraphs**

      fg_batter_leaders(startseason = 2015, endseason = 2015, qual = 400)

### **Scrape Pitcher Leaderboards from FanGraphs**

      fg_pitcher_leaders(startseason = 2015, endseason = 2015, qual = 150)

### **Scrape Fielder Leaderboards from FanGraphs**

      fg_fielder_leaders(startseason = 2015, endseason = 2015, qual = 150)

### **Scrape Team Batter Leaderboards from FanGraphs**

      fg_team_batter(startseason = 2015, endseason = 2015, qual = 400)

### **Scrape Team Pitcher Leaderboards from FanGraphs**

      fg_team_pitcher(startseason = 2015, endseason = 2015, qual = 150)

### **Scrape Team Fielder Leaderboards from FanGraphs**

      fg_team_fielder(startseason = 2015, endseason = 2015, qual = 150)

### **Scrape FanGraphs.com Guts!**

      fg_guts()

### **Scrape Park Factors from FanGraphs.com**

      fg_park(2013)

### **Scrape Park Factors by handedness from FanGraphs.com**

      fg_park_hand(2013)
