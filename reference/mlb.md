# **MLB Functions Overview**

- [`mlb_batting_orders()`](https://billpetti.github.io/baseballr/reference/mlb_batting_orders.md):
  Retrieve batting orders for a given MLB game.

- [`mlb_draft()`](https://billpetti.github.io/baseballr/reference/mlb_draft.md):
  Retrieve draft pick information by year.

- [`mlb_pbp()`](https://billpetti.github.io/baseballr/reference/mlb_pbp.md):
  Acquire pitch-by-pitch data for Major and Minor League games.

- [`mlb_game_info()`](https://billpetti.github.io/baseballr/reference/mlb_game_info.md):
  Retrieve additional game information for major and minor league games.

- [`mlb_game_pks()`](https://billpetti.github.io/baseballr/reference/mlb_game_pks.md):
  Get MLB Game Info by Date and Level.

- [`mlb_schedule()`](https://billpetti.github.io/baseballr/reference/mlb_schedule.md):
  Find game_pk values for professional baseball games (major and minor
  leagues).

- [`mlb_probables()`](https://billpetti.github.io/baseballr/reference/mlb_probables.md):
  Retrieve probable starters for a given MLB game.

## Details

### **Retrieve batting orders for a given MLB game**

      mlb_batting_orders(game_pk=566001)

### **Retrieve draft pick information by year**

      mlb_draft(year= 2018)

### **Acquire pitch-by-pitch data for Major and Minor League games**

      mlb_pbp(game_pk = 575156)

### **Retrieve additional game information for major and minor league games**

      mlb_game_info(game_pk = 566001)

### **Get MLB Game Info by Date and Level**

      mlb_game_pks("2019-04-29")

### **Find game_pk values for professional baseball games (major and minor leagues)**

      mlb_schedule(season = "2019")

### **Retrieve probable starters for a given MLB game**

      mlb_probables(566001)
