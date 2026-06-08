# **MLB All-Star, Awards, Home Run Derby Functions**

- [`mlb_all_star_ballots()`](https://billpetti.github.io/baseballr/reference/mlb_all_star_ballots.md):
  Find MLB All-Star Ballots.

- [`mlb_all_star_final_vote()`](https://billpetti.github.io/baseballr/reference/mlb_all_star_final_vote.md):
  Find MLB All-Star Final Vote.

- [`mlb_all_star_write_ins()`](https://billpetti.github.io/baseballr/reference/mlb_all_star_write_ins.md):
  Find MLB All-Star Write-ins.

- [`mlb_awards()`](https://billpetti.github.io/baseballr/reference/mlb_awards.md):
  Find MLB Awards.

- [`mlb_awards_recipient()`](https://billpetti.github.io/baseballr/reference/mlb_awards_recipient.md):
  Find MLB Award Recipients.

- [`mlb_homerun_derby()`](https://billpetti.github.io/baseballr/reference/mlb_homerun_derby.md):
  Retrieve MLB Home Run Derby Data.

- [`mlb_homerun_derby_bracket()`](https://billpetti.github.io/baseballr/reference/mlb_homerun_derby_bracket.md):
  Retrieve MLB Home Run Derby Bracket.

- [`mlb_homerun_derby_players()`](https://billpetti.github.io/baseballr/reference/mlb_homerun_derby_players.md):
  Retrieve MLB Home Run Derby Players.

## Details

### **Find MLB All-Star Ballots**

      try(mlb_all_star_ballots(league_id = 103, season = 2021))

### **Find MLB All-Star Final Vote**

      try(mlb_all_star_final_vote(league_id = 103, season = 2021))

### **Find MLB All-Star Write-ins**

      try(mlb_all_star_write_ins(league_id = 103, season = 2021))

### **Find MLB Awards**

      try(mlb_awards())

### **Find MLB Award Recipients**

      try(mlb_awards_recipient(award_id = 'MLBHOF', season = 2020))

### **Retrieve MLB Home Run Derby Data**

      try(mlb_homerun_derby(game_pk = 511101))

### **Retrieve MLB Home Run Derby Bracket**

       try(mlb_homerun_derby_bracket(game_pk = 511101))

### **Retrieve MLB Home Run Derby Players**

       try(mlb_homerun_derby_players(game_pk = 511101))
