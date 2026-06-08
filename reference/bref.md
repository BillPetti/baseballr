# **Baseball Reference Functions Overview**

- [`bref_daily_batter()`](https://billpetti.github.io/baseballr/reference/bref_daily_batter.md):
  Scrape Batter Performance Data Over a Custom Time Frame

- [`bref_daily_pitcher()`](https://billpetti.github.io/baseballr/reference/bref_daily_pitcher.md):
  Scrape Pitcher Performance Data Over a Custom Time Frame

- [`bref_standings_on_date()`](https://billpetti.github.io/baseballr/reference/bref_standings_on_date.md):
  Scrape MLB Standings on a Given Date

- [`bref_team_results()`](https://billpetti.github.io/baseballr/reference/bref_team_results.md):
  Scrape Team Results

## Details

### **Scrape Batter Performance Data Over a Custom Time Frame**

      bref_daily_batter("2015-05-10", "2015-06-20")

### **Scrape Pitcher Performance Data Over a Custom Time Frame**

      bref_daily_batter("2015-05-10", "2015-06-20")

### **Scrape MLB Standings on a Given Date**

      bref_standings_on_date(date = "2015-08-04", division = "AL East")

### **Scrape Team Results**

      bref_team_results("NYM", 2015)
      bref_team_results(Tm="TBR", year=2008)

### **Team Level Consistency**

Uses
[`bref_team_results()`](https://billpetti.github.io/baseballr/reference/bref_team_results.md)
to calculate team consistency metrics

      team_consistency(year=2015)
