# **Metrics Functions Overview**

- [`fip_plus()`](https://billpetti.github.io/baseballr/reference/fip_plus.md):
  Calculate FIP and related metrics for any set of data.

- [`woba_plus()`](https://billpetti.github.io/baseballr/reference/woba_plus.md):
  Calculate wOBA and related metrics for any set of data.

- [`team_consistency()`](https://billpetti.github.io/baseballr/reference/team_consistency.md):
  Calculate Team-level Consistency.

- [`label_statcast_imputed_data()`](https://billpetti.github.io/baseballr/reference/label_statcast_imputed_data.md):
  Label Statcast data as imputed.

- [`run_expectancy_code()`](https://billpetti.github.io/baseballr/reference/run_expectancy_code.md):
  Generate run expectancy and related measures from Baseball Savant
  data.

- [`linear_weights_savant()`](https://billpetti.github.io/baseballr/reference/linear_weights_savant.md):
  Generate linear weight values for events using Baseball Savant data.

## Details

### **Calculate Team-level Consistency**

      team_consistency(year=2015)

### **Calculate FIP and related metrics for any set of data**

      fips_plus(df)

### **Calculate wOBA and related metrics for any set of data**

      woba_plus(df)

### **Label Statcast data as imputed**

      statcast_df <- scrape_statcast_savant("2017-05-01", "2017-05-02")
      sc_df <- label_statcast_imputed_data(statcast_df)
      mean(sc_df$imputed)

### **Generate run expectancy and related measures from Baseball Savant data**

      df <- statcast_search(start_date = "2016-04-06", end_date = "2016-04-15",
                            playerid = 621043, player_type = 'batter')
      run_expectancy_code(df, level = "plate appearances")

### **Generate linear weight values for events using Baseball Savant data**

      df <- statcast_search(start_date = "2016-04-06", end_date = "2016-04-15",
                            playerid = 621043, player_type = 'batter')
      df <- run_expectancy_code(df, level = "plate appearances")
      linear_weights_savant(df, level = "plate appearance")
