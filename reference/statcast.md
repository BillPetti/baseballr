# **Statcast Functions Overview**

- [`statcast_search()`](https://billpetti.github.io/baseballr/reference/statcast_search.md):
  Query Statcast by Date Range and Players.

- [`statcast_search_batters()`](https://billpetti.github.io/baseballr/reference/statcast_search.md):
  Query Statcast Batters by Date Range and Player.

- [`statcast_search_pitchers()`](https://billpetti.github.io/baseballr/reference/statcast_search.md):
  Query Statcast Pitchers by Date Range and Player.

- [`statcast_leaderboards()`](https://billpetti.github.io/baseballr/reference/statcast_leaderboards.md):
  Query Baseball Savant Leaderboards.

## Details

### **Query Statcast Batters by Date Range**

    statcast_search(start_date = "2016-04-06",
                    end_date = "2016-04-15",
                    player_type = 'batter')

     ## The above is equivalent to:
    statcast_search_batters(start_date = "2016-04-06",
                            end_date = "2016-04-15",
                            batterid = NULL)

### **Query Statcast Pitchers by Date Range**

    statcast_search(start_date = "2016-04-06",
                    end_date = "2016-04-15",
                    player_type = 'pitcher')

     ## The above is equivalent to:
    statcast_search_pitchers(start_date = "2016-04-06",
                             end_date = "2016-04-15",
                             pitcherid = NULL)

### **Query Statcast Batters by Date Range and Player ID**

      correa <- statcast_search(start_date = "2016-04-06",
                                end_date = "2016-04-15",
                                playerid = 621043,
                                player_type = 'batter')

     ## The above is equivalent to:
      correa <- statcast_search_batters(start_date = "2016-04-06",
                                        end_date = "2016-04-15",
                                        batterid = 621043)

### **Query Statcast Pitchers by Date Range and Player ID**

      noah <- statcast_search(start_date = "2016-04-06",
                              end_date = "2016-04-15",
                              playerid = 592789,
                              player_type = 'pitcher')

     ## The above is equivalent to:
      noah <- statcast_search_pitchers(start_date = "2016-04-06",
                                      end_date = "2016-04-15",
                                      pitcherid = 592789)

### **Query Baseball Savant Leaderboards**

      statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = 2021)
