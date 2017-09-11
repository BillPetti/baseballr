---
layout: page
title: Updated for baseballr 0.3.3
tags: rstats, web-scraping, baseballr, statcast	
---

The latest release of the [`baseballr`](https://billpetti.github.io/baseballr/) package for `R` includes a number of enhancements and bug fixes.

In terms of new functions, `statline_from_statcast` allows users to take raw pitch-by-pitch data from Statcast/PITCHf/x and calculate aggregated, statline-like output. Examples include count data such as number of singles, doubles, etc., as well as rate metrics like Slugging and wOBA on swings or contact.

The function only has two arguments:

* `df`: a dataframe that includes pitch-by-pitch information. The function assumes the following columns are present: `events`, `description`, `game_date`, and `type`.
* `base`: base indicates what the denomincator should be for the rate stats that are calculated. The function defaults to "swings", but you can also choose to use "contact"

Here is an example using all data from the week of 2017-09-04. Here, we want to see a statline for all hitters based on swings:

```r
test <- scrape_statcast_savant_batter_all("2017-09-04", "2017-09-10")

statline_from_statcast(test)

year swings batted_balls  X1B X2B X3B  HR swing_and_miss swinging_strike_percent    ba
1 2017  13790        10663 1129 352  37 259           3127                   0.227 0.129

obp   slg   ops  woba
1 0.129 0.216 0.345 0.144
```

You can also combine the `statline_from_statcast` function with a loop to create statlines for multiple players at once.

Example: calculate statlines for batters on contact for all games played 2017-09-04 through 2017-09-10:

```r
test <- scrape_statcast_savant_batter_all("2017-09-04", "2017-09-10")

output <- data.frame()

for (i in c("Jose Ramirez", "J.D. Martinez", "Francisco Lindor", "Gary Sanchez", "Rhys Hoskins")) {
  reduced_test <- test %>%
    filter(player_name == i)
  x <- statline_from_statcast(reduced_test, base = "contact")
  x$player <- i
  x <- x %>%
    select(player, everything())
  output <- rbind(output, x) %>%
    arrange(desc(woba))
}

print(output, width = Inf)

# A tibble: 5 x 12
            player  year batted_balls   X1B   X2B   X3B    HR    ba   obp   slg   ops  woba
             <chr> <chr>        <dbl> <dbl> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <dbl> <dbl>
1    J.D. Martinez  2017           17     4     1     0     7 0.706 0.706 2.000 2.706 1.092
2     Gary Sanchez  2017           11     3     1     0     2 0.545 0.545 1.182 1.727 0.710
3 Francisco Lindor  2017           27     4     2     1     3 0.370 0.370 0.852 1.222 0.498
4     Rhys Hoskins  2017           14     2     1     0     2 0.357 0.357 0.857 1.214 0.495
5     Jose Ramirez  2017           16     0     0     0     3 0.188 0.188 0.750 0.938 0.370
```
