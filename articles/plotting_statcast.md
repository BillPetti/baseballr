# Plotting Statcast data

In this example, the `baseballr` package is used to acquire Statcast
data for Mookie Betts from 2015-2016.

The data is then processed and plotted to show how his launch angle and
batted ball speed have changed from year to year

``` r

library(baseballr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(zoo)
```

### find Mookie Betts’ MLBAMID

``` r

betts_id <- playerid_lookup("Betts") %>%
  dplyr::filter(first_name == "Mookie") %>%
  dplyr::select(mlbam_id, first_name, last_name)
```

### scrape Betts’ Statcast data, by pitch removing those with a batted ball speed of 0

``` r

betts_15 <- statcast_search("2015-03-31", "2015-10-31", playerid = betts_id[1,1], player_type = 'batter')
betts_16 <- statcast_search("2016-03-31", "2016-10-31", playerid = betts_id[1,1], player_type = 'batter')   
betts <- dplyr::bind_rows(betts_15, betts_16) %>%
  dplyr::mutate(Year = as.factor(substr(game_date,1,4))) %>%
  dplyr::filter(type == "X") %>%
  dplyr::filter(launch_speed != 0)
```

### calculate average launch angles and batted ball speeds by game

``` r

betts_grpd <- betts %>%
  dplyr::group_by(game_date) %>%
  dplyr::summarise(
    `Average Launch Angle` = mean(launch_angle, na.rm = TRUE), 
    `Average Batted Ball Speed` = mean(launch_speed, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  melt(id=c("game_date")) %>%
  dplyr::mutate(Year = as.factor(substr(game_date,1,4)))
```

### calculate Betts’ average launch angle and batted ball speed by year

``` r

betts_avg_speed_yr <- betts %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    speed = round(mean(launch_speed, na.rm = TRUE),1), 
    angle = round(mean(launch_angle, na.rm = TRUE),1))
```

### plot the data

``` r

betts_grpd %>%
  ggplot(aes(game_date, value)) +
  geom_point() +
  stat_smooth(aes(group = Year, color = Year)) +
  facet_wrap(~variable, scales = "free_y") + 
  ggtitle("\nMookie Betts: 2015 vs. 2016\n") + 
  labs(subtitle = paste0("Betts has lowered his launch angle in 2016, from ", betts_avg_speed_yr[1,3], " degrees in 2015 to ", betts_avg_speed_yr[2,3], " degrees this year.\n\n"), 
       caption = "@BillPetti\nData from baseballsavant.mlb.com\nData acquired with the baseballr package") +
  ylab("Angle = Degrees, Speed = MPH\n") +
  xlab("\nDate") +
  #theme_bp_grey() + 
  theme(legend.position = "bottom", strip.text.x = element_text(face = "bold", size = 14), plot.subtitle = element_text(hjust=-.12)) +
  scale_color_manual(values = c("#5F9ED1", "#FF800E"))
```

### Uncomment and run to export plot to your working directory

``` r

# ggsave("betts_angle_speed_year.png", scale = 1.2, width = 14, height = 8.5, units = "in")
```

### Coloring by pitch type

[`statcast_pitch_colors()`](https://billpetti.github.io/baseballr/reference/statcast_pitch_colors.md)
returns Baseball Savant’s own pitch-type palette, so plots colored by
`pitch_type` match what readers see on Savant:

``` r

pal <- statcast_pitch_colors()
pitch_pal <- setNames(pal$color, pal$pitch_type)

betts_16 |>
  dplyr::filter(!is.na(pitch_type)) |>
  ggplot(aes(x = plate_x, y = plate_z, color = pitch_type)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = pitch_pal) +
  coord_fixed() +
  theme_bw()
```

### Pitch locations with the strike zone

[`ggpitchzone()`](https://billpetti.github.io/baseballr/reference/ggpitchzone.md)
plots plate-crossing locations from the catcher’s perspective with the
batter’s strike zone overlaid, using the Savant palette by default:

``` r

ggpitchzone(betts_16)
```

## **Our Authors**

- [Bill Petti](https://x.com/BillPetti)
  [![@BillPetti](https://img.shields.io/twitter/follow/BillPetti?color=blue&label=%40BillPetti&logo=x&style=for-the-badge)](https://x.com/BillPetti)
  [![@BillPetti](https://img.shields.io/github/followers/BillPetti?color=eee&logo=Github&style=for-the-badge)](https://github.com/BillPetti)
- [Saiem Gilani](https://x.com/saiemgilani)
  [![@saiemgilani](https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=x&style=for-the-badge)](https://x.com/saiemgilani)
  [![@saiemgilani](https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge)](https://github.com/saiemgilani)

### **Our Contributors**

- [Ben Baumer](https://x.com/BaumerBen)
  [![@BaumerBen](https://img.shields.io/twitter/follow/BaumerBen?color=blue&label=%40BaumerBen&logo=x&style=for-the-badge)](https://x.com/BaumerBen)
  [![@beanumber](https://img.shields.io/github/followers/beanumber?color=eee&logo=Github&style=for-the-badge)](https://github.com/beanumber)
- [Ben Dilday](https://x.com/BenDilday)
  [![@BenDilday](https://img.shields.io/twitter/follow/BenDilday?color=blue&label=%40BenDilday&logo=x&style=for-the-badge)](https://x.com/BenDilday)
  [![@bdilday](https://img.shields.io/github/followers/bdilday?color=eee&logo=Github&style=for-the-badge)](https://github.com/bdilday)
- [Robert Frey](https://x.com/RobertFrey40)
  [![@RobertFrey40](https://img.shields.io/twitter/follow/RobertFrey40?color=blue&label=%40RobertFrey40&logo=x&style=for-the-badge)](https://x.com/RobertFrey40)
  [![@robert-frey](https://img.shields.io/github/followers/robert-frey?color=eee&logo=Github&style=for-the-badge)](https://github.com/robert-frey)
- [Camden Kay](https://x.com/k_camden)
  [![@k_camden](https://img.shields.io/twitter/follow/k_camden?color=blue&label=%40k_camden&logo=x&style=for-the-badge)](https://x.com/k_camden)
  [![@camdenk](https://img.shields.io/github/followers/camdenk?color=eee&logo=Github&style=for-the-badge)](https://github.com/camdenk)

### **Citation**

To cite the [**`baseballr`**](https://baseballr.sportsdataverse.org/) R
package in publications, use:

BibTeX Citation

``` bibtex
@misc{baseballr,
  author = {Bill Petti and Saiem Gilani},
  title = {baseballr: An R Package for Baseball Data Acquisition and Analysis},
  url = {https://baseballr.sportsdataverse.org/},
  year = {2026}
}
```

### **Related SportsDataverse packages**

- [**cfbfastR**](https://cfbfastR.sportsdataverse.org/) - college
  football
- [**hoopR**](https://hoopR.sportsdataverse.org/) - men’s basketball
- [**wehoop**](https://wehoop.sportsdataverse.org/) - women’s basketball
- [**baseballr**](https://baseballr.sportsdataverse.org/) - baseball
- [**fastRhockey**](https://fastRhockey.sportsdataverse.org/) - hockey
- [**oddsapiR**](https://oddsapiR.sportsdataverse.org/) - betting odds
- [**sportyR**](https://sportyR.sportsdataverse.org/) - playing surfaces
- [**sportsdataverse-py**](https://py.sportsdataverse.org/) - the Python
  package
- [**sportsdataverse-R**](https://r.sportsdataverse.org/) - the R
  meta-package
