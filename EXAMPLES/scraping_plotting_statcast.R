##### Bill Petti
##### 8-27-2016
##### In this example, the baseballr package is used to acquire Statcast data for Mookie Betts from 2015-2016
##### The data is then processed and plotted to show how his launch angle and batted ball speed have changed from year to year

# load required packages

require(devtools)
install_github("BillPetti/baseballr")
require(baseballr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(zoo)

# load custom ggplot2 theme

source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

# find Mookie Betts' MLBAMID

betts_id <- playerid_lookup("Betts") %>%
  filter(first_name == "Mookie") %>%
  select(mlbam_id, first_name, last_name)

# scrape Betts' Statcast data, by pitch removing those with a batted ball speed of 0

betts <- scrape_statcast_savant_batter("2015-03-31", "2016-08-26", betts_id[1,1]) %>%
  mutate(Year = as.factor(substr(game_date,1,4))) %>%
  filter(type == "X") %>%
  filter(hit_speed != 0)

# calculate average launch angles and batted ball speeds by game

betts_grpd <- betts %>%
  group_by(game_date) %>%
  summarise(`Average Launch Angle` = mean(hit_angle, na.rm = TRUE), `Average Batted Ball Speed` = mean(hit_speed, na.rm = TRUE)) %>%
  ungroup() %>%
  melt(id=c("game_date")) %>%
  mutate(Year = as.factor(substr(game_date,1,4)))

# calculate Betts' average launch angle and batted ball speed by year

betts_avg_speed_yr <- betts %>%
  group_by(Year) %>%
  summarise(speed = round(mean(hit_speed, na.rm = TRUE),1), angle = round(mean(hit_angle, na.rm = TRUE),1))

# plot the data

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
  theme_bp_grey() + 
  theme(legend.position = "bottom", strip.text.x = element_text(face = "bold", size = 14), plot.subtitle = element_text(hjust=-.12)) +
  scale_color_manual(values = c("#5F9ED1", "#FF800E"))

# export plot to your working directory

ggsave("betts_angle_speed_year.png", scale = 1.2, width = 14, height = 8.5, units = "in")
