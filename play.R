
# EDA project: MLB data ---------------------------------------------------
# purpose: explore possible ideas for analysis

# load packages
library(tidyverse)
library(patchwork)

# load in data
batted_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/eda_projects/mlb_batted_balls_2022.csv")
head(batted_data)


# explore data ------------------------------------------------------------

summary(batted_data)                        
str(batted_data)
dim(batted_data)
class(batted_data)

# make tibble
Batting <- as_tibble(batted_data)
names(Batting)


# shifts ------------------------------------------------------------------

if_shift <- Batting %>% 
  ggplot(aes(x = if_fielding_alignment)) +
  geom_bar() +
  labs(title = "Infield Shift",
       x = "fielding alignment")
of_shift <- Batting %>% 
  ggplot(aes(x = of_fielding_alignment)) +
  geom_bar() +
  labs(title = "Outfield Shift",
       x = "fielding alignment")
if_shift + of_shift


# launch speed vs launch angle --------------------------------------------

Batting %>% 
  ggplot(aes(x = launch_angle, y = launch_speed)) +
  geom_point(alpha = 0.3) +
  geom_density2d() +
  coord_flip() +
  theme_bw()

# hit distance by batted ball type ----------------------------------------

Batting %>% 
  ggplot(aes(x = hit_distance_sc, y = bb_type)) +
  geom_col() +
  scale_x_continuous(labels = scales::comma) 

# coordinates colored by pitch type ---------------------------------------

Batting %>% 
  ggplot(aes(x = hc_x, y = hc_y, color = pitch_type)) +
  geom_point(alpha = 0.3)


# pitch type --------------------------------------------------------------

# overall pitch types
edit_pitch <- Batting %>% 
  filter(!is.na(pitch_type)) %>% 
  mutate(pitch_type = fct_recode(pitch_type, "Changeup" = "CH", 
                                 "Breaking ball" = "CU",
                                 "Changeup" = "EP",
                                 "Fastball" = "FA",
                                 "Fastball" = "FC", 
                                 "Fastball" = "FF", 
                                 "Fastball" = "FS",
                                 "Breaking ball" = "KC",  
                                 "Fastball" = "SI",  
                                 "Breaking ball" = "SL"))

# strikes
edit_pitch %>% 
  ggplot(aes(x = pitch_type)) +
  geom_bar(position = "dodge", aes(fill = factor(strikes))) +
  labs(title = "Strikes") +
  theme_bw() +
  theme(legend.position = "bottom")

# balls
edit_pitch %>% 
  ggplot(aes(x = pitch_type)) +
  geom_bar(position = "dodge", aes(fill = factor(balls))) +
  labs(title = "Balls") +
  theme_bw() +
  theme(legend.position = "bottom") 


# pitch type by count -----------------------------------------------------

# test 