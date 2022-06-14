# load in data
batted_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/eda_projects/mlb_batted_balls_2022.csv")
head(batted_data)

# load packages
library(tidyverse)
library(patchwork)

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
  geom_bar()
of_shift <- Batting %>% 
  ggplot(aes(x = of_fielding_alignment)) +
  geom_bar()
if_shift + of_shift


# launch speed vs launch angle --------------------------------------------

Batting %>% 
  ggplot(aes(x = launch_angle, y = launch_speed)) +
  geom_point(alpha = 0.3) +
  geom_density2d() +
  coord_flip() +
  theme_bw()

# ignore
Batting %>% 
  ggplot(aes(x = events)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Batting %>% 
  ggplot(aes(x = pitch_type)) +
  geom_bar()
Batting %>% 
  ggplot(aes(x = pitch_type, color = events)) +
  geom_point(stat = "count")

# hit distance by batted ball type ----------------------------------------

Batting %>% 
  ggplot(aes(x = hit_distance_sc, y = bb_type)) +
  geom_col() +
  scale_x_continuous(labels = scales::comma)