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
  filter(!is.na(if_fielding_alignment)) %>% 
  ggplot(aes(x = if_fielding_alignment)) +
  geom_bar() +
  labs(title = "Infield Shift",
       x = "fielding alignment") +
  theme_bw()
of_shift <- Batting %>% 
  filter(!is.na(of_fielding_alignment)) %>% 
  ggplot(aes(x = of_fielding_alignment)) +
  geom_bar() +
  labs(title = "Outfield Shift",
       x = "fielding alignment") +
  theme_bw()

if_shift + of_shift


# launch speed vs launch angle --------------------------------------------

Batting %>% 
  ggplot(aes(x = launch_angle, y = launch_speed)) +
  geom_point(alpha = 0.25) +
  geom_density2d() +
  coord_flip() +
  theme_bw() +
  labs(title = "Launch Angle vs. Launch Speed", 
       x = "Launch Speed",
       y = "Launch Angle")
Batting %>% 
  ggplot(aes(x = launch_angle, y = launch_speed)) +
  stat_density2d(geom = "polygon", # use stat not geom
                 #contour = FALSE,
                 adjust = .75,
                 aes(fill = after_stat(level))) + 
  scale_fill_gradient(low = "darkblue", high = "darkorange",
                      labels = scales::comma) +
  theme_bw() +
  #theme(legend.position = "bottom") +
  coord_equal() +
  labs(title = "Launch Angle vs. Launch Speed", 
       x = "Launch Angle",
       y = "Launch Speed") 

delta_score %>% 
  ggplot(aes(factor(scored_rbi), launch_angle, fill = scored_rbi)) +
  scale_fill_viridis_b() +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Runs Scored", y = "Launch Angle") +
  theme(legend.position = "none")
delta_score %>% 
  ggplot(aes(factor(scored_rbi), launch_speed, fill = scored_rbi)) +
  scale_fill_viridis_b() +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Runs Scored", y = "Launch Speed") +
  theme(legend.position = "none")


# hit distance by batted ball type ----------------------------------------

Batting %>% 
  ggplot(aes(x = hit_distance_sc, y = bb_type)) +
  geom_col() +
  scale_x_continuous(labels = scales::comma) 

# coordinates colored by pitch type ---------------------------------------

Batting %>% 
  ggplot(aes(x = hc_x, y = hc_y, color = pitch_type)) +
  geom_point(alpha = 0.3)
edit_pitch %>% 
  ggplot(aes(x = hc_x, y = (hc_y * -1), color = pitch_type)) +
  geom_point(alpha = 0.3) +
  labs(x = "horizontal coordinate",
       y = "vertical coordinate",
       color = "Pitch Type") +
  facet_wrap(~ stand) +
  theme_bw()


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

# table of probability of each pitch type overall
edit_pitch %>% 
  group_by(pitch_type) %>% 
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) 


# table of pitch types overall --------------------------------------------


all <- edit_pitch %>% 
  group_by(pitch_type) %>% 
  summarize(n = n())

# 0-0
zero_zero <- edit_pitch %>% 
  filter(balls == 0) %>% 
  filter(strikes == 0) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "zero_zero")


# 0-1
zero_one <- edit_pitch %>% 
  filter(balls == 0) %>% 
  filter(strikes == 1) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "zero_one")

# 0-2
zero_two <- edit_pitch %>% 
  filter(balls == 0) %>% 
  filter(strikes == 2) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "zero_two")

# 1-0
one_zero <- edit_pitch %>% 
  filter(balls == 1) %>% 
  filter(strikes == 0) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "one_zero")

# 1-1
one_one <- edit_pitch %>% 
  filter(balls == 1) %>% 
  filter(strikes == 1) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "one_one")

# 1-2
one_two <- edit_pitch %>% 
  filter(balls == 1) %>% 
  filter(strikes == 2) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "one_two")

# 2-0
two_zero <- edit_pitch %>% 
  filter(balls == 2) %>% 
  filter(strikes == 0) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "two_zero")


# 2-1
two_one <- edit_pitch %>% 
  filter(balls == 2) %>% 
  filter(strikes == 1) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "two_one")

# 2-2
two_two <- edit_pitch %>% 
  filter(balls == 2) %>% 
  filter(strikes == 2) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "two_two")

# 3-0
three_zero <- edit_pitch %>% 
  filter(balls == 3) %>% 
  filter(strikes == 0) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "three_zero")

# 3-1
three_one <- edit_pitch %>% 
  filter(balls == 3) %>% 
  filter(strikes == 1) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "three_one")

# 3-2
three_two <- edit_pitch %>% 
  filter(balls == 3) %>% 
  filter(strikes == 2) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "three_two")

prop_count <- zero_zero %>% 
  rbind(zero_one, zero_two, one_zero, one_one, one_two, two_one, two_two, 
        two_zero, three_zero, three_one, three_two)


# write csv so all objects don't have to be loaded every time
write_csv(prop_count, "prop_count.csv")


# load data file of counts/props ------------------------------------------

library(readr)
prop_count <- read_csv("prop_count.csv")
View(prop_count)

# graph
prop_count %>% 
  ggplot(aes(x = pitch_type, y = prop, fill = count)) +
  geom_col(position = "dodge") +
  theme_bw() 


prop_count %>% 
  group_by(count)
table(prop_count$pitch_type)


# props by pitch type graphs ----------------------------------------------

changeup_props <- prop_count %>% 
  filter(pitch_type == "Changeup") %>% 
  mutate(count = fct_recode(count,
                            "0-0" = "zero_zero",
                            "0-1" = "zero_one",
                            "0-2" = "zero_two",
                            "1-0" = "one_zero", 
                            "1-1" = "one_one", 
                            "1-2" = "one_two",
                            "2-0" = "two_zero",
                            "2-1" = "two_one", 
                            "2-2" = "two_two",
                            "3-1" = "three_one", 
                            "3-2" = "three_two"))

breaking_props <- prop_count %>% 
  filter(pitch_type == "Breaking ball") %>% 
  mutate(count = fct_recode(count,
                            "0-0" = "zero_zero",
                            "0-1" = "zero_one",
                            "0-2" = "zero_two",
                            "1-0" = "one_zero", 
                            "1-1" = "one_one", 
                            "1-2" = "one_two",
                            "2-0" = "two_zero",
                            "2-1" = "two_one", 
                            "2-2" = "two_two",
                            "3-1" = "three_one", 
                            "3-2" = "three_two"))


fast_props <- prop_count %>% 
  filter(pitch_type == "Fastball") %>% 
  mutate(count = fct_recode(count,
                            "0-0" = "zero_zero",
                            "0-1" = "zero_one",
                            "0-2" = "zero_two",
                            "1-0" = "one_zero", 
                            "1-1" = "one_one", 
                            "1-2" = "one_two",
                            "2-0" = "two_zero",
                            "2-1" = "two_one", 
                            "2-2" = "two_two",
                            "3-0" = "three_zero", 
                            "3-1" = "three_one", 
                            "3-2" = "three_two"))


# checking to make sure 3-0 is only fastballs
try <- Batting %>% 
  filter(balls == 3) %>% 
  filter(strikes == 0) %>% 
  arrange(pitch_type)


counts <- c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2",
            "3-0", "3-1", "3-2")
counts2 <- c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2",
             "3-1", "3-2")

# graphs
changeup_props %>% 
  ggplot(aes(x = count, y = prop)) +
  theme_bw() +
  geom_col(fill= "#238A8DFF") +
  labs(title = "Changeup") +
  scale_x_discrete(limits = counts2)

breaking_props %>% 
  ggplot(aes(x = count, y = prop)) +
  theme_bw() +
  geom_col() +
  geom_col(fill= "#238A8DFF") +
  labs(title = "Breaking balls",
       y = "proportion") +
  scale_x_discrete(limits = counts2)

fast_props %>% 
  ggplot(aes(x = count, y = prop)) +
  theme_bw() +
  geom_col() +
  geom_col(fill= "#238A8DFF") +
  labs(title = "Fastball") +
  scale_x_discrete(limits = counts)

max(changeup_props$prop) - min(changeup_props$prop) # 0.1217507
max(breaking_props$prop) - min(breaking_props$prop) # 0.3158582
max(fast_props$prop) - min(fast_props$prop) # 0.5477707

summary(changeup_props$prop)
summary(breaking_props$prop)
summary(fast_props$prop)

# runs scored -------------------------------------------------------------

scoring <- Batting %>% 
  filter(post_home_score > home_score)
summary(scoring)

scoring %>% 
  select(hit_distance_sc, launch_speed, launch_angle, release_speed,
         effective_speed) %>% 
  summary()

Batting %>% 
  filter(post_home_score > home_score) %>% 
  ggplot(aes(x = hit_distance_sc, y = (post_home_score - home_score))) +
  geom_line() +
  coord_flip()


# scoring -----------------------------------------------------------------
#### NEW NEW NEW NEW ####

# new var

delta_home <- Batting %>% # ignore
  filter(post_home_score > home_score) %>% 
  mutate(runs_scored = post_home_score - home_score)
delta_score <- Batting %>% # ignore
  if(post_home_score > home_score){
    mutate(runs_scored = post_home_score - home_score)
  } else if(post_away_score > away_score){
    mutate(runs_scored = post_away_score - away_score)
  } else{
    mutate(runs_scored == 0)
  }
delta_score <- Batting %>% # ignore
  if(post_home_score >= home_score){
    mutate(runs_scored = post_home_score - home_score)} %>% 
  if(post_away_score > away_score){
    mutate(runs_scored = post_away_score - away_score)} 
  #if(post_home_score > home_score | post_away_score > away_score){
   # mutate(runs_scored == 0)
  #}
delta_score <- Batting %>% 
  mutate(scored_rbi = case_when(post_home_score != home_score ~ 
                                  post_home_score - home_score,
                                post_away_score != away_score ~ 
                                  post_away_score - away_score,
                                TRUE ~ as.numeric(0)))
delta_score$scored_rbi

delta_score %>% 
  ggplot(aes(x = launch_speed, y = hit_distance_sc, size = scored_rbi,
             color = effective_speed)) +
  geom_point(alpha = 0.3) +
  scale_color_gradient(low = "darkviolet", high = "darkturquoise") +
  labs(title = "Hit Data",
       x = "Launch Speed", y = "Hit Distance",
       color = "Effective Speed", size = "Runs Scored") +
  theme_bw() +
  theme(legend.position = "bottom")
delta_score %>% 
  ggplot(aes(x = hit_distance_sc, y = launch_angle, color = scored_rbi,
             size = scored_rbi)) +
  geom_point(alpha = 0.3) +
  scale_color_gradient(low = "steelblue3", high = "darkviolet") +
  theme_bw() +
  labs(x = "Hit Distance", y = "Launch Angle",
       color = "Runs Scored", size = "Runs Scored") +
  theme(legend.position = "bottom")
#### NEW NEW NEW NEW ####

# pitch type
delta_score2 <- delta_score
delta_score2 <- delta_score2 %>% 
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
delta_score2 %>% 
  select(pitch_type, scored_rbi) %>% 
  group_by(pitch_type, scored_rbi) %>% 
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(x = scored_rbi, y = prop)) +
  geom_col() +
  facet_grid(scored_rbi ~ pitch_type)

# counts of pitch types by number of balls
edit_pitch %>% 
  ggplot(aes(x = pitch_type, fill = factor(balls))) +
  geom_bar(position = "dodge")

# data table --------------------------------------------------------------


Batting %>% 
  select(player_name, stand, events, release_speed, bb_type, 
         hit_distance_sc) %>% 
  head(5) %>% 
  knitr::kable()
