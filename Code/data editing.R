library(tidyverse)
library(arrow)

pitcher_abs <- read_parquet("pitcher_abs.parquet")

write_parquet(pitcher_abs %>% distinct(), 'pitcher_abs.parquet')

pitcher_abs <- pitcher_abs %>% 
  mutate(runs_scored = post_bat_score - bat_score)

pitcher_abs <- pitcher_abs %>% 
  mutate(is_400plus_feet = ifelse(hit_distance_sc < 400 | is.na(hit_distance_sc), 0,1))

glimpse(pitcher_abs)


statcast_all <- read_parquet('statcast_21_24_pitching.parquet')

write_parquet(statcast_all %>% distinct(), 'statcast_21_24_pitching.parquet')
