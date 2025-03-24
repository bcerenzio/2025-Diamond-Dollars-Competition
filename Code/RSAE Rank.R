library(tidyverse)

  appearance_data_5_95 <- appearance_data_5_95 %>% 
  arrange(desc(game_year), desc(expected_rapo_season)) %>% 
  group_by(game_year) %>% 
  mutate(rank = row_number()) %>% 
  ungroup()
  


fangraphs_rank <- fangraphs %>% 
  filter(Season == 2024, IP > 17) %>% 
  arrange(FIP) %>% 
  mutate(rank = row_number())
