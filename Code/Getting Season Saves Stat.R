library(tidyverse)
season_saves_stat <- woba_save %>% 
  #filter(game_type == 'R') %>% 
  mutate(LI_diff = LI_diff - mean(LI_diff, na.rm = TRUE)) %>% 
  group_by(pitcher, player_name, game_year) %>% 
  reframe(
    batters_faced = n(),
    #batters_faced_7th_or_later = sum(inning >= 7),
    new_saves = sum(LI_diff, na.rm = TRUE),
    new_saves_bf = new_saves/batters_faced
  ) %>% 
  arrange(desc(game_year), desc(new_saves)) %>% 
  filter(batters_faced > 100)

season_saves_stat <- season_saves_stat %>% 
  group_by(game_year) %>% 
  mutate(rank = row_number()) %>% 
  ungroup()

season_saves_stat %>% 
  filter(player_name == "McFarland, T.J.")


