# library(tidyverse)
# library(arrow)
# library(stargazer)

 statcast <- read_csv('D:/RStudio/Projects/Arizona2025PC/delta_last_full.csv')
 woba_data <- read_csv('D:/RStudio/Projects/Arizona2025PC/weighted_woba.csv')

statcast_new <- statcast %>% 
  mutate(game_year = as.integer(substr(game_date, 1, 4)),  #taking year from gamedate
         pitcher_win_exp = 1 - bat_win_exp, 
         pitcher_score_diff = bat_score_diff * -1, 
         is_home_team = ifelse(inning_topbot == "Bot", 1, 0),  #home team indicator
         base_state = case_when(
           !is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ "321",
           !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ "32_",   
           !is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ "3_1",   
           is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ "32_",   
           is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ "3__",    
           is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ "_2_",    
           !is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ "__1",    
           TRUE ~ "___"  
         ) %>% factor())  #make categorical

#combine woba and statcast
full_data <- statcast_new %>%
  left_join(woba_data, by = c("batter", "game_year"))

delta_model_data <- full_data %>%
  select(
    new_delta_win_exp, is_home_team, pitcher_score_diff, inning, 
    outs_when_up, base_state, wOBA
  )

#---- Win Expectancy Regression ----##


deltaWinExpLM <- lm(new_delta_win_exp ~ ., data = delta_model_data)
summary(deltaWinExpLM)

deltaAbsWinExpLM <- lm(abs(new_delta_win_exp) ~ ., data = delta_model_data)
summary(deltaAbsWinExpLM)




