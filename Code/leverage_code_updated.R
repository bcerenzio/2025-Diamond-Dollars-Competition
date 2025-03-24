library(tidyverse)
library(arrow)
library(stargazer)
# 
# setwd("~/Desktop/Syracuse/Semesters/Spring 2025/Arizona/Diamond Dollars")
# 
# statcast <- read_csv('delta_last_full.csv')
# woba_data <- read_csv('weighted_woba.csv')
# save_run_exp_pitcher_abs <- read_csv('save_run_exp_pitcher_abs.csv')
# 
# statcast_new <- statcast %>% 
#   mutate(game_year = as.integer(substr(game_date, 1, 4)),  #taking year from gamedate
#          pitcher_win_exp = 1 - bat_win_exp, 
#          pitcher_score_diff = bat_score_diff * -1, 
#          is_home_team = ifelse(inning_topbot == "Top", 1, 0),  #home team indicator
#          base_state = case_when(
#            !is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ "321",
#            !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ "32_",   
#            !is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ "3_1",   
#            is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ "32_",   
#            is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ "3__",    
#            is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ "_2_",    
#            !is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ "__1",    
#            TRUE ~ "___"  
#          ) %>% factor())  #make categorical
# 
# #combine woba and statcast
# full_data <- statcast_new %>%
#   left_join(woba_data, by = c("batter", "game_year"))
# 
# delta_model_data <- full_data %>%
#   select(
#     new_delta_win_exp, is_home_team, pitcher_score_diff, inning, 
#     outs_when_up, base_state, wOBA
#   )
# 
# #---- Win Expectancy Regression ----##
# 
# 
# deltaWinExpLM <- lm(new_delta_win_exp ~ ., data = delta_model_data)
# summary(deltaWinExpLM)
# 
# deltaAbsWinExpLM <- lm(abs(new_delta_win_exp) ~ ., data = delta_model_data)
# summary(deltaAbsWinExpLM)



#Zach's edits/additions to Aidens model#
save_run_exp_pitcher_abs %>%  mutate(game_year = as.integer(substr(game_date, 1, 4)),  #taking year from gamedate
                                     pitcher_win_exp = 1 - bat_win_exp,
                                     pitcher_score_diff = bat_score_diff * -1,
                                     is_home_team = ifelse(inning_topbot == "Top", 1, 0),  #home team indicator
                                     base_state = case_when(
                                       !is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ "321",
                                       !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ "32_",
                                       !is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ "3_1",
                                       is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ "32_",
                                       is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ "3__",
                                       is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ "_2_",
                                       !is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ "__1",
                                       TRUE ~ "___"
                                     ) %>% factor()) -> save_run_exp_pitcher_abs

#predict(deltaAbsWinExpLM, woba_data) -> woba_data$ab_win_expect

save_run_exp_pitcher_abs %>% left_join(woba_data, by = c("batter", "game_year")) -> woba_save

#write_csv(woba_save, 'win_expect_data.csv')

predict(deltaAbsWinExpLM, woba_save) -> woba_save$ab_win_expect

woba_save <- read_csv('win_exp_data.csv')
save_run_exp_pitcher_abs <- read_csv('save_run_exp_pitcher_abs.csv')

new_save_run_exp_pitcher_abs <- save_run_exp_pitcher_abs %>% 
  select(game_pk, at_bat_number, pitch_number, run_expectancy_saves)

woba_save <- new_save_run_exp_pitcher_abs %>% 
  left_join(woba_save, by = c('game_pk','at_bat_number','pitch_number'))

# woba_save <- new_save_run_exp_pitcher_abs %>% 
#   left_join(statcast_last, by = c('game_pk', 'at_bat_number','pitch_number'))


summary(abs(woba_save$ab_win_expect))

mean(abs(woba_save$ab_win_expect), na.rm = TRUE) -> mean_win_expect

woba_save <- woba_save %>% mutate(leverage = abs(ab_win_expect)/abs(mean_win_expect))

mean(woba_save$leverage, na.rm = TRUE) -> mean_leverage

average_leverage <- woba_save %>% 
  filter(bat_score_diff == -1) %>% 
  group_by(inning) %>% 
  reframe(leverage = mean(leverage, na.rm = TRUE))

test_woba_save <- woba_save %>% mutate(LI_diff = if_else((bat_score - fld_score) == lead(bat_score - fld_score),
      leverage - lead(leverage), -abs((leverage - lead(leverage)) * run_expectancy_saves)))


#woba_save <- woba_save %>% 
#  mutate(LI_diff = case_when((bat_score - fld_score) == (post_bat_score - post_fld_score) && inning < 9 ~ 
#                               leverage - lead(leverage), 
 #                            (bat_score - fld_score) != (post_bat_score - post_fld_score) && inning < 9 ~ 
  #         -abs((leverage - lead(leverage)) * run_expectancy_saves), 
   #        home_score < away_score && group_by(game_pk) %>% max(at_bat_number, inning) ~ abs(leverage - 0),
    #     ))

woba_save <- woba_save %>% 
  mutate(is_home_team = ifelse(inning_topbot == "Top", 1, 0),)

woba_save <- woba_save %>% 
  mutate(base_sit = case_when(
    !is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ "321",
    !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ "32_",
    !is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ "3_1",
    is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ "32_",
    is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ "3__",
    is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ "_2_",
    !is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ "__1",
    TRUE ~ "___"
  ) %>% factor())

woba_save <- woba_save %>%
  arrange(game_pk, at_bat_number, pitch_number) %>% 
  mutate(
    max_runs_possible = case_when(
      base_sit == '___' ~ 1,
      base_sit == '321' ~ 4,
      base_sit %in% c('__1', '_2_', '3__') ~ 2,
      .default = 3
    ),
    run_expectancy_saves = case_when(
      run_expectancy_saves < 0 ~ 0,
      run_expectancy_saves > 4 ~ 4,
      base_sit == '321' & events %in% c('walk', 'hit_by_pitch') ~ 1,
      .default = run_expectancy_saves
    ),
    
    LI_diff = case_when(
      (bat_score - fld_score) == lead(bat_score - fld_score) & at_bat_number != max(at_bat_number, na.rm = TRUE) ~ 
        (leverage - lead(leverage))*(max_runs_possible - run_expectancy_saves),
      
      (bat_score - fld_score) != lead(bat_score - fld_score) & at_bat_number != max(at_bat_number, na.rm = TRUE) ~ 
        -abs((leverage - lead(leverage)) * run_expectancy_saves),
      
      post_home_score > post_away_score & 
        at_bat_number == max(at_bat_number, na.rm = TRUE) & 
        inning >= 9 &
        is_home_team == 1 ~ 
        (leverage - 0)*(max_runs_possible - run_expectancy_saves),
      
      at_bat_number == max(at_bat_number[is_home_team == 1], na.rm = TRUE) & 
        max(at_bat_number[is_home_team == 1], na.rm = TRUE) < max(at_bat_number, na.rm = TRUE) &
        inning >= 9 &
        is_home_team == 1 &
        abs(bat_score_diff) < 3 ~ 
        (leverage - lead(leverage))*(max_runs_possible - run_expectancy_saves),
      
      at_bat_number == max(at_bat_number[is_home_team == 0], na.rm = TRUE) & 
        max(at_bat_number[is_home_team == 0], na.rm = TRUE) < max(at_bat_number, na.rm = TRUE) &
        inning == 8 &
        is_home_team == 0  &
        abs(bat_score_diff) < 3 ~ 
        (leverage - lead(leverage))*(max_runs_possible - run_expectancy_saves),
      
      post_away_score > post_home_score & 
        at_bat_number == max(at_bat_number, na.rm = TRUE) & 
        inning >= 9 &
        is_home_team == 0 ~ 
        (leverage - 0)*(max_runs_possible - run_expectancy_saves),
      
      post_home_score > post_away_score & 
        at_bat_number == max(at_bat_number, na.rm = TRUE) & 
        inning >= 9 &
        is_home_team == 0 ~ 
        -abs((0 - leverage) * run_expectancy_saves),
    )
  )

View(woba_save)

write_csv(woba_save, 'leverage_diff2.csv')
