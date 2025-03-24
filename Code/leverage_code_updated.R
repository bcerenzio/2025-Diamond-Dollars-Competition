library(tidyverse)
library(arrow)
library(stargazer)

woba_save <- read_csv('win_exp_data.csv')
save_run_exp_pitcher_abs <- read_csv('save_run_exp_pitcher_abs.csv')

new_save_run_exp_pitcher_abs <- save_run_exp_pitcher_abs %>% 
  select(game_pk, at_bat_number, pitch_number, run_expectancy_saves)

woba_save <- new_save_run_exp_pitcher_abs %>% 
  left_join(woba_save, by = c('game_pk','at_bat_number','pitch_number'))


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
