
 library(arrow)
 library(tidyverse)
library(dplyr)

statcast <- read_parquet("C:/Users/Aidan/Downloads/statcast_data_w_WinExp.parquet")

#create new delta win exp column
#group by game pk and Inning top bottom
# lead function to subtract current win exp from next win exp
#might lead into na values but check with brett at that point

statcast <- read_parquet('statcast_data_w_WinExp.parquet')

statcast_new <- statcast %>%
  group_by(game_pk, inning_topbot) %>%
  mutate(
    new_delta_win_exp = lead(updated_pitcher_win_exp, 1) - updated_pitcher_win_exp
  ) %>%
  ungroup()

# statcast_small <- statcast_new %>% 
#   select(game_pk, inning, inning_topbot, updated_pitcher_win_exp, new_delta_win_exp)

statcast_last <- statcast_new %>%
  group_by(game_pk) %>% 
  mutate(
    new_delta_win_exp = case_when(
      max(at_bat_number) == at_bat_number & (post_fld_score > post_bat_score) ~ 1 - updated_pitcher_win_exp, 
      max(at_bat_number) == at_bat_number & (post_fld_score < post_bat_score) ~ 0 - updated_pitcher_win_exp,
      max(at_bat_number) == at_bat_number & str_detect(des, 'balk') ~ 0 - updated_pitcher_win_exp,
      (max(at_bat_number) != at_bat_number) & is.na(new_delta_win_exp) ~ (1 - lead(updated_pitcher_win_exp,1)) - updated_pitcher_win_exp,
      .default = new_delta_win_exp
      )
  ) %>% ungroup()

write_csv(statcast_last, 'delta_last_full2.csv')

# come back to fix NAs

statcast_last_small <- statcast_last %>% 
  select(game_pk, inning, inning_topbot, updated_pitcher_win_exp, new_delta_win_exp)



# write.csv(statcast_last_small, "D:/RStudio/Projects/Arizona2025PC/2_delta_test.csv")
# 
# 
# statcast_1 <- statcast_new %>%
#   group_by(game_pk) %>%
#   filter(last(as.numeric(at_bat_number))) %>%
#   ungroup()
# 
# statcast_1 <- statcast_new %>%
#   group_by(game_pk) %>%
#   filter(at_bat_number == max(as.numeric(at_bat_number))) %>%
#   ungroup()


