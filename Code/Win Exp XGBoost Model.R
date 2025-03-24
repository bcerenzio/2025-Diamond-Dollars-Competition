library(xgboost)
library(tidyverse)
library(arrow)
library(rsample)
library(fastDummies)

woba_by_player <- read_csv('batter_woba.csv')
statcast_all <- read_parquet('statcast_21_24_pitching.parquet')

#finding the final play of each game
final_play_of_game <- statcast_all %>% 
  group_by(game_pk) %>% 
  slice_max(at_bat_number, n = 1) %>% 
  slice_max(pitch_number, n = 1) %>% 
  ungroup() %>% 
  distinct()

# finding if the home team won
final_play_of_game <- final_play_of_game %>% 
  select(home_score, away_score, game_pk) %>% 
  mutate(home_win = ifelse(home_score > away_score, 1,0))

statcast_all <- statcast_all %>% 
  left_join(final_play_of_game %>% select(game_pk, home_win), by = 'game_pk')

# creating a dummy variable if the pitching team won
statcast_all <- statcast_all %>% 
  mutate(pitching_win = case_when(
    (home_win == 1 & inning_topbot == 'Top') | (home_win == 0 & inning_topbot == 'Bot') ~ 1,
    (home_win == 1 & inning_topbot == 'Bot') | (home_win == 0 & inning_topbot == 'Top') ~ 0
  ))


# creating base situations
statcast_all <- statcast_all %>% 
  mutate(base_sit = case_when(
    is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ '___',
    !is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ '__1',
    is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ '_2_',
    is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ '3__',
    !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ '_21',
    is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ '32_',
    !is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ '3_1',
    !is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ '321',
    .default = NA, # check to see if we're missing any base situations
  )) %>% 
  relocate(base_sit, .after = 'on_3b')


# joining batter wobas to the statcast table
statcast_all <- statcast_all %>% 
  left_join(woba_by_player %>% rename('batter_season_wOBA' = 'wOBA'), by = c('batter', 'game_year'))

# finding if pitcher is on the home or away team
statcast_all <- statcast_all %>% 
  mutate(on_home_team = ifelse(inning_topbot == 'Top', 1,0)) # Note: home pitchers pitch in the top of the inning


win_exp_df <- statcast_all %>% 
  group_by(game_pk, at_bat_number) %>% 
  slice_max(pitch_number, n = 1) %>% 
  ungroup() %>% # above code converts data to at-bat level
  distinct() %>% 
  reframe(
    pitching_win,
    on_home_team,
    batter_season_wOBA,
    pitcher_score_diff = fld_score - bat_score,
    abs_scor_diff = abs(fld_score - bat_score),
    total_runs_scored = bat_score + fld_score,
    base_sit = as.factor(base_sit),
    inning = ifelse(inning > 9, 10, inning), #treating all extra innings as the same (sudden death)
    outs_when_up,
    inning_topbot,
    tying_winning_run = case_when(
      pitcher_score_diff > 4 ~ 0,
      base_sit %in% c('321') & pitcher_score_diff <= 4  & pitcher_score_diff >= 0 ~ 1,
      base_sit %in% c('32_', '3_1','_21') & pitcher_score_diff <= 3 & pitcher_score_diff >= 0 ~ 1,
      base_sit %in% c('___') & pitcher_score_diff <= 1 & pitcher_score_diff >= 0 ~ 1,
      base_sit %in% c('__1','_2_','3__') & pitcher_score_diff <= 2 & pitcher_score_diff >= 0 ~ 1,
      .default = 0
    )
  ) %>% 
  dummy_cols(select_columns = 'base_sit', remove_most_frequent_dummy = TRUE,
             remove_selected_columns = TRUE)
  

win_exp_df <- win_exp_df %>% 
  drop_na(pitching_win) %>% 
  mutate(
    walkoff_possibilty = ifelse(inning_topbot == 'Bot' &  inning >= 9 & tying_winning_run == 1, 1,0)
  ) %>% 
  select(-inning_topbot)



set.seed(101);win_split <- initial_split(win_exp_df, prop = 0.65, strata = pitching_win)

train_win <- training(win_split)
test_win <- testing(win_split)

dtrain_win <- xgb.DMatrix(as.matrix(train_win %>% 
                                        select(-pitching_win)), label = train_win$pitching_win)


dtest_win <- xgb.DMatrix(as.matrix(test_win %>%
                                       select(-pitching_win)), label = test_win$pitching_win)


hyperparam_win_tuning_reg <- function(max_depth, weight,subsample, row_num){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.train(
    params = list(
      eta = 0.3,
      objective = 'binary:logistic',
      eval_metric = 'logloss', 
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample),
    data = dtrain_win,
    watchlist = list(train = dtrain_win, test = dtest_win),
    nrounds = 1000,
    early_stopping_rounds = 50,
    print_every_n = 50,
    nthread = 7
  ) 
  
  logloss <- mod$evaluation_log %>% 
    slice_min(test_logloss, n = 1) %>% 
    pull(test_logloss)
  
  nrounds <- mod$evaluation_log %>% 
    slice_min(test_logloss, n = 1) %>% 
    pull(iter)
  
  df <- tibble(
    logloss = logloss,
    nrounds = nrounds
  )
}

reg_tuning_win_df <- expand_grid(
  max_depth = c(3,4,6,8),
  weight = c(1,4,8,12, 16),
  subsample = c(0.5, 0.75,1)
) %>% 
  mutate(row_num = row_number())

win_rmse_nrounds_df <- pmap_df(list(reg_tuning_win_df$max_depth, reg_tuning_win_df$weight, reg_tuning_win_df$subsample, reg_tuning_win_df$row_num), hyperparam_win_tuning_reg)


reg_tuning_win_df$logloss <- win_rmse_nrounds_df$logloss; reg_tuning_win_df$nrounds <- win_rmse_nrounds_df$nrounds


reg_tuning_win_df %>% 
  arrange(logloss) %>% 
  head(5)



reg_tuning_win_best <- reg_tuning_win_df %>% 
  slice_min(logloss, n = 1) %>% 
  dplyr::slice(1)

max_depth_win <- reg_tuning_win_best$max_depth #4
weight_win <- reg_tuning_win_best$weight #12
subsample_win <- reg_tuning_win_best$subsample #1
nrounds_win <- reg_tuning_win_best$nrounds



dwin <- xgb.DMatrix(as.matrix(win_exp_df %>% 
                                      select(-pitching_win)), label = win_exp_df$pitching_win)


set.seed(101);win_mod <- xgboost(
  params = list(
    eta = 0.3,
    objective = 'binary:logistic',
    eval_metric = 'logloss', 
    max_depth = max_depth_win,
    min_child_weight = weight_win,
    subsample = subsample_win),
  data = dwin,
  nrounds = nrounds_win,
  print_every_n = 50,
  nthread = 7
) 


save(win_mod, file = 'Win_Prob_XGBmod.RData')
load('Win_Prob_XGBmod.RData')


remove(win_split)

statcast_all <- read_parquet('statcast_21_24_pitching.parquet')

statcast_all <- statcast_all %>% arrange(game_pk, at_bat_number, pitch_number) %>% group_by(game_pk, inning_topbot) %>% mutate(sp = first(pitcher)) %>% ungroup()


statcast_all <- statcast_all %>% group_by(game_pk, at_bat_number) %>% slice_max(pitch_number, n = 1) %>% ungroup()

statcast_all <- statcast_all %>% arrange(game_date, game_pk, at_bat_number)

statcast_all$updated_pitcher_win_exp <- predict(win_mod, as.matrix(statcast_all %>% 
                                                                    mutate(base_sit = case_when(
                                                                      is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ '___',
                                                                      !is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ '__1',
                                                                      is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ '_2_',
                                                                      is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ '3__',
                                                                      !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ '_21',
                                                                      is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ '32_',
                                                                      !is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ '3_1',
                                                                      !is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ '321',
                                                                      .default = NA, # check to see if we're missing any base situations
                                                                    )) %>% 
                                                                    left_join(woba_by_player %>% rename('batter_season_wOBA' = 'wOBA'), by = c('batter', 'game_year')) %>% 
                                                                    mutate(on_home_team = ifelse(inning_topbot == 'Top', 1,0)) %>% 
                                                                    reframe(
                                                                      on_home_team,
                                                                      batter_season_wOBA,
                                                                      pitcher_score_diff = fld_score - bat_score,
                                                                      abs_scor_diff = abs(fld_score - bat_score),
                                                                      inning_topbot, 
                                                                      total_runs_scored = bat_score + fld_score,
                                                                      base_sit = as.factor(base_sit),
                                                                      inning = ifelse(inning > 9, 10, inning), #treating all extra innings as the same (sudden death)
                                                                      outs_when_up,
                                                                      tying_winning_run = case_when(
                                                                        pitcher_score_diff > 4 ~ 0,
                                                                        base_sit %in% c('321') & pitcher_score_diff <= 4  & pitcher_score_diff >= 0 ~ 1,
                                                                        base_sit %in% c('32_', '3_1','_21') & pitcher_score_diff <= 3 & pitcher_score_diff >= 0 ~ 1,
                                                                        base_sit %in% c('___') & pitcher_score_diff <= 1 & pitcher_score_diff >= 0 ~ 1,
                                                                        base_sit %in% c('__1','_2_','3__') & pitcher_score_diff <= 2 & pitcher_score_diff >= 0 ~ 1,
                                                                        .default = 0
                                                                      )
                                                                    ) %>% 
                                                                    dummy_cols(select_columns = 'base_sit', remove_most_frequent_dummy = TRUE,
                                                                               remove_selected_columns = TRUE) %>% 
                                                                     mutate(
                                                                       walkoff_possibilty = ifelse(inning_topbot == 'Bot' &  inning >= 9 & tying_winning_run == 1, 1,0)
                                                                     ) %>% 
                                                                     select(-inning_topbot)
                                                                     
                                                                  ))

write_parquet(statcast_all, 'statcast_data_w_WinExp.parquet')
