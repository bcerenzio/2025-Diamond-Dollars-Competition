library(xgboost)
library(tidyverse)
library(arrow)
library(rsample)
library(fastDummies)

delta_last_full <- read_csv('delta_last_full2.csv')


delta_last_full <- delta_last_full %>% 
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

batter_woba <- read_csv('weighted_woba.csv')


# joining batter wobas to the statcast table
delta_last_full <- delta_last_full %>% 
  left_join(batter_woba %>% rename('batter_season_wOBA' = 'wOBA'), by = c('batter', 'game_year'))

# finding if pitcher is on the home or away team
delta_last_full <- delta_last_full %>% 
  mutate(on_home_team = ifelse(inning_topbot == 'Top', 1,0)) # Note: home pitchers pitch in the top of the inning


batter_probs <- read_csv('player_probabilities.csv')

glimpse(batter_probs)

upd_batter_probs <- batter_probs %>% 
  select(-prob_total, -new_PA) %>% 
  rename('batter' = 'key_mlbam', 'game_year' = 'season', 'batter_name' = 'player_name')

delta_last_full <- delta_last_full %>% 
  left_join(upd_batter_probs, by = c('batter', 'game_year'))

dwin_exp_mod_df <- delta_last_full %>% 
  reframe(
    swing_delta_win_exp = abs(new_delta_win_exp),
    on_home_team,
    inning_topbot,
    batter_season_wOBA = ifelse(is.na(batter_season_wOBA), mean(batter_season_wOBA, na.rm = TRUE), batter_season_wOBA),
    pitcher_score_diff = fld_score - bat_score,
    abs_scor_diff = abs(fld_score - bat_score),
    total_runs_scored = bat_score + fld_score,
    base_sit = as.factor(base_sit),
    inning = ifelse(inning > 9, 10, inning), #treating all extra innings as the same (sudden death)
    outs_when_up,
    prob_1B = ifelse(is.na(prob_1B), mean(prob_1B, na.rm = TRUE), prob_1B),
    prob_2B = ifelse(is.na(prob_2B), mean(prob_2B, na.rm = TRUE), prob_2B),
    prob_3B = ifelse(is.na(prob_3B), mean(prob_3B, na.rm = TRUE), prob_3B),
    prob_HR = ifelse(is.na(prob_HR), mean(prob_HR, na.rm = TRUE), prob_HR),
    prob_K = ifelse(is.na(prob_K), mean(prob_K, na.rm = TRUE), prob_K),
    prob_BB = ifelse(is.na(prob_BB), mean(prob_BB, na.rm = TRUE), prob_BB),
    prob_SF = ifelse(is.na(prob_SF), mean(prob_SF, na.rm = TRUE), prob_SF),
    prob_out = ifelse(is.na(prob_out), mean(prob_out, na.rm = TRUE), prob_out),
    tying_winning_run = case_when(
      pitcher_score_diff > 4 ~ 0,
      base_sit %in% c('321') & pitcher_score_diff <= 4  & pitcher_score_diff >= 0 ~ 1,
      base_sit %in% c('32_', '3_1','_21') & pitcher_score_diff <= 3 & pitcher_score_diff >= 0 ~ 1,
      base_sit %in% c('___') & pitcher_score_diff <= 1 & pitcher_score_diff >= 0 ~ 1,
      base_sit %in% c('__1','_2_','3__') & pitcher_score_diff <= 2 & pitcher_score_diff >= 0 ~ 1,
      .default = 0
    )
  ) %>% 
  dummy_cols(select_columns = 'base_sit', remove_most_frequent_dummy = TRUE, remove_selected_columns = TRUE)

dwin_exp_mod_df <- dwin_exp_mod_df %>% 
  drop_na(swing_delta_win_exp) %>% 
  mutate(
    walkoff_possibilty = ifelse(inning_topbot == 'Bot' &  inning >= 9 & tying_winning_run == 1, 1,0)
  ) %>% 
  select(-inning_topbot)


set.seed(101);delta_win_split <- initial_split(dwin_exp_mod_df, prop = 0.65, strata = swing_delta_win_exp)

train_dwin <- training(delta_win_split)
test_dwin<- testing(delta_win_split)

dtrain_dwin <- xgb.DMatrix(as.matrix(train_dwin %>% 
                                      select(-swing_delta_win_exp)), label = train_dwin$swing_delta_win_exp)


dtest_dwin <- xgb.DMatrix(as.matrix(test_dwin %>%
                                     select(-swing_delta_win_exp)), label = test_dwin$swing_delta_win_exp)


hyperparam_dwin_tuning_reg <- function(max_depth, weight,subsample, row_num){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.train(
    params = list(
      eta = 0.5,
      objective = 'reg:squarederror',
      eval_metric = 'rmse', 
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample),
    data = dtrain_dwin,
    watchlist = list(train = dtrain_dwin, test = dtest_dwin),
    nrounds = 500,
    early_stopping_rounds = 50,
    print_every_n = 50,
    nthread = 7
  ) 
  
  rmse <- mod$evaluation_log %>% 
    slice_min(test_rmse, n = 1) %>% 
    pull(test_rmse)
  
  nrounds <- mod$evaluation_log %>% 
    slice_min(test_rmse, n = 1) %>% 
    pull(iter)
  
  df <- tibble(
    rmse = rmse,
    nrounds = nrounds
  )
  
  return(df)
  
}

reg_tuning_dwin_df <- expand_grid(
  max_depth = c(3,4,6,8),
  weight = c(1,4,8,12, 16),
  subsample = c(0.5, 0.75,1)
) %>% 
  mutate(row_num = row_number())

dwin_rmse_nrounds_df <- pmap_df(list(reg_tuning_dwin_df$max_depth, reg_tuning_dwin_df$weight, reg_tuning_dwin_df$subsample, reg_tuning_dwin_df$row_num), hyperparam_dwin_tuning_reg)


reg_tuning_dwin_df$rmse <- dwin_rmse_nrounds_df$rmse; reg_tuning_dwin_df$nrounds <- dwin_rmse_nrounds_df$nrounds

reg_tuning_dwin_df %>% 
  arrange(rmse) %>% 
  head(5)



reg_tuning_dwin_best <- reg_tuning_dwin_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_dwin <- reg_tuning_dwin_best$max_depth #6
weight_dwin <- reg_tuning_dwin_best$weight #16
subsample_dwin <- reg_tuning_dwin_best$subsample #1
nrounds_dwin <- reg_tuning_dwin_best$nrounds# 67


dwin_exp <- xgb.DMatrix(as.matrix(dwin_exp_mod_df %>% 
                                select(-swing_delta_win_exp)), label = dwin_exp_mod_df$swing_delta_win_exp)


set.seed(101);dwin_mod <- xgboost(
  params = list(
    eta = 0.5,
    objective = 'reg:squarederror',
    eval_metric = 'rmse', 
    max_depth = max_depth_dwin,
    min_child_weight = weight_dwin,
    subsample = subsample_dwin),
  data = dwin_exp,
  nrounds = nrounds_dwin,
  print_every_n = 50,
  nthread = 7
) 


save(dwin_mod, file = 'delta_Win_Prob_XGBmod.RData')
load('delta_Win_Prob_XGBmod.RData')

delta_last_full <- delta_last_full %>% drop_na(new_delta_win_exp)

delta_last_full$ab_win_expect <- predict(dwin_mod, as.matrix(
  delta_last_full %>% 
    reframe(
      on_home_team,
      inning_topbot,
      batter_season_wOBA = ifelse(is.na(batter_season_wOBA), mean(batter_season_wOBA, na.rm = TRUE), batter_season_wOBA),
      pitcher_score_diff = fld_score - bat_score,
      abs_scor_diff = abs(fld_score - bat_score),
      total_runs_scored = bat_score + fld_score,
      base_sit = as.factor(base_sit),
      inning = ifelse(inning > 9, 10, inning), #treating all extra innings as the same (sudden death)
      outs_when_up,
      prob_1B = ifelse(is.na(prob_1B), mean(prob_1B, na.rm = TRUE), prob_1B),
      prob_2B = ifelse(is.na(prob_2B), mean(prob_2B, na.rm = TRUE), prob_2B),
      prob_3B = ifelse(is.na(prob_3B), mean(prob_3B, na.rm = TRUE), prob_3B),
      prob_HR = ifelse(is.na(prob_HR), mean(prob_HR, na.rm = TRUE), prob_HR),
      prob_K = ifelse(is.na(prob_K), mean(prob_K, na.rm = TRUE), prob_K),
      prob_BB = ifelse(is.na(prob_BB), mean(prob_BB, na.rm = TRUE), prob_BB),
      prob_SF = ifelse(is.na(prob_SF), mean(prob_SF, na.rm = TRUE), prob_SF),
      prob_out = ifelse(is.na(prob_out), mean(prob_out, na.rm = TRUE), prob_out),
      tying_winning_run = case_when(
        pitcher_score_diff > 4 ~ 0,
        base_sit %in% c('321') & pitcher_score_diff <= 4  & pitcher_score_diff >= 0 ~ 1,
        base_sit %in% c('32_', '3_1','_21') & pitcher_score_diff <= 3 & pitcher_score_diff >= 0 ~ 1,
        base_sit %in% c('___') & pitcher_score_diff <= 1 & pitcher_score_diff >= 0 ~ 1,
        base_sit %in% c('__1','_2_','3__') & pitcher_score_diff <= 2 & pitcher_score_diff >= 0 ~ 1,
        .default = 0
      )
    ) %>% 
    dummy_cols(select_columns = 'base_sit', remove_most_frequent_dummy = TRUE, remove_selected_columns = TRUE) %>% 
    mutate(
      walkoff_possibilty = ifelse(inning_topbot == 'Bot' &  inning >= 9 & tying_winning_run == 1, 1,0)
    ) %>% 
    select(-inning_topbot)
))

delta_last_full <- delta_last_full %>% 
  mutate(ab_win_expect = ifelse(ab_win_expect < 0, 0.001, ab_win_expect))


write_csv(delta_last_full, 'win_exp_data.csv')
