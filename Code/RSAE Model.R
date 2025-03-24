library(xgboost)
library(tidyverse)
library(arrow)
library(rsample)
library(fastDummies)

appearance_data <- read_csv('appearance_data.csv')


#### Runs allowed based on game_state ####
appearance_data <- appearance_data %>% 
  mutate(initial_base_sit = case_when(
    is.na(base_1b) & is.na(base_2b) & is.na(base_3b) ~ '___',
    !is.na(base_1b) & is.na(base_2b) & is.na(base_3b) ~ '__1',
    is.na(base_1b) & !is.na(base_2b) & is.na(base_3b) ~ '_2_',
    is.na(base_1b) & is.na(base_2b) & !is.na(base_3b) ~ '3__',
    !is.na(base_1b) & !is.na(base_2b) & is.na(base_3b) ~ '_21',
    is.na(base_1b) & !is.na(base_2b) & !is.na(base_3b) ~ '32_',
    !is.na(base_1b) & is.na(base_2b) & !is.na(base_3b) ~ '3_1',
    !is.na(base_1b) & !is.na(base_2b) & !is.na(base_3b) ~ '321',
    .default = NA, # check to see if we're missing any base situations
  )) %>% 
  relocate(initial_base_sit, .after = 'base_3b')


runs_allowed_basestate_df <- appearance_data %>%
  drop_na(outs_start) %>% 
  reframe(runs_allowed,
          outs_start,
          initial_base_sit,
          gidp_possibility = ifelse(initial_base_sit %in% c('__1', '_21', '3_1', '321') & outs_start < 2, 1,0),
          sac_fly_possibility = ifelse(
            initial_base_sit %in% c('3__','3_1','32_','321') & outs_start < 2, 1,0
          ),
          mean_wOBA,
          pitch_number_appearance = pitch_number_appearance - 1) %>% 
  dummy_cols(select_columns = 'initial_base_sit', remove_selected_columns = TRUE)


set.seed(101);runs_allowed_bs_split <- initial_split(runs_allowed_basestate_df, prop = 0.75, strata = runs_allowed)

train_runs_bs <- training(runs_allowed_bs_split)
test_runs_bs <- testing(runs_allowed_bs_split)

dtrain_runs_bs <- xgb.DMatrix(as.matrix(train_runs_bs %>% 
                                       select(-runs_allowed)), label = train_runs_bs$runs_allowed)


dtest_runs_bs <- xgb.DMatrix(as.matrix(test_runs_bs %>%
                                      select(-runs_allowed)), label = test_runs_bs$runs_allowed)

remove(runs_allowed_bs_split)

hyperparam_runs_bs_tuning_reg <- function(max_depth, weight,subsample, row_num){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.train(
    params = list(
      eta = 0.05,
      objective = 'reg:squarederror',
      eval_metric = 'rmse', 
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample),
    data = dtrain_runs_bs,
    watchlist = list(train = dtrain_runs_bs, test = dtest_runs_bs),
    nrounds = 1000,
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

reg_tuning_runs_bs_df <- expand_grid(
  max_depth = c(2,3,4,6,8),
  weight = c(12, 16,20, 24),
  subsample = c(0.5, 0.75,1)
) %>% 
  mutate(row_num = row_number())

runs_bs_rmse_nrounds_df <- pmap_df(list(reg_tuning_runs_bs_df$max_depth, 
                                        reg_tuning_runs_bs_df$weight, 
                                        reg_tuning_runs_bs_df$subsample, 
                                        reg_tuning_runs_bs_df$row_num), hyperparam_runs_bs_tuning_reg)


reg_tuning_runs_bs_df$rmse <- runs_bs_rmse_nrounds_df$rmse; reg_tuning_runs_bs_df$nrounds <- runs_bs_rmse_nrounds_df$nrounds

reg_tuning_runs_bs_df %>% 
  arrange(rmse) %>% 
  head(5)



reg_tuning_runs_bs_best <- reg_tuning_runs_bs_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_runs_bs <- reg_tuning_runs_bs_best$max_depth #3
weight_runs_bs <- reg_tuning_runs_bs_best$weight #20
subsample_runs_bs <- reg_tuning_runs_bs_best$subsample #0.5
nrounds_runs_bs <- reg_tuning_runs_bs_best$nrounds #241


dtrain_rapo_bs <- xgb.DMatrix(as.matrix(runs_allowed_basestate_df %>% 
                                            select(-runs_allowed)), label = runs_allowed_basestate_df$runs_allowed)


set.seed(101);rapo_bs_mod <- xgboost(
  params = list(
    eta = 0.05,
    objective = 'reg:squarederror',
    eval_metric = 'rmse', 
    max_depth = max_depth_runs_bs,
    min_child_weight = weight_runs_bs,
    subsample = subsample_runs_bs),
  data = dtrain_rapo_bs,
  nrounds = nrounds_runs_bs,
  print_every_n = 25,
  nthread = 7
) 

save(rapo_bs_mod, file = 'RAPO_bs_mod.RData')

load('RAPO_bs_mod.RData')

#exp runs allowed statline

runs_allowed_expected_df <- appearance_data %>%
  drop_na(outs_start) %>% 
  reframe(runs_allowed,
          outs_start,
          initial_base_sit,
          batters_faced,
          outs_made,
          pitch_number_appearance = pitch_number_appearance - 1,
          max_possible_runs = case_when(
            initial_base_sit == '___' ~ 0 + batters_faced,
            initial_base_sit == '321' ~ 3 + batters_faced,
            initial_base_sit %in% c('__1', '_2_', '3__') ~ 1 + batters_faced,
            .default = 2 + batters_faced
          ),
          gidp_possibility = ifelse(initial_base_sit %in% c('__1', '_21', '3_1', '321') & outs_start < 2, 1,0),
          sac_fly_possibility = ifelse(
            initial_base_sit %in% c('3__','3_1','32_','321') & outs_start < 2, 1,0
          ),
          mean_wOBA,
          strikeouts,
          walks,
          barrels,
          avg_exit_velocity,
          Chase,
          Heart,
          Shadow,
          Waste,
          BIP,
          GB,
          LD,
          FB,
          Pop
          ) %>% 
  dummy_cols(select_columns = 'initial_base_sit', remove_selected_columns = TRUE)



set.seed(101);runs_allowed_exp_split <- initial_split(runs_allowed_expected_df, prop = 0.75, strata = runs_allowed)

train_runs_exp <- training(runs_allowed_exp_split)
test_runs_exp <- testing(runs_allowed_exp_split)

dtrain_runs_exp <- xgb.DMatrix(as.matrix(train_runs_exp %>% 
                                          select(-runs_allowed)), label = train_runs_exp$runs_allowed)


dtest_runs_exp <- xgb.DMatrix(as.matrix(test_runs_exp %>%
                                         select(-runs_allowed)), label = test_runs_exp$runs_allowed)

remove(runs_allowed_exp_split)

hyperparam_runs_exp_tuning_reg <- function(max_depth, weight,subsample, row_num){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.train(
    params = list(
      eta = 0.05,
      objective = 'reg:squarederror',
      eval_metric = 'rmse', 
      lambda = 0,
      gamma = 0,
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample),
    data = dtrain_runs_exp,
    watchlist = list(train = dtrain_runs_exp, test = dtest_runs_exp),
    nrounds = 5000,
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

reg_tuning_runs_exp_df <- expand_grid(
  max_depth = c(3,4,6,8),
  weight = c(1,4,8,12, 16),
  subsample = c(0.5, 0.75,1)
) %>% 
  mutate(row_num = row_number())

runs_exp_rmse_nrounds_df <- pmap_df(list(reg_tuning_runs_exp_df$max_depth, 
                                        reg_tuning_runs_exp_df$weight, 
                                        reg_tuning_runs_exp_df$subsample, 
                                        reg_tuning_runs_exp_df$row_num), hyperparam_runs_exp_tuning_reg)


reg_tuning_runs_exp_df$rmse <- runs_exp_rmse_nrounds_df$rmse; reg_tuning_runs_exp_df$nrounds <- runs_exp_rmse_nrounds_df$nrounds

reg_tuning_runs_exp_df %>% 
  arrange(rmse) %>% 
  head(5)



reg_tuning_runs_exp_best <- reg_tuning_runs_exp_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_runs_exp <- reg_tuning_runs_exp_best$max_depth # 4
weight_runs_exp <- reg_tuning_runs_exp_best$weight # 12
subsample_runs_exp <- reg_tuning_runs_exp_best$subsample # 1
nrounds_runs_exp <- reg_tuning_runs_exp_best$nrounds


dtrain_rapo_exp <- xgb.DMatrix(as.matrix(runs_allowed_expected_df %>% 
                                          select(-runs_allowed)), label = runs_allowed_expected_df$runs_allowed)


set.seed(101);rapo_exp_mod <- xgboost(
  params = list(
    eta = 0.1,
    objective = 'reg:squarederror',
    eval_metric = 'rmse', 
    lambda = 0.5,
    gamma = 0,
    max_depth = max_depth_runs_exp,
    min_child_weight = weight_runs_exp,
    subsample = subsample_runs_exp),
  data = dtrain_rapo_exp,
  nrounds = nrounds_runs_exp,
  print_every_n = 25,
  nthread = 7
) 

appearance_data <- appearance_data %>% drop_na(outs_start)


save(rapo_exp_mod, file = 'RAPO_exp_mod.RData')

load(file = 'RAPO_exp_mod.RData')
load(file = 'RAPO_bs_mod.RData')


appearance_data$runs_bs <- predict(rapo_bs_mod, as.matrix(
  appearance_data %>% 
    drop_na(outs_start) %>% 
    reframe(
      outs_start,
      initial_base_sit,
      gidp_possibility = ifelse(initial_base_sit %in% c('__1', '_21', '3_1', '321') & outs_start < 2, 1,0),
      sac_fly_possibility = ifelse(
        initial_base_sit %in% c('3__','3_1','32_','321') & outs_start < 2, 1,0
      ),
      mean_wOBA,
      pitch_number_appearance = pitch_number_appearance - 1
      )%>% 
    dummy_cols(select_columns = 'initial_base_sit', remove_selected_columns = TRUE)
  
))


appearance_data$exp_runs_allowed <- predict(rapo_exp_mod, as.matrix(
  appearance_data %>%
    drop_na(outs_start) %>% 
    reframe(
      outs_start,
      initial_base_sit,
      batters_faced,
      outs_made,
      pitch_number_appearance = pitch_number_appearance - 1,
      max_possible_runs = case_when(
        initial_base_sit == '___' ~ 0 + batters_faced,
        initial_base_sit == '321' ~ 3 + batters_faced,
        initial_base_sit %in% c('__1', '_2_', '3__') ~ 1 + batters_faced,
        .default = 2 + batters_faced
      ),
      gidp_possibility = ifelse(initial_base_sit %in% c('__1', '_21', '3_1', '321') & outs_start < 2, 1,0),
      sac_fly_possibility = ifelse(
        initial_base_sit %in% c('3__','3_1','32_','321') & outs_start < 2, 1,0
      ),
      mean_wOBA,
      strikeouts,
      walks,
      barrels,
      avg_exit_velocity,
      Chase,
      Heart,
      Shadow,
      Waste,
      BIP,
      GB,
      LD,
      FB,
      Pop
    ) %>% 
    dummy_cols(select_columns = 'initial_base_sit', remove_selected_columns = TRUE)
))

write_csv(appearance_data, 'appearance_data_w_RAPO.csv')

