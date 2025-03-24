library(xgboost)
library(tidyverse)
library(arrow)
library(rsample)
library(fastDummies)

statcast_all <- read_parquet('statcast_21_24_pitching.parquet')


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


#### Finding Heart, Shadow, Chase, & Waste portions of the zone ####

# converting plate location to inches
statcast_all <- statcast_all %>% 
  mutate(plate_x_in = plate_x*12,
         plate_z_in = plate_z * 12) %>% 
  relocate(plate_x_in, .after = plate_x) %>% 
  relocate(plate_z_in, .after = plate_z)

# converting top and bottom of strikezone coordinates to inches
statcast_all <- statcast_all %>% 
  mutate(sz_top_in = sz_top*12,
         sz_bot_in = sz_bot * 12) %>% 
  relocate(sz_top_in, .after = sz_top) %>% 
  relocate(sz_bot_in, .after = sz_bot)

#### Identifying Attack Zones Code ####
#adding  left and right positions for attack zones
heart_left <- -6.7; heart_right <- 6.7
shadow_farleft <- -13.3; shadow_left <- -6.7
shadow_farright <- 13.3; shadow_right <- 6.7
chase_farleft <- (-13.3-6.6); chase_left <- -13.3
chase_farright <- (13.3+6.6);chase_right <- 13.3
waste_left <- -13.3-6.6; waste_right <- 13.3+6.6


# finding middle of vertical strikezone
statcast_all <- statcast_all %>% 
  mutate(sz_mid_in = (sz_top_in + sz_bot_in)/2) %>% 
  relocate(sz_mid_in, .after = sz_top_in)

# finding "percentages: in pitch location vs strike zone from attack zones
statcast_all <- statcast_all %>% 
  mutate(plate_z_pct = ifelse(plate_z_in <= sz_mid_in,(plate_z_in - sz_mid_in)/(sz_mid_in-sz_bot_in)*100, (plate_z_in - sz_mid_in)/(sz_top_in-sz_mid_in)*100)) %>% 
  relocate(plate_z_pct, .after = plate_z_in) %>% 
  relocate(sz_mid_in, .after = plate_z_pct)

# adding percentage positions for attack zones
heart_pct_bot <- -67; heart_pct_top <- 67
shadow_pct_farbot <- -133; shadow_pct_bot <- -67
shadow_pct_fartop <- 133; shadow_pct_top <- 67
chase_pct_farbot <- -200; chase_pct_bot <- -133
chase_pct_fartop <- 200; chase_pct_top <- 133
waste_bot <- -200; waste_top <- 200

# finding attack zones
statcast_all <- statcast_all %>% 
  mutate(attack_zone = case_when(
    (plate_x_in <= heart_right & plate_x_in >= heart_left) &
      (plate_z_pct <= heart_pct_top & plate_z_pct >= heart_pct_bot) ~ 'Heart',
    (plate_x_in <= shadow_farright & plate_x_in > shadow_right & 
       plate_z_pct <= shadow_pct_fartop & plate_z_pct >= shadow_pct_farbot) |
      (plate_x_in <= shadow_farright & plate_x_in >= shadow_farleft &
         plate_z_pct <= shadow_pct_fartop & plate_z_pct > shadow_pct_top) |
      (plate_x_in >= shadow_farleft & plate_x_in < shadow_left & 
         plate_z_pct <= shadow_pct_fartop & plate_z_pct >= shadow_pct_farbot) |
      (plate_x_in <= shadow_farright & plate_x_in >= shadow_farleft &
         plate_z_pct >= shadow_pct_farbot & plate_z_pct < shadow_pct_bot)~ 'Shadow',
    plate_x_in < waste_left | plate_x_in > waste_right |
      plate_z_pct > waste_top | plate_z_pct < waste_bot ~ 'Waste',
    .default = 'Chase'
  )) %>% 
  relocate(attack_zone, .after = 'plate_z_pct')


statcast_all <- statcast_all %>% 
  left_join(woba_by_player %>% rename('batter_season_wOBA' = 'wOBA'), by = c('batter', 'game_year'))


runs_exp_saves_df <- statcast_all %>%
  group_by(game_pk, at_bat_number) %>% 
  slice_max(pitch_number, n = 1) %>% 
  ungroup() %>% # above code converts data to at-bat level
  distinct() %>% 
  reframe(
    runs_scored = post_bat_score - bat_score,
    is_400ft = ifelse(hit_distance_sc >= 400, 1,0),
    base_sit,
    launch_speed = ifelse(is.na(launch_speed), 0, launch_speed),
    below_0_LA = ifelse(launch_angle < 0, 1,0),
    barrel = ifelse(launch_speed_angle == 6, 1,0),
    attack_zone = as.factor(attack_zone),
    batter_woba = batter_season_wOBA,
    max_runs_possible = case_when(
      base_sit == '___' ~ 1,
      base_sit == '321' ~ 4,
      base_sit %in% c('__1', '_2_', '3__') ~ 2,
      .default = 3
    ),
    bb_type = as.factor(bb_type)
  ) %>% 
  mutate(
    base_sit = as.factor(base_sit)
  ) %>% 
  dummy_cols(select_columns = c('bb_type','base_sit', 'attack_zone'), remove_selected_columns = TRUE)



set.seed(101);run_save_split <- initial_split(runs_exp_saves_df, prop = 0.75, strata = runs_scored)

train_runs_save <- training(run_save_split)
test_runs_save <- testing(run_save_split)

dtrain_runs <- xgb.DMatrix(as.matrix(train_runs_save %>% 
                                      select(-runs_scored)), label = train_runs_save$runs_scored)


dtest_runs <- xgb.DMatrix(as.matrix(test_runs_save %>%
                                     select(-runs_scored)), label = test_runs_save$runs_scored)

remove(run_save_split)

hyperparam_runs_saves_tuning_reg <- function(max_depth, weight,subsample, row_num){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101);mod <- xgb.train(
    params = list(
      eta = 0.3,
      objective = 'reg:squarederror',
      eval_metric = 'rmse', 
      lambda = 0,
      gamma = 0,
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample),
    data = dtrain_runs,
    watchlist = list(train = dtrain_runs, test = dtest_runs),
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

reg_tuning_runs_saves_df <- expand_grid(
  max_depth = c(3,4,6,8),
  weight = c(1,4,8,12, 16),
  subsample = c(0.5, 0.75,1)
) %>% 
  mutate(row_num = row_number())

runs_rmse_nrounds_df <- pmap_df(list(reg_tuning_runs_saves_df$max_depth, reg_tuning_runs_saves_df$weight, reg_tuning_runs_saves_df$subsample, reg_tuning_runs_saves_df$row_num), hyperparam_runs_saves_tuning_reg)


reg_tuning_runs_saves_df$rmse <- runs_rmse_nrounds_df$rmse; reg_tuning_runs_saves_df$nrounds <- runs_rmse_nrounds_df$nrounds

reg_tuning_runs_saves_df %>% 
  arrange(rmse) %>% 
  head(5)



reg_tuning_runs_saves_best <- reg_tuning_runs_saves_df %>% 
  slice_min(rmse, n = 1) %>% 
  dplyr::slice(1)

max_depth_runs_saves <- reg_tuning_runs_saves_best$max_depth #6
weight_runs_saves <- reg_tuning_runs_saves_best$weight #4
subsample_runs_saves <- reg_tuning_runs_saves_best$subsample #1
nrounds_runs_saves <- reg_tuning_runs_saves_best$nrounds



dtrain_runs_save <- xgb.DMatrix(as.matrix(runs_exp_saves_df %>% 
                                      select(-runs_scored)), label = runs_exp_saves_df$runs_scored)


set.seed(101);saves_run_mod <- xgboost(
  params = list(
    eta = 0.3,
    objective = 'reg:squarederror',
    eval_metric = 'rmse', 
    max_depth = max_depth_runs_saves,
    min_child_weight = weight_runs_saves,
    subsample = subsample_runs_saves),
  data = dtrain_runs_save,
  nrounds = nrounds_runs_saves,
  print_every_n = 1,
  nthread = 7
) 


pitchers_abs <- read_parquet('pitcher_abs.parquet')

save(saves_run_mod, file = 'Saves_Run_Model.RData')

pitchers_abs <- pitchers_abs %>% 
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

# converting plate location to inches
pitchers_abs <- pitchers_abs %>% 
  mutate(plate_x_in = plate_x*12,
         plate_z_in = plate_z * 12) %>% 
  relocate(plate_x_in, .after = plate_x) %>% 
  relocate(plate_z_in, .after = plate_z)

# converting top and bottom of strikezone coordinates to inches
pitchers_abs <- pitchers_abs %>% 
  mutate(sz_top_in = sz_top*12,
         sz_bot_in = sz_bot * 12) %>% 
  relocate(sz_top_in, .after = sz_top) %>% 
  relocate(sz_bot_in, .after = sz_bot)

#### Identifying Attack Zones Code ####
#adding  left and right positions for attack zones
heart_left <- -6.7; heart_right <- 6.7
shadow_farleft <- -13.3; shadow_left <- -6.7
shadow_farright <- 13.3; shadow_right <- 6.7
chase_farleft <- (-13.3-6.6); chase_left <- -13.3
chase_farright <- (13.3+6.6);chase_right <- 13.3
waste_left <- -13.3-6.6; waste_right <- 13.3+6.6


# finding middle of vertical strikezone
pitchers_abs <- pitchers_abs %>% 
  mutate(sz_mid_in = (sz_top_in + sz_bot_in)/2) %>% 
  relocate(sz_mid_in, .after = sz_top_in)

# finding "percentages: in pitch location vs strike zone from attack zones
pitchers_abs <- pitchers_abs %>% 
  mutate(plate_z_pct = ifelse(plate_z_in <= sz_mid_in,(plate_z_in - sz_mid_in)/(sz_mid_in-sz_bot_in)*100, (plate_z_in - sz_mid_in)/(sz_top_in-sz_mid_in)*100)) %>% 
  relocate(plate_z_pct, .after = plate_z_in) %>% 
  relocate(sz_mid_in, .after = plate_z_pct)

# adding percentage positions for attack zones
heart_pct_bot <- -67; heart_pct_top <- 67
shadow_pct_farbot <- -133; shadow_pct_bot <- -67
shadow_pct_fartop <- 133; shadow_pct_top <- 67
chase_pct_farbot <- -200; chase_pct_bot <- -133
chase_pct_fartop <- 200; chase_pct_top <- 133
waste_bot <- -200; waste_top <- 200

# finding attack zones
pitchers_abs <- pitchers_abs %>% 
  mutate(attack_zone = case_when(
    (plate_x_in <= heart_right & plate_x_in >= heart_left) &
      (plate_z_pct <= heart_pct_top & plate_z_pct >= heart_pct_bot) ~ 'Heart',
    (plate_x_in <= shadow_farright & plate_x_in > shadow_right & 
       plate_z_pct <= shadow_pct_fartop & plate_z_pct >= shadow_pct_farbot) |
      (plate_x_in <= shadow_farright & plate_x_in >= shadow_farleft &
         plate_z_pct <= shadow_pct_fartop & plate_z_pct > shadow_pct_top) |
      (plate_x_in >= shadow_farleft & plate_x_in < shadow_left & 
         plate_z_pct <= shadow_pct_fartop & plate_z_pct >= shadow_pct_farbot) |
      (plate_x_in <= shadow_farright & plate_x_in >= shadow_farleft &
         plate_z_pct >= shadow_pct_farbot & plate_z_pct < shadow_pct_bot)~ 'Shadow',
    plate_x_in < waste_left | plate_x_in > waste_right |
      plate_z_pct > waste_top | plate_z_pct < waste_bot ~ 'Waste',
    .default = 'Chase'
  )) %>% 
  relocate(attack_zone, .after = 'plate_z_pct')


pitchers_abs <- pitchers_abs %>% 
  left_join(woba_by_player %>% rename('batter_season_wOBA' = 'wOBA'), by = c('batter', 'game_year'))




pitchers_abs$run_expectancy_saves <- predict(saves_run_mod, as.matrix(pitchers_abs %>% 
                                                                         # above code converts data to at-bat level
                                                                        distinct() %>% 
                                                                        reframe(
                                                                          is_400ft = ifelse(hit_distance_sc >= 400, 1,0),
                                                                          base_sit,
                                                                          launch_speed = ifelse(is.na(launch_speed), 0, launch_speed),
                                                                          below_0_LA = ifelse(launch_angle < 0, 1,0),
                                                                          barrel = ifelse(launch_speed_angle == 6, 1,0),
                                                                          attack_zone = as.factor(attack_zone),
                                                                          batter_woba = batter_season_wOBA,
                                                                          max_runs_possible = case_when(
                                                                            base_sit == '___' ~ 1,
                                                                            base_sit == '321' ~ 4,
                                                                            base_sit %in% c('__1', '_2_', '3__') ~ 2,
                                                                            .default = 3
                                                                          ),
                                                                          bb_type = as.factor(bb_type)
                                                                        ) %>% 
                                                                        mutate(
                                                                          base_sit = as.factor(base_sit)
                                                                        ) %>% 
                                                                        dummy_cols(select_columns = c('bb_type','base_sit', 'attack_zone'), remove_selected_columns = TRUE)
))

write_csv(pitchers_abs, 'save_run_exp_pitcher_abs.csv')

asfs <- pitchers_abs %>% reframe(game_pk, inning, base_sit, outs_when_up, run_expectancy_saves,  runs_scored = post_bat_score - bat_score)


asfs %>% ggplot(aes(
  run_expectancy_saves
)) +
  geom_density()

asfs %>% filter(base_sit != '___') %>% ggplot(aes(
  run_expectancy_saves
)) +
  geom_density()

asfs %>% filter(base_sit %in% c('321')) %>% ggplot(aes(
  run_expectancy_saves
)) +
  geom_density()


asfs %>% 
  mutate(run_expectancy_saves = case_when(
    run_expectancy_saves < 0 ~ 0,
    run_expectancy_saves > 4 ~ 4,
    .default = run_expectancy_saves
  )) %>% ggplot(aes(
  run_expectancy_saves,
  fill = as.factor(outs_when_up),
  group = outs_when_up
)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~base_sit, scales = 'free_y' )
