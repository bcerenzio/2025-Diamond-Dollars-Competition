library(tidyverse)
library(arrow)

statcast <- read_parquet("statcast_21_24_pitching.parquet")
batter_woba <- read_csv('weighted_woba.csv') %>% select(-"...1")

statcast <- statcast %>% 
  arrange(game_pk, at_bat_number, pitch_number)

statcast_pa_level <- statcast %>% 
  group_by(game_pk, at_bat_number) %>% 
  slice_max(pitch_number, n = 1) %>% 
  ungroup()




appearance_start_game_state <- statcast_pa_level %>% 
  group_by(game_pk, pitcher, inning) %>% 
  reframe(
    outs_start = first(outs_when_up),
    base_1b = first(on_1b),
    base_2b = first(on_2b),
    base_3b = first(on_3b)
  ) %>% 
  ungroup()

statcast_pa_level <- statcast_pa_level %>%
  left_join(batter_woba, by = c('batter', 'game_year'))


woba_summary <- statcast_pa_level %>%
  group_by(game_pk, pitcher, inning) %>%
  summarise(mean_wOBA = mean(wOBA, na.rm = TRUE)) %>%
  ungroup()
# 
# outs_made_summary <- statcast %>%
#   group_by(game_pk,pitcher, inning) %>%
#   summarise(
#     outs_made = sum(events %in% c('field_out', 'strikeout', 'force_out', 'sac_fly', 'sac_bunt', 'fielders_choice_out'), na.rm = TRUE) +
#       sum(events %in% c('double_play', 'grounded_into_double_play', 'strikeout_double_play', 'sac_fly_double_play', 'sac_bunt_double_play'), na.rm = TRUE) * 2 +
#       sum(events %in% c('triple_play', 'sac_fly_triple_play'), na.rm = TRUE) * 3
#   ) %>%
#   ungroup()


outs_made_summary <- statcast %>%
    group_by(game_pk, inning, inning_topbot) %>% 
    mutate(num_outs_made = ifelse(is.na(lead(outs_when_up,1)), 3 - outs_when_up, lead(outs_when_up,1) - outs_when_up)) %>% 
    ungroup() %>% 
    group_by(game_pk,pitcher, inning) %>%
    summarise(
      outs_made = sum(num_outs_made)
    ) %>%
    ungroup()

batters_faced_summary <- statcast_pa_level %>%
  group_by(pitcher, game_pk, inning) %>%
  summarise(
    batters_faced = n(),
    strikeouts = sum(events %in% c('strikeout', 'strikeout_double_play'), na.rm = TRUE),
    walks = sum(events %in% c('walk', 'hit_by_pitch'), na.rm = TRUE),
    ) %>%
  ungroup()

barrels_summary <- statcast_pa_level %>%
  group_by(pitcher, game_pk, inning) %>%
  summarise(
    barrels = sum(launch_speed_angle == 6, na.rm = TRUE),
    avg_exit_velocity = mean(launch_speed, na.rm = TRUE)
  ) %>%
  ungroup()
  
runs_summary <- statcast_pa_level %>%
  group_by(pitcher, game_pk, inning) %>%
  summarise(
    runs_allowed = max(post_bat_score, na.rm = TRUE) -
      min(bat_score, na.rm = TRUE)) %>%
  ungroup()



# converting plate location to inches
statcast <- statcast %>% 
  mutate(plate_x_in = plate_x*12,
         plate_z_in = plate_z * 12) %>% 
  relocate(plate_x_in, .after = plate_x) %>% 
  relocate(plate_z_in, .after = plate_z)

# converting top and bottom of strikezone coordinates to inches
statcast <- statcast %>% 
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
statcast <- statcast %>% 
  mutate(sz_mid_in = (sz_top_in + sz_bot_in)/2) %>% 
  relocate(sz_mid_in, .after = sz_top_in)

# finding "percentages: in pitch location vs strike zone from attack zones
statcast <- statcast %>% 
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
statcast <- statcast %>% 
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

  
attack_zone_summary <- statcast %>%
  group_by(pitcher, game_pk, attack_zone, inning) %>%
  summarise(pitches_in_zone = n()) %>%
  group_by(pitcher, game_pk, inning) %>%
  mutate(perc_pitches_in_zone = pitches_in_zone / sum(pitches_in_zone),
         num_pitches = sum(pitches_in_zone)) %>%
  ungroup() %>% 
  select(-pitches_in_zone) %>%
  pivot_wider(names_from = "attack_zone", values_from = perc_pitches_in_zone, values_fill = 0)


# finding total pitches thrown before entering next inning
pitches_thrown_summary <- statcast %>% 
  group_by(game_pk, pitcher) %>% 
  reframe(
    inning,
    pitch_number_appearance = row_number()) %>% 
  group_by(game_pk, pitcher, inning) %>% 
  slice_min(pitch_number_appearance, n = 1) %>% 
  ungroup()

# BIP & batted ball data
bip_summary <- statcast_pa_level %>% 
  group_by(game_pk, pitcher, inning) %>% 
  reframe(
    BIP = sum(description == 'hit_into_play', na.rm = TRUE),
    GB = sum(bb_type == 'ground_ball', na.rm = TRUE),
    LD = sum(bb_type == 'line_drive', na.rm = TRUE),
    FB = sum(bb_type == 'fly_ball', na.rm = TRUE),
    Pop = sum(bb_type == 'popup', na.rm = TRUE)
  )

hits_summary <- statcast_pa_level %>% 
  group_by(game_pk, pitcher, inning) %>% 
  reframe(
    H = sum(events %in% c('single','double','triple','home_run'))
  )


final_summary <- appearance_start_game_state %>%
  left_join(woba_summary, by = c('pitcher', 'game_pk', 'inning')) %>%
  left_join(outs_made_summary, by = c('pitcher', 'game_pk', 'inning')) %>%
  left_join(batters_faced_summary, by = c('pitcher', 'game_pk', 'inning')) %>%
  left_join(barrels_summary, by = c('pitcher', 'game_pk', 'inning')) %>%
  left_join(runs_summary, by = c('pitcher', 'game_pk', 'inning')) %>%
  left_join(attack_zone_summary, by = c('pitcher', 'game_pk', 'inning')) %>% 
  left_join(pitches_thrown_summary, by = c('pitcher','game_pk','inning')) %>% 
  left_join(bip_summary, by = c('pitcher','game_pk','inning')) %>% 
  left_join(hits_summary, by = c('pitcher','game_pk','inning'))


pitchers_abs <- read_parquet('pitcher_abs.parquet')

appearance_data <- pitchers_abs %>% 
  select(game_pk, pitcher, inning) %>% 
  left_join(final_summary, by = c('game_pk','pitcher', 'inning')) %>% 
  distinct()

appearance_data <- appearance_data %>% 
  mutate(avg_exit_velocity = ifelse(is.na(avg_exit_velocity), 0, avg_exit_velocity))


write_csv(appearance_data, 'appearance_data.csv')
