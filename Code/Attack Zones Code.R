library(tidyverse)
library(arrow)

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
