library(tidyverse)
library(arrow)

statcast <- read_parquet("statcast_21_24_pitching.parquet")

at_bat_data <- statcast %>%
  filter(!is.na(events))

woba_by_player <- at_bat_data %>%
  group_by(batter, game_year) %>%
  summarise(
    total_woba_value = sum(woba_value, na.rm = TRUE),
    total_woba_denom = sum(woba_denom, na.rm = TRUE),
    wOBA = ifelse(total_woba_denom != 0, total_woba_value / total_woba_denom, 0)
  )

woba_by_player <- woba_by_player %>%
  select(c(batter, game_year, wOBA))

write_csv(woba_by_player, 'batter_woba.csv')
