library(tidyverse)
library(arrow)

statcast <- read_parquet("pitcher_abs.parquet")

csv_files <- c('AAA_pbp_2021.csv', 'AAA_pbp_2022.csv', 'AAA_pbp_2023.csv', 'AAA_pbp_2024.csv')

combined_df <- bind_rows(lapply(csv_files, read_csv))

statcast$game_date <- as.Date(statcast$game_date, format='%Y-%m-%d')
combined_df$game_date <- as.Date(combined_df$game_date, format='%Y-%m-%d')

combined_df$game_year <- format(combined_df$game_date, '%Y')


outing_data_mlb <- statcast %>% 
  select(game_date, game_pk, pitcher, game_year) %>% 
  distinct()

# outing_data <- statcast %>%
#   group_by(pitcher, game_year, game_date, game_pk) %>%
#   summarize(outing_count = n(), .groups = 'drop')

combined_df <- combined_df %>%
  filter(`matchup.pitcher.id` %in% outing_data_mlb$pitcher) %>%
  arrange(`matchup.pitcher.id`, game_date)

#renaming_combined_df pitcher id and selecting appropriate columns
combined_df_small <- combined_df %>% 
  rename('pitcher' = matchup.pitcher.id) %>% 
  select(game_pk, game_date, game_year, pitcher) %>% 
  mutate(game_year = as.numeric(game_year)) %>% 
  distinct()


# statcast_w_rest <- outing_data %>%
#   arrange(game_date, game_pk, pitcher) %>%
#   group_by(pitcher, game_year) %>%
#   mutate(days_of_rest = ifelse(is.na(lag(game_date)), NA, as.numeric(game_date - lag(game_date)) - 1)) #%>%
# 
# aaa_outings <- combined_df %>%
#   group_by(`matchup.pitcher.id`, game_year, game_date) %>%
#   summarize(outing_count = n(), .groups = 'drop') %>%
#   mutate(aaa_game_date = game_date) %>%
#   rename(pitcher = `matchup.pitcher.id`)


complete_rest_df <- bind_rows(outing_data_mlb, combined_df_small)


all_outings <- complete_rest_df %>%
  arrange(game_date) %>%
  group_by(pitcher, game_year) %>% 
  mutate(days_of_rest = ifelse(is.na(lag(game_date)), 3, as.numeric(game_date - lag(game_date)) - 1)) %>% 
  ungroup()

# accounting for multiple back-to-back games
final_rest_df <- all_outings %>% 
  group_by(pitcher, game_year) %>% 
  mutate(days_of_rest = case_when(
    days_of_rest == 0 & lag(days_of_rest,1) == 0 ~ days_of_rest - 1,
    days_of_rest == 0 & lag(days_of_rest,1) == 0 & lag(days_of_rest, 2) == 0 ~ days_of_rest - 2,
    days_of_rest == 0 & lag(days_of_rest,1) == 0 & lag(days_of_rest, 2) == 0 & lag(days_of_rest, 3) == 0 ~ days_of_rest - 3,
    .default = days_of_rest
    )) %>% 
  ungroup()


write_csv(final_rest_df, 'rest_days.csv')
