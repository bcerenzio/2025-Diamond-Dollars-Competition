
library(tidyverse)
library(dplyr)

rest_data <- read_csv("C:/Users/Aidan/Downloads/rest_days.csv")
pitcher_lookup <- read_csv("C:/Users/Aidan/Downloads/pitcher_id_lookup.csv")

rest_data <- read_csv('rest_days.csv')
pitcher_lookup <- read_csv('pitcher_id_lookup.csv')


rest_filtered <- rest_data %>% filter(days_of_rest <= 10)

rest_filtered <- rest_filtered %>% 
  group_by(pitcher, game_year) %>% 
  filter(game_date != min(game_date)) %>% 
  ungroup()

rest_filtered <- rest_filtered %>%
  mutate(
    rest_label = case_when(
      days_of_rest == -1 ~ "B2B2B",
      days_of_rest == 0 ~ "B2B",
      days_of_rest == 1 ~ "OneDay",
      days_of_rest == 2 ~ "TwoDay",
      days_of_rest == 3 ~ "ThreeDay",
      days_of_rest == 4 ~ "FourDay",
      days_of_rest == 5 ~ "FiveDay",
      days_of_rest == 6 ~ "SixDay",
      days_of_rest == 7 ~ "SevenDay",
      days_of_rest == 8 ~ "EightDay",
      days_of_rest == 9 ~ "NineDay",
      days_of_rest == 10 ~ "TenDay"
    )
  )

rest_summary <- rest_filtered %>%
  filter(game_year > 2022) %>% 
  group_by(pitcher, rest_label) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = rest_label, values_from = count, values_fill = list(count = 0))

rest_summary <- rest_summary %>% replace(is.na(.), 0)

rest_summary <- rest_summary %>%
  rowwise() %>%
  mutate(
    total = B2B2B + B2B + OneDay + TwoDay + ThreeDay + FourDay + FiveDay + 
      SixDay + SevenDay + EightDay + NineDay + TenDay, 
    B2B2B_prop = B2B2B / total,
    B2B_prop = B2B / total,
    OneDay_prop = OneDay / total,
    TwoDay_prop = TwoDay / total,
    ThreeDay_prop = ThreeDay / total,
    FourDay_prop = FourDay / total,
    FiveDay_prop = FiveDay / total,
    SixDay_prop = SixDay / total,
    SevenDay_prop = SevenDay / total,
    EightDay_prop = EightDay / total,
    NineDay_prop = NineDay / total,
    TenDay_prop = TenDay / total
  ) %>%
  ungroup() 


entropy_term <- function(probability) {
  if (probability == 0) {
    return(0)
  } else {
    return(probability * log(probability))
  }
}

rest_summary <- rest_summary %>%
  rowwise() %>%
  mutate(
    entropy = -(
      entropy_term(B2B2B_prop) +
        entropy_term(B2B_prop) +
        entropy_term(OneDay_prop) +
        entropy_term(TwoDay_prop) +
        entropy_term(ThreeDay_prop) +
        entropy_term(FourDay_prop) +
        entropy_term(FiveDay_prop) +
        entropy_term(SixDay_prop) +
        entropy_term(SevenDay_prop) +
        entropy_term(EightDay_prop) +
        entropy_term(NineDay_prop) +
        entropy_term(TenDay_prop)
    )
  ) %>%
  ungroup()

rest_summary <- rest_summary %>%
  left_join(pitcher_lookup, by = "pitcher")

# rest_summary <- rest_summary %>%
#   select(pitcher, game_year,player_name, entropy, total, B2B2B, B2B, OneDay, TwoDay, ThreeDay, 
#          FourDay, FiveDay, SixDay, SevenDay, EightDay, NineDay, TenDay, everything())

rest_summary <- rest_summary %>%
  select(pitcher,player_name, entropy, total, B2B2B, B2B, OneDay, TwoDay, ThreeDay, 
         FourDay, FiveDay, SixDay, SevenDay, EightDay, NineDay, TenDay, everything())

write.csv(rest_summary,"D:/RStudio/Projects/Arizona2025PC/entropy_rest_day.csv")


appearance_data_5_95 <- read_csv('rapo_5th_to_95th_quantile_performance.csv')

appearance_data_5_95_rest <- appearance_data_5_95 %>% 
  filter(game_year >= 2023) %>% 
  group_by(pitcher) %>% 
  reframe(
          actual_rapo_season = weighted.mean(actual_rapo_season, appearances),
          expected_rapo_season = weighted.mean(expected_rapo_season, appearances),
          appearances = sum(appearances))

appearance_data_5_95_new <- appearance_data_5_95_rest %>%  
  left_join(rest_summary, by = c('pitcher'))
# 
# appearance_data_5_95_new <- appearance_data_5_95_new %>% 
#   group_by(pitcher, player_name) %>% 
#   reframe(outs_made = sum(outs_made),
#           actual_rapo_season = sum(actual_rapo_season),
#           expected_rapo_season = sum(expected_rapo_season))
#   mutate(rapo_times_entropy = actual_rapo_season * entropy,
#          xrapo_times_entropy = expected_rapo_season * entropy)

appearance_data_5_95_new <- appearance_data_5_95_new %>% 
  mutate(rapo_entropy = actual_rapo_season*entropy) %>% 
  left_join(pitcher_id_lookup, by = 'pitcher')


write_csv(appearance_data_5_95_new, 'RAPO & xRAPO with Entropy Incorporated.csv')
