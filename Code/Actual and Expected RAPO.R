library(tidyverse)

appearance_data <- read.csv("appearance_data_w_RAPO.csv")
game_lookup <- read.csv("game_pk_lookup.csv")

appearance_data <- appearance_data %>%
  left_join(game_lookup %>% select("game_pk", "game_year"), by = 'game_pk')

appearance_data <- appearance_data %>%
  mutate(
    actual_rapo = runs_bs - runs_allowed,
    expected_rapo = runs_bs - exp_runs_allowed
  )

asfasfsfsfs <- appearance_data %>%
  mutate(actual_rapo = actual_rapo - mean(actual_rapo, na.rm = TRUE),
         expected_rapo = expected_rapo - mean(expected_rapo, na.rm = TRUE)
  ) %>%  
  group_by(pitcher, game_pk, game_year) %>% 
  mutate(q5 = quantile(actual_rapo, probs = 0.05, na.rm = TRUE),
         q95 = quantile(actual_rapo, probs = 0.95, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(actual_rapo < q95, actual_rapo > q5)

season_metrics <- appearance_data %>%
 # mutate(actual_rapo = actual_rapo - mean(actual_rapo, na.rm = TRUE),
 #        expected_rapo = expected_rapo - mean(expected_rapo, na.rm = TRUE)
 # ) %>%  
  group_by(pitcher, game_pk, game_year) %>% 
  reframe(actual_rapo = sum(actual_rapo),
          expected_rapo = sum(expected_rapo),
          outs_made = sum(outs_made)) %>% 
  group_by(pitcher, game_year) %>%
  summarise(
    appearances = n(),
    actual_rapo_season = sum(actual_rapo) / sum(outs_made) * 3,
    expected_rapo_season = sum(expected_rapo) / sum(outs_made) * 3
  ) %>% 
  filter(appearances > 25)

base_sit_rsae <- appearance_data %>% 
  group_by(initial_base_sit, outs_start) %>% 
  reframe(run_bs = mean(runs_bs))


season_metrics <- subset(season_metrics, season_metrics$actual_rapo_season != Inf)
season_metrics <- subset(season_metrics, season_metrics$expected_rapo_season != -Inf)


season_metrics <- season_metrics %>% left_join(pitcher_id_lookup, by = 'pitcher')


season_metrics_actual <- season_metrics %>% 
  arrange(desc(game_year), desc(actual_rapo_season))


season_metric_expected <- season_metrics %>% 
  arrange(desc(game_year), desc(expected_rapo_season))


write_csv(season_metrics_actual, 'season_metrics_actual.csv')
write_csv(season_metric_expected, 'season_metrics_expected.csv')




appearance_data_5_95 <- appearance_data %>%
  #mutate(actual_rapo = actual_rapo - mean(actual_rapo, na.rm = TRUE),
  #       expected_rapo = expected_rapo - mean(expected_rapo, na.rm = TRUE)
  #) %>%  
  group_by(pitcher, game_year) %>% 
  mutate(q5 = quantile(actual_rapo, probs = 0.05, na.rm = TRUE),
          q95 = quantile(actual_rapo, probs = 0.95, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(actual_rapo < q95, actual_rapo > q5)


appearance_data_5_95 <- appearance_data_5_95 %>%  
  group_by(pitcher, game_pk, game_year) %>% 
  reframe(actual_rapo = sum(actual_rapo),
          expected_rapo = sum(expected_rapo),
          outs_made = sum(outs_made)) %>% 
  group_by(pitcher, game_year) %>%
  summarise(
    appearances = n(),
    outs_made = sum(outs_made),
    actual_rapo_season = sum(actual_rapo) / sum(outs_made) * 3,
    expected_rapo_season = sum(expected_rapo) / sum(outs_made) * 3
  ) %>% 
  filter(appearances > 25)

pitcher_id_lookup <- read_csv('pitcher_id_lookup.csv')

appearance_data_5_95 <- appearance_data_5_95 %>% left_join(pitcher_id_lookup, by = 'pitcher')

appearance_data_5_95 <- subset(appearance_data_5_95, appearance_data_5_95$actual_rapo_season != Inf)
appearance_data_5_95 <- subset(appearance_data_5_95, appearance_data_5_95$actual_rapo_season != -Inf)

appearance_data_5_95 <- appearance_data_5_95 %>% 
  arrange(desc(game_year), desc(actual_rapo_season))


write_csv(appearance_data_5_95, 'rapo_5th_to_95th_quantile_performance.csv')


appearance_data_5_95 <- appearance_data_5_95 %>% 
  arrange(desc(game_year), desc(expected_rapo_season))

appearance_data_5_95 <- appearance_data_5_95 %>% 
  mutate(diff = actual_rapo_season - expected_rapo_season)


appearance_data_5_95
