library(tidyverse)



appearance_data_5_95_graph <- appearance_data_5_95 %>% 
  arrange(pitcher, game_year)


appearance_data_5_95_graph <- appearance_data_5_95_graph %>% 
  group_by(pitcher) %>% 
  mutate(prev_rapo = lag(actual_rapo_season),
         prev_xrapo = lag(expected_rapo_season)) %>% 
  ungroup()

appearance_data_5_95_graph %>% 
  ggplot(aes(prev_rapo, actual_rapo_season)) +
  geom_point(aes(size = appearances), alpha = 0.2) + 
  geom_abline(slope = 1, intercept = 0) +
  theme_classic() +
  scale_x_continuous(breaks = seq(-0.25,0.5, by = 0.25)) +
  scale_y_continuous(breaks = seq(-0.25,0.5, by = 0.25)) +
  annotate('text', x = -0.1, y = 0.45, label = 'p = 0.004**') +
  labs(x = 'RSAE of Previous Season',
       y = 'RSAE of Current Season',
       title = 'Stickiness of RSAE from Season to Season',
       size = '# of\nAppearances')


appearance_data_5_95_graph %>% 
  ggplot(aes(prev_rapo, actual_rapo_season)) +
  geom_point(aes(size = appearances), alpha = 0.2) + 
  geom_abline(slope = 1, intercept = 0) +
  theme_classic() +
  scale_x_continuous(breaks = seq(-0.25,0.5, by = 0.25)) +
  scale_y_continuous(breaks = seq(-0.25,0.5, by = 0.25)) +
  annotate('text', x = -0.1, y = 0.45, label = 'p = 0.004**') +
  labs(x = 'RSAE of Previous Season',
       y = 'RSAE of Current Season',
       title = 'Stickiness of RAPO from Season to Season',
       size = '# of\nAppearances')


corrr::correlate(appearance_data_5_95_graph$prev_rapo, appearance_data_5_95_graph$actual_rapo_season)  


summary(lm(actual_rapo_season ~ prev_rapo, data = appearance_data_5_95_graph))

appearance_data_5_95_graph %>% 
  ggplot(aes(prev_xrapo, expected_rapo_season)) +
  geom_point(aes(size = appearances), alpha = 0.2) + 
  geom_abline(slope = 1, intercept = 0) +
  theme_classic() +
  scale_x_continuous(breaks = seq(-0.25,0.5, by = 0.25)) +
  scale_y_continuous(breaks = seq(-0.25,0.5, by = 0.25)) +
  annotate('text', x = -0.1, y = 0.45, label = 'p = 0.0008**') +
  labs(x = 'xRSAE of Previous Season',
       y = 'xRSAE of Current Season',
       title = 'Stickiness of xRSAE from Season to Season',
       size = '# of\nAppearances')


season_saves_stat_new <- season_saves_stat %>% 
  arrange(pitcher, game_year)

season_saves_stat_new <- season_saves_stat_new %>% 
  group_by(pitcher) %>% 
  mutate(prev_saves_stat = lag(new_saves)) %>% 
  ungroup()

season_saves_stat_new %>% 
  mutate(batters_faced = case_when(
   pitcher == 642232 & game_year == 2024 ~ 401,
   .default = batters_faced
   )) %>% 
  ggplot(aes(prev_saves_stat, new_saves)) +
  geom_point(aes(size = batters_faced), alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic() +
  scale_x_continuous(breaks = seq(-100,100, by = 25), limits = c(-100,100)) +
  scale_y_continuous(breaks = seq(-100,100, by = 25), limits = c(-100,100)) +
  annotate('text', x = -75, y = 99, label = 'p = 0.06') +
  labs(
    title = 'LPAA Stickiness',
    x = 'Previous Season LPAA',
    y = 'Current Season LPAA',
    size = 'Batters Faced'
  )
  


summary(lm(new_saves ~ prev_saves_stat, data = season_saves_stat_new))
