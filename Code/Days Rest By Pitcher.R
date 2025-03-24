


rapo_new <- read_csv('appearance_data_w_RAPO.csv')
rest_days <- read_csv('rest_days.csv')
pitcher_id_lookup <- read_csv('pitcher_id_lookup.csv')


rapo_new <- rapo_new %>% 
  left_join(rest_days, by = c('pitcher','game_pk'))



rapo_w_rest <- rapo_new %>%
  filter(days_of_rest <= 10, game_year > 2022) %>%
  group_by(pitcher, game_year) %>% 
  filter(game_date != min(game_date)) %>% 
  ungroup() %>% 
  mutate(rapo = runs_bs - runs_allowed,
         xrapo = runs_bs - exp_runs_allowed) %>% 
  mutate(rapo = rapo - mean(rapo, na.rm = TRUE),
         xrapo = xrapo - mean(xrapo, na.rm = TRUE)) %>% 
  group_by(days_of_rest, pitcher) %>% 
  reframe(
    appearances = n(),
    outs_made = sum(outs_made, na.rm = TRUE),
    rapo = sum(rapo, na.rm =TRUE)/sum(outs_made, na.rm = TRUE)*3,
    xrapo = sum(xrapo, na.rm = TRUE)/sum(outs_made, na.rm = TRUE)*3,
    rapo_app = rapo/appearances,
    xrapo_app = xrapo/appearances
  ) %>% 
  arrange(days_of_rest, desc(rapo)) %>% 
  group_by(pitcher) %>% 
  filter(sum(appearances) > 15) %>% 
  ungroup()

rapo_w_rest <- rapo_w_rest %>% 
  left_join(pitcher_id_lookup, by = 'pitcher')

rapo_w_rest_bbb <- rapo_w_rest %>% 
  filter(days_of_rest < 0, outs_made > 5) %>% 
  arrange(desc(rapo))

write_csv(rapo_w_rest_bbb, 'rapo_w_rest_bbb.csv')

rapo_w_rest_bb <- rapo_w_rest %>% 
  filter(days_of_rest == 0, outs_made > 20) %>% 
  arrange(desc(rapo))

write_csv(rapo_w_rest_bb, 'rapo_w_rest_bb.csv')


rapo_w_rest_1_3 <- rapo_w_rest %>% 
  filter(between(days_of_rest, 1, 3), outs_made > 30) %>% 
  arrange(desc(rapo))

write_csv(rapo_w_rest_1_3, 'rapo_w_rest_1_3.csv')

write_csv(rapo_w_rest, 'rapo_w_rest.csv')
