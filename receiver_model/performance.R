# Read in market projections
market_proj <- purrr::map_dfr(2016:current_season, function(sn) {
  readRDS(paste0("data/market/proj_", sn, ".rds"))
})

# Read in predictions
predictions <- readRDS("data/predictions.rds")

joined_predictions <- predictions %>% 
  left_join(market_proj %>% 
              dplyr::rename(market_pred_pts = proj_PPFD_pts),
            by = c("season", "week", "player_id" = "gsis_id")) %>% 
  filter(!is.na(ensemble_pred_pts),
         !is.na(PPFD_pts))

joined_predictions_long <- joined_predictions %>% 
  tidyr::pivot_longer(cols = c(dplyr::ends_with("_pred_pts")),
                      names_to = "model",
                      values_to = "value") %>% 
  mutate(model = stringr::str_to_title(stringr::str_remove(model, "_pred_pts")))

joined_predictions_long %>% 
  group_by(model) %>% 
  summarise(player_games = dplyr::n(),
            rmse = scales::number(sqrt(mean((value - PPFD_pts)^2, na.rm = TRUE)), accuracy = 0.00001),
            .groups = "drop")

market_ensemble <- joined_predictions %>% 
  select(season, week, player_id, ends_with("_pred_pts"), PPFD_pts) %>% 
  crossing(weight = seq(0, 1, 0.005)) %>% 
  mutate(market_ensemble = ensemble_pred_pts*(1-weight) + market_pred_pts*(weight)) %>% 
  group_by(weight) %>% 
  summarise(n = n(),
            rmse = sqrt(mean((market_ensemble - PPFD_pts)^2, na.rm = T)))

market_ensemble %>% 
  ggplot(aes(weight, rmse)) +
  geom_line() +
  geom_vline(xintercept = market_ensemble$weight[market_ensemble$rmse == min(market_ensemble$rmse)],
             color = "red", lty = 2) +
  labs(title = "Weighting model with the market",
       x = "Market projection weight",
       y = "RMSE")

ggsave(filename = "plots/market_weight.png", .Last.value, dpi = 700, height = 5*0.9, width = 8)


joined_predictions_long %>% 
  group_by(model, value = (value %/% 0.5) * 0.5) %>% 
  summarise(player_games = n(),
            PPFD_pts = mean(PPFD_pts),
            .groups = "drop") %>% 
  ggplot(aes(value, PPFD_pts)) +
  geom_abline(color = "grey50", lty = 2, size = 1.1) +
  geom_point(aes(size = player_games)) +
  geom_smooth(aes(weight = player_games)) +
  scale_size_continuous(range = c(0.5, 2.5)) +
  facet_wrap(~ model) +
  labs(title = "Model diagnostics",
       x = "Predicted value",
       y = "Actual PPFD points",
       size = "Player-games")

ggsave(filename = "plots/model_diagnostics.png", .Last.value, dpi = 700, height = 5*0.9, width = 8)


joined_predictions %>% 
  tidyr::replace_na(list(market_pred_pts = 0)) %>% 
  tidyr::pivot_longer(cols = c(dplyr::ends_with("_pred_pts")),
                      names_to = "model",
                      values_to = "value") %>% 
  group_by(season, model) %>% 
  summarise(player_games = dplyr::n(),
            rmse = sqrt(mean((value - PPFD_pts)^2)),
            .groups = "drop") %>% 
  mutate(model = stringr::str_to_title(stringr::str_remove(model, "_pred_pts"))) %>% 
  ggplot(aes(season, rmse, color = model)) +
  geom_line() +
  geom_point(aes(size = player_games)) +
  scale_size_continuous(range = c(0.5, 2.5)) +
  labs(title = "Model performance by season",
       x = "NFL season",
       y = "RMSE",
       size = "Player-games",
       color = "Model")

ggsave(filename = "plots/model_performance.png", .Last.value, dpi = 700, height = 5*0.9, width = 8)


model_dataset %>% 
  filter(next_season >= 2016,
         active == 1,
         current == 0) %>% 
  select(season = next_season,
         week = next_week,
         game_id = next_game_id,
         player_id, active, type,
         PPFD_pts = next_PPFD_pts) %>% 
  left_join(ensemble_predictions %>% 
              select(season, week, game_id, player_id, team, current,
                     ends_with("_pred_pts")),
            by = c("season", "week", "game_id", "player_id")) %>% 
  left_join(market_proj %>% 
              select(-c(team, dplyr::starts_with("receiving_"))) %>% 
              rename(market_pred_pts = proj_PPFD_pts),
            by = c("player_id" = "gsis_id", "season", "week")) %>% 
  filter(is.na(market_pred_pts)) %>% View()
tidyr::pivot_longer(cols = c(dplyr::ends_with("_pred_pts")),
                    names_to = "model",
                    values_to = "value") %>% 
  group_by(season, model) %>% 
  summarise(player_games = dplyr::n(),
            rmse = sqrt(mean((value - PPFD_pts)^2)),
            .groups = "drop") %>% 
  ggplot(aes(season, rmse, color = model)) +
  geom_line() +
  geom_point(aes(size = player_games)) +
  scale_size_continuous(range = c(0.5, 2))

market_proj

rec_clean

orig <- readRDS("data/market/fantasypros_proj.rds")


split <- rec_clean %>% 
  bind_rows(orig %>% 
              inner_join(missing)) %>% 
  group_split(season)



missing <- orig %>% 
  filter(season > 2015) %>% 
  distinct(season, week, fantasypros_id) %>% 
  anti_join(market_proj %>% 
              distinct(season, week, fantasypros_id, gsis_id))

ensemble_predictions %>% 
  filter(current == 0) %>% 
  tidyr::pivot_longer(cols = c(dplyr::ends_with("_pred_pts")),
                      names_to = "model",
                      values_to = "value") %>% 
  group_by(season, model) %>% 
  summarise(player_games = dplyr::n(),
            rmse = sqrt(mean((value - PPFD_pts)^2)),
            .groups = "drop") %>% 
  ggplot(aes(season, rmse, color = model)) +
  geom_line() +
  geom_point(aes(size = player_games)) +
  scale_size_continuous(range = c(0.5, 2))

ensemble_predictions %>% count(season, week) %>% View()

# Current week projections
current_proj <- ensemble_predictions %>% 
  # filter(current == 1) %>% 
  filter(season == 2021, week == 11) %>% 
  left_join(rb_wr_te_key,
            by = c("player_id" = "gsis_id")) %>% 
  left_join(team_map %>% 
              select(team_abbr, team_logo_espn),
            by = c("team" = "team_abbr")) %>% 
  arrange(desc(ensemble_pred_pts)) %>% 
  mutate(rank = dplyr::row_number())

# Players model struggles to predict
ensemble_predictions %>% 
  filter(current == 0) %>% 
  tidyr::pivot_longer(cols = c(ends_with("_pred_pts")),
                      names_to = "model",
                      values_to = "value") %>% 
  group_by(season, player_id, model) %>% 
  summarise(player_games = dplyr::n(),
            rmse = sqrt(mean((value - PPFD_pts)^2)),
            .groups = "drop") %>% 
  left_join(rb_wr_te_key,
            by = c("player_id" = "gsis_id")) %>% View()

# Tyreek hill over time
ensemble_predictions %>% 
  filter(player_id == "00-0033040") %>% 
  tidyr::pivot_longer(cols = c(lin_pred_pts, xg_pred_pts, PPFD_pts),
                      names_to = "type",
                      values_to = "value") %>% 
  ggplot(aes(week, value, color = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ season)


# Table
current_proj %>% 
  head(25) %>% 
  select(rank, 
         # season, week, 
         team_logo_espn, position, headshot_url, full_name, 
         lin_pred_pts, xg_pred_pts, ensemble_pred_pts) %>% 
  gt::gt() %>% 
  gt::tab_header(title = gt::md('**Top 25 Projected PPFD Fantasy Points Recieving**'),
                 subtitle = gt::md("*Lasso, XGBoost, Ensemble models | 2021 Week 11*")) %>% 
  gt::cols_label(rank = 'RNK',
                 # season = 'Season',
                 # week = 'Week',
                 headshot_url = '',
                 position = '',
                 full_name = '',
                 team_logo_espn = '',
                 lin_pred_pts = 'LM',
                 xg_pred_pts = 'XGB',
                 ensemble_pred_pts = 'ENS') %>% 
  gt::tab_spanner(label = 'Projection', columns = c(dplyr::ends_with("_pred_pts"))) %>% 
  gt::fmt_number(columns = c(dplyr::ends_with("_pred_pts")),
                 decimals = 2) %>% 
  gt::text_transform(locations = gt::cells_body(c(team_logo_espn, headshot_url)),
                     fn = function(x) gt::web_image(url = x, height = 23)) %>% 
  gt::data_color(columns = c(dplyr::ends_with("_pred_pts")),
                 colors = scales::col_numeric(
                   palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
                   domain = NULL),
                 autocolor_text = F) %>% 
  gt::tab_source_note("Table: @jacklich10 | Data: @nflfastR") %>%
  gtExtras::gt_theme_538()


all_predictions <- ensemble_predictions %>% 
  select(season, week, game_id, team, player_id, current,
         active, no_play, dnp,
         ends_with("_pred_pts"), PPFD_pts) %>% 
  left_join(rb_wr_te_key,
            by = c("player_id" = "gsis_id")) %>% 
  left_join(market_proj %>% 
              filter(!is.na(fantasypros_id)) %>% 
              select(-c(player_name, team, pos)),
            by = c("fantasypros_id", "season", "week"))

all_predictions %>% 
  count(dnp, active, is.na(proj_PPFD_pts))

all_predictions %>% 
  filter(is.na(proj_PPFD_pts)) %>% View()

all_predictions_long <- all_predictions %>% 
  mutate(across(ends_with("_pred_pts"), ~ ifelse(dnp == 1 | active == 0, 0, .))) %>%
  inner_join(market_weeks,
             by = c("season", "week")) %>%
  # count(dnp, proj_PPFD_pts) %>% View()
  tidyr::replace_na(list(proj_PPFD_pts = 0)) %>%
  tidyr::pivot_longer(cols = c(ends_with("_pred_pts"), proj_PPFD_pts),
                      names_to = "model",
                      values_to = "value") %>% 
  mutate(model = clean_model_names(model))

model_dataset %>% 
  count(active, next_PPFD_pts != 0)

all_predictions_long %>% 
  group_by(season, model) %>% 
  summarise(player_games = n(),
            rmse = sqrt(mean((value-PPFD_pts)^2)),
            .groups = "drop") %>% 
  ggplot(aes(season, rmse, color = model)) +
  geom_line() +
  geom_point(aes(size = player_games)) +
  scale_size_continuous(range = c(0.5, 2))

all_predictions_long %>% 
  group_by(season, week, model) %>% 
  summarise(player_games = n(),
            rmse = sqrt(mean((value-PPFD_pts)^2)),
            .groups = "drop") %>% 
  ggplot(aes(week, rmse, color = model)) +
  geom_line() +
  geom_point(aes(size = player_games)) +
  scale_size_continuous(range = c(0.5, 2)) +
  facet_wrap(~ season)

all_predictions %>% 
  filter(current == 0) %>% 
  tidyr::pivot_longer(cols = c(ends_with("_pred_pts"), proj_PPFD_pts),
                      names_to = "model",
                      values_to = "value") %>% 
  mutate(model = clean_model_names(model)) %>% 
  group_by(model, value = (value %/% 0.5) * 0.5) %>% 
  summarise(player_games = dplyr::n(),
            PPFD_pts = mean(PPFD_pts),
            .groups = "drop") %>% 
  ggplot(aes(value, PPFD_pts)) +
  geom_abline(lty = 2, size = 1.1, color = "grey") +
  geom_point(aes(size = player_games)) +
  geom_smooth(aes(weight = player_games)) +
  scale_size_continuous(range = c(1, 3)) +
  facet_wrap(~ model) +
  labs(x = "Predicted PPFD fantasy points",
       y = "Actual PPFD fantasy points",
       size = "# of player games")

# Weeks with market predictions
market_weeks <- market_proj %>% 
  distinct(season, week)

market_proj %>% 
  filter(season == 2021, week == 11) %>% View()

market_proj %>% 
  filter(stringr::str_detect(player_name, "Alshon Jeffery")) %>% View()



all_predictions %>% 
  filter(current == 0) %>% 
  inner_join(market_weeks,
             by = c("season", "week")) %>%
  filter(!is.na(proj_PPFD_pts)) %>% 
  # tidyr::replace_na(list(proj_PPFD_pts = 0)) %>%
  tidyr::pivot_longer(cols = c(ends_with("_pred_pts"), proj_PPFD_pts),
                      names_to = "model",
                      values_to = "value") %>% 
  group_by(season, week, model) %>% 
  summarise(player_games = n(),
            rmse = sqrt(mean((value-PPFD_pts)^2)),
            .groups = "drop") %>% 
  ggplot(aes(week, rmse, color = model)) +
  geom_line() +
  geom_point(aes(size = player_games)) +
  scale_size_continuous(range = c(0.5, 2)) +
  facet_wrap(~ season)

model_dataset %>% 
  select(next_season, next_week, next_game_id, player_id, team) %>% 
  anti_join(ftsy_predictions,
            by = c("next_game_id" = "game_id", "player_id")) %>% View()

ftsy_predictions %>% 
  summarise_na()

ensemble_predictions %>% 
  summarise_na()


# rec <- readr::read_csv("data/rec.csv")
# 
# rec_clean <- rec %>% 
#   rename(pff_id = player_id,
#          receiving_tds = rec_td,
#          receiving_yds = rec_yards,
#          receiving_rec = rec) %>% 
#   filter(week <= 17) %>% 
#   left_join(pff_map,
#             by = c("pff_id")) %>% 
#   filter(!is.na(gsis_id)) %>% 
#   inner_join(rb_wr_te_key, by = "gsis_id") %>% 
#   mutate(proj_PPFD_pts = receiving_tds*6 + 0.1*receiving_yds + 0.22*receiving_rec + 0.03*receiving_yds) %>% 
#   select(season, week, gsis_id, fantasypros_id, player_name  = full_name, pos = position,
#          team = current_team, receiving_rec, receiving_yds, receiving_tds, proj_PPFD_pts)


products <- readr::read_csv("~/Desktop/Duke/Duke Senior Year/CS316/project/team-wamyj-mini-amazon-skeleton/db/generated/Products.csv",
                            col_names = c("id", 'name', 'cat_name', 'description', 'img', 'available'))

tibble(id = seq(0, 600, by = 1)) %>% 
  anti_join(products) %>% View()

