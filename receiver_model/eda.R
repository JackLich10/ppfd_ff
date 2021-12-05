### EDA
library(ggplot2)

sets <- list.files("data/", pattern = "model_d")

model_dataset <- purrr::map_df(sets, function(s) {
  attrs <- s %>%
    stringr::str_remove(., "model_dataset_") %>%
    stringr::str_remove(., ".rds$") %>%
    dplyr::tibble(roll_type = .) %>%
    tidyr::separate(roll_type, sep = "_", into = c("min_g", "max_g", "roll_type"), convert = TRUE)
  readRDS(paste0("data/", s)) %>%
    dplyr::bind_cols(attrs)
})

metrics <- model_dataset %>%
  dplyr::filter(current == 0) %>% 
  tidyr::pivot_longer(cols = c(targets:offense_pct),
                      names_to = "metric",
                      values_to = "value") %>%
  dplyr::group_by(min_g, max_g, roll_type, metric) %>%
  dplyr::summarise(broom::tidy(cor.test(next_PPFD_pts, value)),
                   .groups = "drop") %>%
  dplyr::mutate(abs_value = abs(estimate))

rm(model_dataset)

metrics %>%
  dplyr::group_by(min_g, max_g, roll_type) %>%
  dplyr::summarise(n = dplyr::n(),
                   estimate = sum(abs_value),
                   .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(estimate))

best_metrics <- metrics %>%
  group_by(metric) %>%
  slice_max(abs_value) %>%
  ungroup() %>%
  arrange(desc(abs_value))

# Plot
metrics %>%
  dplyr::filter(min_g == 5) %>%
  group_by(metric) %>% 
  mutate(best = ifelse(abs(estimate) == max(abs(estimate)), 1, 0)) %>% 
  ungroup() %>% 
  dplyr::mutate(metric = stringr::str_to_title(stringr::str_replace_all(metric, "_", " ")),
                metric = forcats::fct_reorder(metric, abs(estimate), .desc = TRUE),
                roll_type = dplyr::case_when(
                  roll_type == "e" ~ "Exponential",
                  roll_type == "r" ~ "Rolling",
                  roll_type == "s" ~ "Simple",
                  roll_type == "w" ~ "Weighted")) %>% 
  ggplot(aes(max_g, abs(estimate), color = roll_type)) +
  geom_line() +
  geom_point(aes(size = ifelse(best == 1, 2, 0.7))) +
  scale_size_identity() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Correlation of metric to next game PPFD fantasy points",
       subtitle = "Max game number refers to maximum window size of rolling average",
       x = "Max game window",
       y = "Absolute value of correlation coefficient",
       color = "Type of average")

ggsave(filename = "plots/predictor_metrics.png", .Last.value, dpi = 700, height = 5, width = 8)


# Read in final dataset
mod_path <- paste0("data/model_dataset_", 5, "_", 10, "_", 'e', ".rds")
model_dataset <- readRDS(mod_path)

# Distribution of target
model_dataset %>% 
  filter(!report_status %in% c("Doubtful", "Out"),
         type %in% c("None", "Questionable", "Probable", "C19",  "day-to-day"),
         active == 1, 
         current == 0) %>% 
  mutate(log_next_PPFD_pts = log(next_PPFD_pts + 1)) %>% 
  tidyr::pivot_longer(cols = dplyr::ends_with("next_PPFD_pts")) %>% 
  dplyr::mutate(name = stringr::str_to_title(stringr::str_replace_all(name, "_", " "))) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ name, scales = "free_x") +
  labs(title = "Distribution of points per first down fantasy points",
       x = NULL,
       y = NULL)

ggsave(filename = "plots/target_dist.png", .Last.value, dpi = 700, height = 5*0.5, width = 8)

# Team/game-level predictors
model_dataset_long <- model_dataset %>% 
  filter(current == 0) %>% 
  tidyr::pivot_longer(cols = c(next_career_game:offense_pct,
                               # pass_oe_pred, PPFD_allowed_pred, PPFD_allowed_target_pred,
                               # team_implied_total, spread_line,
                               consec_missed, cur_PPFD_pts),
                      names_to = "metric",
                      values_to = "value") %>% 
  dplyr::mutate(metric = stringr::str_to_title(stringr::str_replace_all(metric, "_", " ")))

model_dataset_long_team_stats <- model_dataset %>% 
  filter(current == 0) %>% 
  tidyr::pivot_longer(cols = c(pass_oe_pred, PPFD_allowed_pred, PPFD_allowed_target_pred,
                               team_implied_total, spread_line),
                      names_to = "metric",
                      values_to = "value") %>% 
  dplyr::mutate(metric = stringr::str_to_title(stringr::str_replace_all(metric, "_", " ")))


p1 <- model_dataset_long_team_stats %>% 
  dplyr::filter(metric %in% c("Spread Line", "Team Implied Total", "Ppfd Allowed Pred")) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ metric, scales = "free") +
  labs(title = "Distribution of team and game-level predictors",
       x = NULL,
       y = NULL)

p2 <- model_dataset_long_team_stats %>% 
  dplyr::filter(metric %in% c("Spread Line", "Team Implied Total", "Ppfd Allowed Pred")) %>% 
  group_by(metric, value = (value %/% 2) * 2) %>% 
  summarise(n = dplyr::n(),
            next_PPFD_pts = mean(next_PPFD_pts),
            .groups = "drop") %>% 
  ggplot(aes(value, next_PPFD_pts)) +
  # geom_abline(size = 1.1, color = "grey50", lty = 2) +
  geom_point(aes(size = n), show.legend = FALSE) +
  geom_smooth(aes(weight = n)) +
  scale_size_continuous(range = c(0.5, 2.5)) +
  facet_wrap(~ metric, scales = "free") +
  labs(title = "... and their relationship with next game PPFD fantasy points",
       x = NULL,
       y = "Next game PPFD points")

team_level_plot <- cowplot::plot_grid(p1, p2, nrow = 2, align = "hv")

ggsave(filename = "plots/team_level_predictors.png", team_level_plot, dpi = 700, height = 5*0.9, width = 8)

model_dataset %>% 
  filter(current == 0) %>% 
  ggplot(aes(next_career_game, next_PPFD_pts)) +
  geom_smooth()

player_level_plot <- model_dataset_long %>% 
  filter(!metric %in% c("Targets", "Air Yards", "Cur Ppfd Pts", 
                        "Exp Ppr Pts", "Ppr Pts", "Diff Yards", "Diff Ppfd Pts")) %>% 
  # filter(stringr::str_detect(metric, "Exp ")) %>% 
  # mutate(log_value = log(value + 0.01)) %>%
  # tidyr::pivot_longer(cols = c(value, log_value),
  #                     names_to = "log",
  #                     values_to = "value") %>% 
  ggplot(aes(value, next_PPFD_pts)) +
  # geom_abline(size = 1.1, color = "grey50", lty = 2) +
  geom_smooth() +
  facet_wrap(~ metric, scales = "free") +
  labs(title = "Player-level predictors relationship with next game PPFD fantasy points",
       x = NULL,
       y = NULL)

ggsave(filename = "plots/player_level_predictors.png", player_level_plot, dpi = 700, height = 5*0.9, width = 8)

model_dataset_long %>% 
  filter(!stringr::str_detect(metric, "exp_")) %>% 
  mutate(log_value = log(value + 0.01)) %>%
  # tidyr::pivot_longer(cols = c(value, log_value),
  #                     names_to = "log",
  #                     values_to = "value") %>% 
  ggplot(aes(value, next_PPFD_pts)) +
  geom_abline(size = 1.1, color = "grey50", lty = 2) +
  geom_smooth() +
  facet_wrap(~ metric, scales = "free")

model_dataset %>% 
  filter(current == 0) %>% 
  ggplot(aes(air_yards, next_PPFD_pts, color = factor(air_yards == 0))) +
  geom_point(alpha = 0.05) +
  geom_smooth()

# linear:
# targets, target_share, air_yards_share, wpor, exp_yards, exp_catches,





