### Modeling 
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(tune)))
suppressMessages(suppressWarnings(library(broom)))

# Source helper functions
source("receiver_model/helpers.R")

mod_path <- paste0("data/model_dataset_", 5, "_", 10, "_", 'e', ".rds")
model_dataset <- readRDS(mod_path)

# Find the current season as default
season <- as.numeric(substr(Sys.Date(), start = 1, stop = 4))
month <- as.numeric(substr(Sys.Date(), start = 6, stop = 7))
current_season <- ifelse(month >= 7, season, season - 1)

option_list <- list(
  make_option("--tune_lin", type="logical", default = TRUE,
              help="Tune linear model"),
  make_option("--tune_xg", type="logical", default = TRUE,
              help="Tune XGBoost model"),
  make_option("--log", type="logical", default = FALSE,
              help="Log response")
)
opt <- parse_args(OptionParser(option_list=option_list))

cat("Points per first down fantasy model...\n")

# Set metric as rmse, mae
mset <- yardstick::metric_set(yardstick::rmse)

mod_path <- paste0("data/model_dataset_", 5, "_", 10, "_", 'e', ".rds")
model_dataset <- readRDS(mod_path)

# model_dataset %>% 
#   count(next_season, next_week) %>% View()

# Split the data into train and test
set.seed(123)
spl <- rsample::initial_split(model_dataset %>% dplyr::filter(current == 0), strata = prev_season)
train <- rsample::training(spl)
test <- rsample::testing(spl)

# Create 5 folds for cross-validation
train_fold <- train %>%
  rsample::vfold_cv(v = 5)

# Naive RMSE (predict mean PPFD for test as baseline)
naive <- train %>% 
  dplyr::summarise(points = mean(next_PPFD_pts)) %>% 
  dplyr::pull(points)

# Naive RMSE: 5.507
naive_rmse <- test %>% 
  dplyr::mutate(naive = naive) %>% 
  yardstick::rmse(next_PPFD_pts, naive) 

cat(paste0("Points per first down naive RMSE: ", naive_rmse$.estimate, "\n"))

# train %>% 
#   mutate(new = 0.5*practice_status_num + 5*report_status_num,
#          location = ifelse(location == "away", 1, 0)) %>% 
#   tidyr::pivot_longer(cols = c(targets:offense_pct, offense_snaps, consec_missed, exp_racr, change_team,
#                                report_status_num, practice_status_num, on_injury_report, active,
#                                next_week, cur_PPFD_pts, diff_PPFD_pts, new, injury, pass_oe_pred, location,
#                                PPFD_allowed_pred, PPFD_allowed_target_pred),
#                       names_to = "metric",
#                       values_to = "value") %>% 
#   # mutate(value = log(value + 0.01)) %>% 
#   # filter(!is.na(value)) %>% 
#   group_by(metric) %>%
#   summarise(broom::tidy(cor.test(next_PPFD_pts, value))) %>%
#   arrange(desc(estimate)) %>% 
#   print(n = Inf) %>% View()

train %>% summarise_na()

# model_dataset %>% 
#   count(inj_type_num, report_status_num) %>% View()
# 
# model_dataset %>% 
#   count(type, report_status)

# model_dataset %>% 
#   filter(current == 0) %>% 
#   group_by(active, type) %>% 
#   summarise(player_games = n(),
#             no_play = mean(no_play),
#             dnp = mean(dnp)) %>% View()
# 
# train %>% 
#   group_by(target_share = (target_share %/% 0.01) * 0.01) %>% 
#   summarise_target() %>% 
#   plot_continuous(metric = target_share)
# 
# model_dataset %>% 
#   count(inj_type, primary_injury) %>% View()
# 
# train %>% 
#   mutate(racr = pmin(25, racr)) %>% 
#   group_by(racr = (racr %/% 0.1) * 0.1) %>% 
#   summarise_target() %>% 
#   plot_continuous(metric = racr)

# train %>% 
#   group_by(report_status) %>% 
#   summarise_target() %>% 
#   plot_categorical(metric = report_status)
# 
# model_dataset %>% count(type) %>% View()

# Create model recipe
rec <- recipes::recipe(next_PPFD_pts ~ diff_PPFD_pts + diff_yards + exp_PPFD_pts + PPFD_pts + 
                         exp_first_downs + exp_touchdowns + exp_yards + exp_catches + 
                         rank + yards + target_share + air_yards_share + wpor + exp_racr +
                         next_career_game + team_implied_total + spread_line + 
                         PPFD_allowed_pred + PPFD_allowed_target_pred +
                         pass_oe_pred + offense_pct + offense_snaps + change_team + 
                         consec_missed + active + type + injury +
                         report_status + practice_status_num + primary_injury,
                       data = train) %>% 
  # Do not predict on `Doubtful` and `Out` players
  recipes::step_filter(!report_status %in% c("Doubtful", "Out"),
                       type %in% c("None", "Questionable", "Probable", "C19",  "day-to-day"),
                       active == 1) %>% # Filter only for active players
  recipes::step_rm(active, type, report_status, change_team) 

# Log the response variable
if (isTRUE(opt$log)) {
  rec <- rec %>% 
    recipes::step_log(next_PPFD_pts, offset = 1, base = 2)
}

lin_rec <- rec %>% 
  recipes::step_other(primary_injury, threshold = tune::tune()) %>% 
  recipes::step_dummy(primary_injury) %>% 
  recipes::step_ns(rank, offense_pct, offense_snaps, 
                   next_career_game, spread_line, yards,
                   exp_yards, exp_first_downs, exp_touchdowns)

xg_rec <- rec %>% 
  recipes::step_rm(diff_PPFD_pts, diff_yards, primary_injury)

# View model data
xg_rec %>% prep_juice()

# xg_rec %>% 
#   prep_juice() %>% 
#   tidyr::pivot_longer(cols = everything()) %>% 
#   ggplot(aes((value))) +
#   geom_histogram() +
#   facet_wrap(~ name, scales = "free")

# Create a workflow
lin_wf <- workflows::workflow() %>%
  workflows::add_recipe(lin_rec) %>% 
  workflows::add_model(parsnip::linear_reg(penalty = tune::tune()) %>% 
                         parsnip::set_engine("glmnet"))

xg_wf <- workflows::workflow() %>%
  workflows::add_recipe(xg_rec) %>%
  workflows::add_model(parsnip::boost_tree("regression",
                                           mtry = tune::tune(),
                                           tree_depth = tune::tune(),
                                           trees = tune::tune(),
                                           learn_rate = tune::tune()) %>% 
                         parsnip::set_engine("xgboost",
                                             early_stopping_rounds = 50))
# Tune hyperparamaters
lin_tune <- lin_wf %>%
  tune::tune_grid(train_fold,
                  grid = tidyr::crossing(penalty = 10 ^ seq(-6, -0.5, .05),
                                         threshold = c(seq(0.014, 0.026, by = 0.003))
                  ),
                  metrics = mset,
                  control = grid_control)

xg_tune <- xg_wf %>%
  tune::tune_grid(train_fold,
                  grid = tidyr::crossing(mtry = c(3, 4, 5),
                                         trees = seq(200, 1300, 50),
                                         learn_rate = c(0.0096, 0.015),
                                         tree_depth = c(5, 6, 7)),
                  metrics = mset,
                  control = grid_control)

# Analyze results
# tune::autoplot(lin_tune)
# tune::autoplot(xg_tune)

# Save metrics
path <- paste0(ifelse(isTRUE(opt$log), "_metrics_log.rds", "_metrics.rds"))
saveRDS(lin_tune %>% 
          tune::collect_metrics(), paste0("data/lin_tune", path))

saveRDS(xg_tune %>% 
          tune::collect_metrics(), paste0("data/xg_tune", path))

# Read in metrics
lin_metrics <- readRDS("data/lin_tune_metrics.rds")

lin_wf_best <- lin_wf %>%
  tune::finalize_workflow(parameters = lin_metrics %>%
                            dplyr::filter(.metric == "rmse") %>%
                            dplyr::arrange(mean) %>%
                            head(1))

xg_metrics <- readRDS("data/xg_tune_metrics.rds")

xg_wf_best <- xg_wf %>%
  tune::finalize_workflow(parameters = xg_metrics %>%
                            dplyr::filter(.metric == "rmse") %>%
                            dplyr::arrange(mean) %>%
                            head(1))

# Fit to the data using best parameters
lin_fit_best <- lin_wf_best %>%
  parsnip::fit(train)

lin_test_preds <- lin_fit_best %>%
  augment(test)

lin_test_preds %>%
  yardstick::rmse(next_PPFD_pts, .pred)

# Fit to the data using best parameters
xg_fit_best <- xg_wf_best %>%
  parsnip::fit(train)

xg_test_preds <- xg_fit_best %>%
  augment(test)

xg_test_preds %>%
  yardstick::rmse(next_PPFD_pts, .pred)

## Log
# Linear: CV: 1.15; Test: 4.43
# XG: CV: 1.15; Test: 4.40
## Raw
# Linear: CV: 4.46; Test: 4.43
# XG: CV: 4.44; Test: 4.41

cat("Completed tuning of linear and XGBoost models")

# # Best CV: 4.440
# best_cv <- xg_tune %>% 
#   tune::collect_metrics() %>% 
#   dplyr::filter(.metric == "rmse") %>% 
#   dplyr::arrange(mean) %>% 
#   head(1)
# 
# best_params <- tune::select_best(xg_tune, metric = "rmse")
# 
# cat(paste0("Best params: mtry = ", best_params$mtry, ", trees = ", best_params$trees, ", learn_rate = ", best_params$learn_rate,
#            "\nBest CV RMSE: ", best_cv$mean, "\n"))
# 
# # Select best cross validated log loss parameters (mtry = 11, trees = 700, learn_rate = 0.02)
# xg_wf_best <- xg_wf %>%
#   tune::finalize_workflow(best_params)
# 
# # Fit to the data using best parameters
# xg_fit_best <- xg_wf_best %>%
#   parsnip::fit(train)
# 
# # XGBoost importance
# importances <- xgboost::xgb.importance(model = xg_fit_best$fit$fit$fit)
# 
# importances %>%
#   dplyr::mutate(Feature = forcats::fct_reorder(Feature, Gain)) %>%
#   ggplot(aes(Gain, Feature)) +
#   geom_point() +
#   labs(title = "Importance of each term in xgboost",
#        subtitle = "Fantasy football model")
# 
# # Testing RMSE: 4.39
# xg_wf_best %>%
#   tune::last_fit(spl, metrics = mset) %>%
#   tune::collect_metrics()  
# 
# saveRDS(xg_wf_best, "data/xg_wf.rds")
# xg_wf_best <- readRDS("data/xg_wf.rds")
# 
# # Training RMSE: 4.48
# lin_tune %>% 
#   tune::collect_metrics() %>% 
#   dplyr::filter(.metric == "rmse") %>% 
#   dplyr::arrange(mean)
# 
# lin_wf_best <- lin_wf %>%
#   tune::finalize_workflow(tune::select_best(lin_tune, metric = "rmse"))
# 
# # Testing RMSE: 4.41
# lin_wf_best %>%
#   tune::last_fit(spl, metrics = mset) %>%
#   tune::collect_metrics()  
# 
# lin_fit_best <- lin_wf_best %>% 
#   parsnip::fit(train)
# 
# lin_fit_best$fit$fit %>% 
#   broom::tidy() %>% View()
# 
# saveRDS(lin_wf_best, "data/lin_wf.rds")
lin_wf_best <- readRDS("data/lin_wf.rds")

lin_fit_best <- lin_wf_best %>% 
  parsnip::fit(train)

lin_coef_plot <- lin_fit_best$fit$fit %>%
  broom::tidy() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = forcats::fct_reorder(term, estimate)) %>% 
  ggplot(aes(estimate, term, color = estimate == 0)) +
  geom_point() +
  labs(title = "Lasso model coefficients",
       x = "Estimate",
       y = NULL,
       color = "Zeroed out")

# XGBoost importance
importances <- xgboost::xgb.importance(model = xg_fit_best$fit$fit$fit)

xg_fip_plot <- importances %>%
  dplyr::mutate(Feature = forcats::fct_reorder(Feature, Gain)) %>%
  ggplot(aes(Gain, Feature)) +
  geom_point() +
  labs(title = "XGBoost importance",
       y = NULL)

coef_plot <- cowplot::plot_grid(lin_coef_plot, xg_fip_plot, nrow = 1, align = "hv")

ggsave(filename = "plots/coef_plot.png", coef_plot, dpi = 700, height = 5*0.9, width = 8)


