### Model predict
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(broom)))
suppressMessages(suppressWarnings(library(parsnip)))

# Source helper functions
source("receiver_model/helpers.R")

mod_path <- paste0("data/model_dataset_", 5, "_", 10, "_", 'e', ".rds")
model_dataset <- readRDS(mod_path)

# Find the current season as default
season <- as.numeric(substr(Sys.Date(), start = 1, stop = 4))
month <- as.numeric(substr(Sys.Date(), start = 6, stop = 7))
current_season <- ifelse(month >= 7, season, season - 1)

option_list <- list(
  make_option("--rerun_lin", type="logical", default = FALSE,
              help="Re-run linear model"),
  make_option("--rerun_xg", type="logical", default = FALSE,
              help="Re-run XGBoost model"),
  make_option("--rerun_ensemble", type="logical", default = FALSE,
              help="Re-run ensemble model")
)
opt <- parse_args(OptionParser(option_list=option_list))

train_mod <- function(sn, wk, rerun_lin, rerun_xg) {
  
  lin_wf_best <- readRDS("data/lin_wf.rds")
  xg_wf_best <- readRDS("data/xg_wf.rds")
  
  usethis::ui_info("Training receiving fantasy football model for {sn}, week {wk}...")
  
  # Split into train and test
  train <- model_dataset %>% 
    dplyr::filter(!(next_season == 2013 & next_week <= 5), # Too sparse data with rolling averages
                  !(next_season == sn & next_week >= wk), # Not the current week we are predicting on
                  next_season <= sn, # Not future seasons
                  !report_status %in% c("Doubtful", "Out"),
                  type %in% c("None", "Questionable", "Probable", "C19",  "day-to-day"),
                  active == 1,
                  current == 0)
  
  test <- model_dataset %>% 
    dplyr::filter(!(next_season == 2013 & next_week <= 5),
                  next_season == sn,
                  next_week == wk) %>% 
    dplyr::mutate(active = ifelse(current == 1, 1, active))
  
  if (nrow(test) == 0) {
    return(dplyr::tibble())
  }
  
  if (isTRUE(rerun_lin) | length(list.files("data/ff_mods/lm/", pattern = paste0("lin_mod_", sn, "wk_", wk))) == 0) {
    
    lin_fit <- lin_wf_best %>% 
      parsnip::fit(train %>% 
                     dplyr::filter(active == 1)) 
    
    # train %>% 
    #   summarise(across(everything(), ~ sum(is.na(.)))) %>% t()
    # coef <- workflows::pull_workflow_fit(lin_fit) %>% broom::tidy()
    
    # Save model
    saveRDS(lin_fit, paste0("data/ff_mods/lm/lin_mod_", sn, "wk_", wk, ".rds"))
  } else {
    lin_fit <- readRDS(paste0("data/ff_mods/lm/lin_mod_", sn, "wk_", wk, ".rds"))
  }
  
  if (isTRUE(rerun_xg) | length(list.files("data/ff_mods/xg/", pattern = paste0("xg_mod_", sn, "wk_", wk))) == 0) {
    
    xg_fit <- xg_wf_best %>% 
      parsnip::fit(train)
    
    # Save model
    saveRDS(xg_fit, paste0("data/ff_mods/xg/xg_mod_", sn, "wk_", wk, ".rds"))
  } else {
    xg_fit <- readRDS(paste0("data/ff_mods/xg/xg_mod_", sn, "wk_", wk, ".rds"))
  }
  
  augment(lin_fit, test) %>% 
    dplyr::select(game_id = next_game_id, player_id, team,
                  pass_oe_pred, report_status, practice_status, inj_type_num, 
                  active, no_play, dnp, current,
                  spread_line, team_implied_total, team_rest_diff, team_qb_id,
                  lin_pred_pts = .pred, PPFD_pts = next_PPFD_pts) %>% 
    dplyr::left_join(augment(xg_fit, test) %>% 
                       dplyr::select(game_id = next_game_id, player_id, xg_pred_pts = .pred),
                     by = c("game_id", "player_id"))
}

ftsy_predictions <- dplyr::tibble(season = 2016:current_season) %>% 
  tidyr::crossing(week = 1:18) %>% 
  dplyr::mutate(preds = purrr::map2(season, week, ~ train_mod(.x, .y, 
                                                              rerun_lin = opt$rerun_lin,
                                                              rerun_xg = opt$rerun_xg))) %>% 
  tidyr::unnest(preds) %>% 
  dplyr::mutate(dplyr::across(dplyr::ends_with("_pred_pts"), ~ pmax(0, .)), # Cut off at 0
                dplyr::across(dplyr::ends_with("_pred_pts"), ~ 
                                dplyr::if_else(report_status %in% c("Out", "Doubtful") | inj_type_num == 4 | active == 0,
                                               0, ., missing = .)))

# model_dataset %>%
#   anti_join(ftsy_predictions,
#             by = c("next_game_id" = "game_id",
#                    "player_id")) %>%
#   count(next_season)

ensemble_mod <- function(sn, wk, rerun) {
  
  usethis::ui_info("Training PPFD ensemble model for {sn}, week {wk}...")
  
  # Split into train and test
  train <- ftsy_predictions %>% 
    dplyr::filter(!(season == 2013 & week <= 5), # Too sparse data with rolling averages
                  !(season == sn & week >= wk), # Not the current week we are predicting on
                  # season <= sn, # Not future seasons
                  !report_status %in% c("Doubtful", "Out"),
                  inj_type_num != 4,
                  current == 0)
  
  test <- ftsy_predictions %>% 
    dplyr::filter(!(season == 2013 & week <= 5),
                  season == sn,
                  week == wk) %>% 
    dplyr::mutate(active = ifelse(current == 1, 1, active))
  
  # Variables in ensemble
  vars <- c("lin_pred_pts", "xg_pred_pts", "team_implied_total", "spread_line")
  
  if (isTRUE(rerun) | length(list.files("data/ff_mods/ens/", pattern = paste0("ensemble_mod_", sn, "wk_", wk))) == 0) {
    ensemble_mod <- glmnet::cv.glmnet(x = as.matrix(train %>% dplyr::select(dplyr::all_of(vars))),
                                      y = train$PPFD_pts) 
    
    # ensemble_mod$glmnet.fit %>%
    #   broom::tidy() %>%
    #   filter(lambda == ensemble_mod$lambda.min)
    
    # Save model
    saveRDS(ensemble_mod, paste0("data/ff_mods/ens/ensemble_mod_", sn, "_wk_", wk, ".rds"))
  } else {
    ensemble_mod <- readRDS(paste0("data/ff_mods/ens/ensemble_mod_", sn, "_wk_", wk, ".rds"))
  }
  
  preds <- predict(ensemble_mod, 
                   as.matrix(test %>% dplyr::select(dplyr::all_of(vars))), 
                   s = "lambda.min") %>% 
    as.numeric()
  
  test %>% 
    dplyr::bind_cols(ensemble_pred_pts = preds) %>% 
    dplyr::select(-season, -week)
}

ensemble_predictions <- dplyr::tibble(season = 2016:current_season) %>% 
  tidyr::crossing(week = 1:18) %>% 
  dplyr::mutate(preds = purrr::map2(season, week, ~ ensemble_mod(sn = .x, 
                                                                 wk = .y,
                                                                 rerun = opt$rerun_ensemble))) %>% 
  tidyr::unnest(preds) %>% 
  dplyr::mutate(dplyr::across(dplyr::ends_with("_pred_pts"), ~ pmax(0, .)), # Cut off at 0
                dplyr::across(dplyr::ends_with("_pred_pts"), ~ 
                                dplyr::if_else(report_status %in% c("Out", "Doubtful") | inj_type_num == 4 | active == 0,
                                               0, ., missing = .)))

saveRDS(ensemble_predictions, "data/predictions.rds")
