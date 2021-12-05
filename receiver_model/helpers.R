### Helpers

# Set a theme for plots
ggplot2::theme_set(ggplot2::theme_bw() + 
                     ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", 
                                                                       size = 36/ggplot2::.pt, hjust = 0), 
                                    plot.subtitle = ggplot2::element_text(face = "italic", 
                                                                          size = 30/ggplot2::.pt), 
                                    plot.caption = ggplot2::element_text(face = "italic", 
                                                                         size = 20/ggplot2::.pt), 
                                    strip.background = ggplot2::element_blank(), 
                                    strip.text = ggplot2::element_text(face = "bold", 
                                                                       size = 24/ggplot2::.pt, hjust = 0), 
                                    panel.grid.minor = ggplot2::element_blank(), 
                                    panel.border = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), 
                                    axis.text = ggplot2::element_text(size = 24/ggplot2::.pt), 
                                    axis.title = ggplot2::element_text(face = "bold", size = 26/ggplot2::.pt), 
                                    legend.title = ggplot2::element_text(face = "bold", size = 26/ggplot2::.pt), 
                                    legend.text = ggplot2::element_text(size = 24/ggplot2::.pt)))

# Save predictions and workflow
grid_control <- tune::control_grid(
  # save_pred = TRUE,
                                   save_workflow = TRUE,
                                   extract = tune::extract_model,
                                   verbose = TRUE)

# Helper function for looking at model recipe pre-processing
prep_juice <- function(x) recipes::juice(recipes::prep(x))

# Helper function for binding predictions onto data
augment.workflow <- function(x, newdata, ...) {
  stats::predict(x, newdata, ...) %>%
    dplyr::bind_cols(newdata)
}

# Helper function for finding number of NAs in each column of dataframe
summarise_na <- function(tbl) {
  tbl %>% 
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.)))) %>% 
    t()
}

# EDA
summarise_target <- function(tbl) {
  tbl %>%
    dplyr::summarise(games = dplyr::n(),
                     tot_next_PPFD_pts = sum(next_PPFD_pts),
                     .groups = "drop") %>%
    dplyr::mutate(next_PPFD_pts = tot_next_PPFD_pts / games,
                  low = qbeta(.025, tot_next_PPFD_pts + .5, games - tot_next_PPFD_pts + .5),
                  high = qbeta(.975, tot_next_PPFD_pts + .5, games - tot_next_PPFD_pts + .5))
}

plot_continuous <- function(tbl, metric) {
  tbl %>% 
    ggplot(aes({{ metric }}, next_PPFD_pts)) +
    geom_point(aes(size = games)) +
    geom_line() +
    geom_ribbon(aes(ymin = low, ymax = high), 
                alpha = .2) +
    scale_size_continuous(range = c(0.5, 2.5),
                          labels = scales::comma) 
}

plot_categorical <- function(tbl, metric) {
  tbl %>% 
    ggplot(aes({{ metric }}, next_PPFD_pts)) +
    geom_point(aes(size = games)) +
    geom_errorbar(aes(ymin = low, ymax = high)) 
}

# Function to get moving average of a dynamic window from 5 - 41 games
wt_mov_avg <- function(var, weight, window, type, moving = T) {
  if (length(weight) == 1 & is.na(weight[1])) {
    return(var)
  }
  if (length(weight) == 1 & weight[1] == 1) {
    weight <- rep(1, length(var))
  }
  
  if (moving) {
    max <- length(var)-1
    
    if (max > 1) {
      dplyr::case_when(
        window == 5 ~ pracma::movavg(var*weight, n = pmin(max, 5), type = type)/
          pracma::movavg(weight, n = pmin(max, 5), type = type),
        window == 6 ~ pracma::movavg(var*weight, n = pmin(max, 6), type = type)/
          pracma::movavg(weight, n = pmin(max, 6), type = type),
        window == 7 ~ pracma::movavg(var*weight, n = pmin(max, 7), type = type)/
          pracma::movavg(weight, n = pmin(max, 7), type = type),
        window == 8 ~ pracma::movavg(var*weight, n = pmin(max, 8), type = type)/
          pracma::movavg(weight, n = pmin(max, 8), type = type),
        window == 9 ~ pracma::movavg(var*weight, n = pmin(max, 9), type = type)/
          pracma::movavg(weight, n = pmin(max, 9), type = type),
        window == 10 ~ pracma::movavg(var*weight, n = pmin(max, 10), type = type)/
          pracma::movavg(weight, n = pmin(max, 10), type = type),
        window == 11 ~ pracma::movavg(var*weight, n = pmin(max, 11), type = type)/
          pracma::movavg(weight, n = pmin(max, 11), type = type),
        window == 12 ~ pracma::movavg(var*weight, n = pmin(max, 12), type = type)/
          pracma::movavg(weight, n = pmin(max, 12), type = type),
        window == 13 ~ pracma::movavg(var*weight, n = pmin(max, 13), type = type)/
          pracma::movavg(weight, n = pmin(max, 13), type = type),
        window == 14 ~ pracma::movavg(var*weight, n = pmin(max, 14), type = type)/
          pracma::movavg(weight, n = pmin(max, 14), type = type),
        window == 15 ~ pracma::movavg(var*weight, n = pmin(max, 15), type = type)/
          pracma::movavg(weight, n = pmin(max, 15), type = type),
        window == 16 ~ pracma::movavg(var*weight, n = pmin(max, 16), type = type)/
          pracma::movavg(weight, n = pmin(max, 16), type = type),
        window == 17 ~ pracma::movavg(var*weight, n = pmin(max, 17), type = type)/
          pracma::movavg(weight, n = pmin(max, 17), type = type),
        window == 18 ~ pracma::movavg(var*weight, n = pmin(max, 18), type = type)/
          pracma::movavg(weight, n = pmin(max, 18), type = type),
        window == 19 ~ pracma::movavg(var*weight, n = pmin(max, 19), type = type)/
          pracma::movavg(weight, n = pmin(max, 19), type = type),
        window == 20 ~ pracma::movavg(var*weight, n = pmin(max, 20), type = type)/
          pracma::movavg(weight, n = pmin(max, 20), type = type),
        window == 21 ~ pracma::movavg(var*weight, n = pmin(max, 21), type = type)/
          pracma::movavg(weight, n = pmin(max, 21), type = type),
        window == 22 ~ pracma::movavg(var*weight, n = pmin(max, 22), type = type)/
          pracma::movavg(weight, n = pmin(max, 22), type = type),
        window == 23 ~ pracma::movavg(var*weight, n = pmin(max, 23), type = type)/
          pracma::movavg(weight, n = pmin(max, 23), type = type),
        window == 24 ~ pracma::movavg(var*weight, n = pmin(max, 24), type = type)/
          pracma::movavg(weight, n = pmin(max, 24), type = type),
        window == 25 ~ pracma::movavg(var*weight, n = pmin(max, 25), type = type)/
          pracma::movavg(weight, n = pmin(max, 25), type = type),
        window == 26 ~ pracma::movavg(var*weight, n = pmin(max, 26), type = type)/
          pracma::movavg(weight, n = pmin(max, 26), type = type),
        window == 27 ~ pracma::movavg(var*weight, n = pmin(max, 27), type = type)/
          pracma::movavg(weight, n = pmin(max, 27), type = type),
        window == 28 ~ pracma::movavg(var*weight, n = pmin(max, 28), type = type)/
          pracma::movavg(weight, n = pmin(max, 28), type = type),
        window == 29 ~ pracma::movavg(var*weight, n = pmin(max, 29), type = type)/
          pracma::movavg(weight, n = pmin(max, 29), type = type),
        window == 30 ~ pracma::movavg(var*weight, n = pmin(max, 30), type = type)/
          pracma::movavg(weight, n = pmin(max, 30), type = type),
        window == 31 ~ pracma::movavg(var*weight, n = pmin(max, 31), type = type)/
          pracma::movavg(weight, n = pmin(max, 31), type = type),
        window == 32 ~ pracma::movavg(var*weight, n = pmin(max, 32), type = type)/
          pracma::movavg(weight, n = pmin(max, 32), type = type),
        window == 33 ~ pracma::movavg(var*weight, n = pmin(max, 33), type = type)/
          pracma::movavg(weight, n = pmin(max, 33), type = type),
        window == 34 ~ pracma::movavg(var*weight, n = pmin(max, 34), type = type)/
          pracma::movavg(weight, n = pmin(max, 34), type = type),
        window == 35 ~ pracma::movavg(var*weight, n = pmin(max, 35), type = type)/
          pracma::movavg(weight, n = pmin(max, 35), type = type),
        window == 36 ~ pracma::movavg(var*weight, n = pmin(max, 36), type = type)/
          pracma::movavg(weight, n = pmin(max, 36), type = type),
        window == 37 ~ pracma::movavg(var*weight, n = pmin(max, 37), type = type)/
          pracma::movavg(weight, n = pmin(max, 37), type = type),
        window == 38 ~ pracma::movavg(var*weight, n = pmin(max, 38), type = type)/
          pracma::movavg(weight, n = pmin(max, 38), type = type),
        window == 39 ~ pracma::movavg(var*weight, n = pmin(max, 39), type = type)/
          pracma::movavg(weight, n = pmin(max, 39), type = type),
        window == 40 ~ pracma::movavg(var*weight, n = pmin(max, 40), type = type)/
          pracma::movavg(weight, n = pmin(max, 40), type = type),
        window == 41 ~ pracma::movavg(var*weight, n = pmin(max, 41), type = type)/
          pracma::movavg(weight, n = pmin(max, 41), type = type)
      )
    } else {
      sum(var*weight)/sum(weight)
    }
  } else {
    pracma::movavg(var*weight, n = 10, type = type)/
      pracma::movavg(weight, n = 10, type = type)
  }
}

# Helper function to clean model names
clean_model_names <- function(col) {
  dplyr::case_when(
    col == "xg_pred_pts" ~ "XGBoost",
    col == "lin_pred_pts" ~ "Lasso",
    col == "proj_PPFD_pts" ~ "Market",
    col == "ensemble_pred_pts" ~ "Ensemble")
}

# Helper function to collapse injury type string columns
clean_injuries <- function(col) {
  dplyr::case_when(
    col %in% c("not injury related - resting player", 
               "not injury related - personal matter",
               "not injury related - personal ",
               "not injury related -- resting veteran",
               "not injury related - discipline",
               "not injury related - resting p",
               "rest", "resting veteran", "personal",
               "travel after trade", "notinjuryrelated",
               "not football related", "not-injury related",
               "non injury related") ~ "not injury related",
    stringr::str_detect(col, "suspension ") ~ "suspension",
    col %in% c("coremuscle", "abdominal", "abdomen", "oblique", 
               "core", "core muscle injury", "core muscle") ~ "core",
    col %in% c("thigh", "thighs", "quadricep", "quad", "quadriceps") ~ "quadricep",
    col %in% c("feet", "foot") ~ "foot",
    col %in% c("hand", "finger", "thumb") ~ "hand",
    col %in% c("bicep", "biceps", "tricep", "triceps") ~ "arm",
    col %in% c("pectoral", "chest") ~ "chest",
    col %in% c("rib", "ribs", "rib cage") ~ "ribs",
    col %in% c("head", "concussion") ~ "concussion",
    col %in% c("achillies", "achilles") ~ "achilles",
    stringr::str_detect(col, "knee") ~ "knee",
    stringr::str_detect(col, "ankle") ~ "ankle",
    stringr::str_detect(col, "hamstring") ~ "hamstring",
    TRUE ~ col) %>% 
    stringr::str_squish(.)
}

# Expected yards after catch
add_xyac2 <- function(pbp, summarize = T) {
  if (nrow(pbp) == 0) {
    user_message("Nothing to do. Return passed data frame.", "info")
  } else {
    # testing only
    # pbp <- g
    
    pbp <- pbp %>% dplyr::select(-tidyselect::any_of(drop.cols.xyac))
    
    # for joining at the end
    pbp <- pbp %>%
      dplyr::mutate(index = 1:dplyr::n())
    
    # prepare_xyac_data helper function shown below
    passes <- prepare_xyac_data(pbp) %>%
      dplyr::filter(.data$valid_pass == 1, .data$distance_to_goal != 0)
    
    if (!nrow(passes) == 0) {
      user_message("Computing xyac...", "todo")
      join_data <- passes %>%
        dplyr::select(
          "index", "distance_to_goal", "season", "week", "home_team", "posteam", "roof",
          "half_seconds_remaining", "down", "ydstogo",
          "posteam_timeouts_remaining", "defteam_timeouts_remaining",
          "original_spot" = "yardline_100", "original_ep" = "ep", "air_epa", "air_yards"
        ) %>%
        dplyr::mutate(
          down = as.integer(.data$down),
          ydstogo = as.integer(.data$ydstogo),
          original_ydstogo = .data$ydstogo
        ) %>%
        dplyr::select("index":"ydstogo", "original_ydstogo", dplyr::everything())
      
      xyac_vars <-
        stats::predict(
          fastrmodels::xyac_model,
          as.matrix(passes %>% xyac_model_select())
        ) %>%
        tibble::as_tibble() %>%
        dplyr::rename(prob = "value") %>%
        dplyr::bind_cols(
          tibble::tibble(
            "yac" = rep_len(-5:70, length.out = nrow(passes) * 76),
            "index" = rep(passes$index, times = rep_len(76, length.out = nrow(passes)))
          ) %>%
            dplyr::left_join(join_data, by = "index") %>%
            dplyr::mutate(
              half_seconds_remaining = dplyr::if_else(
                .data$half_seconds_remaining <= 6,
                0,
                .data$half_seconds_remaining - 6
              )
            )
        ) %>%
        dplyr::group_by(.data$index) %>%
        dplyr::mutate(
          max_loss = dplyr::if_else(.data$distance_to_goal < 95, -5, .data$distance_to_goal - 99),
          max_gain = dplyr::if_else(.data$distance_to_goal > 70, 70, .data$distance_to_goal),
          cum_prob = cumsum(.data$prob),
          prob = dplyr::case_when(
            # truncate probs at loss greater than max loss
            .data$yac == .data$max_loss ~ .data$cum_prob,
            # same for gains bigger than possible
            .data$yac == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
            TRUE ~ .data$prob
          ),
          # get end result for each possibility
          yardline_100 = .data$distance_to_goal - .data$yac
        ) %>%
        dplyr::filter(.data$yac >= .data$max_loss, .data$yac <= .data$max_gain) %>%
        dplyr::select(-.data$cum_prob) %>%
        dplyr::mutate(
          posteam_timeouts_pre = .data$posteam_timeouts_remaining,
          defeam_timeouts_pre = .data$defteam_timeouts_remaining,
          gain = .data$original_spot - .data$yardline_100,
          turnover = dplyr::if_else(.data$down == 4 & .data$gain < .data$ydstogo, as.integer(1), as.integer(0)),
          down = dplyr::if_else(.data$gain >= .data$ydstogo, 1, .data$down + 1),
          ydstogo = dplyr::if_else(.data$gain >= .data$ydstogo, 10, .data$ydstogo - .data$gain),
          # possession change if 4th down failed
          down = dplyr::if_else(.data$turnover == 1, as.integer(1), as.integer(.data$down)),
          ydstogo = dplyr::if_else(.data$turnover == 1, as.integer(10), as.integer(.data$ydstogo)),
          # flip yardline_100 and timeouts for turnovers
          yardline_100 = dplyr::if_else(.data$turnover == 1, as.integer(100 - .data$yardline_100), as.integer(.data$yardline_100)),
          posteam_timeouts_remaining = dplyr::if_else(
            .data$turnover == 1,
            .data$defeam_timeouts_pre,
            .data$posteam_timeouts_pre
          ),
          defteam_timeouts_remaining = dplyr::if_else(
            .data$turnover == 1,
            .data$posteam_timeouts_pre,
            .data$defeam_timeouts_pre
          ),
          # ydstogo can't be bigger than yardline
          ydstogo = dplyr::if_else(.data$ydstogo >= .data$yardline_100, as.integer(.data$yardline_100), as.integer(.data$ydstogo))
        ) %>%
        dplyr::ungroup() %>%
        nflfastR::calculate_expected_points() %>%
        dplyr::group_by(.data$index) %>%
        dplyr::mutate(
          ep = dplyr::case_when(
            .data$yardline_100 == 0 ~ 7,
            .data$turnover == 1 ~ -1 * .data$ep,
            TRUE ~ ep
          ),
          epa = .data$ep - .data$original_ep,
          wt_epa = .data$epa * .data$prob,
          wt_yardln = .data$yardline_100 * .data$prob,
          med = dplyr::if_else(
            cumsum(.data$prob) > .5 & dplyr::lag(cumsum(.data$prob) < .5), .data$yac, as.integer(0)
          )) %>% 
        dplyr::ungroup()
      
      if (isTRUE(summarize)) {
        xyac_vars <- xyac_vars %>%
          dplyr::group_by(.data$index) %>%
          dplyr::summarise(
            xyac_epa = sum(.data$wt_epa) - dplyr::first(.data$air_epa),
            xyac_mean_yardage = (dplyr::first(.data$original_spot) - dplyr::first(.data$air_yards)) - sum(.data$wt_yardln),
            xyac_median_yardage = max(.data$med),
            xyac_success = sum((.data$ep > .data$original_ep) * .data$prob),
            xyac_fd = sum((.data$gain >= .data$original_ydstogo) * .data$prob)
          ) %>%
          dplyr::ungroup()
        
        pbp <- pbp %>%
          dplyr::left_join(xyac_vars, by = "index") %>%
          dplyr::select(-.data$index)
      } else {
        pbp <- pbp %>%
          dplyr::left_join(xyac_vars %>% 
                             dplyr::select(dplyr::any_of(c("index", "wt_epa",
                                                           "original_spot", "med",
                                                           "ep", "original_ep", "prob",
                                                           "gain", "original_ydstogo"))),
                           by = "index") %>%
          dplyr::select(-.data$index)
      }
      
      message_completed("added xyac variables")
    } else { # means no valid pass plays in the pbp
      pbp <- pbp %>%
        dplyr::mutate(
          xyac_epa = NA_real_,
          xyac_mean_yardage = NA_real_,
          xyac_median_yardage = NA_real_,
          xyac_success = NA_real_,
          xyac_fd = NA_real_
        ) %>%
        dplyr::select(-.data$index)
      user_message("No non-NA values for xyac calculation detected. xyac variables set to NA", "info")
    }
  }
  
  return(pbp)
}
