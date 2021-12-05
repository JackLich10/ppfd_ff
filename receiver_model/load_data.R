# Load libraries
suppressMessages(suppressWarnings(library(nflreadr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(tune)))
suppressMessages(suppressWarnings(library(pracma)))

option_list <- list(
  make_option("--start_season", type="integer", default = 2013,
              help="Start season"),
  make_option("--live", type="logical", default = FALSE,
              help="Live"),
  make_option("--min_g", type="integer", default = 5,
              help="Min moving average window"),
  make_option("--max_g", type="integer", default = 10,
              help="Max moving average window"),
  make_option("--roll_type", type="character", default = "e",
              help="Type of moving average")
)
opt <- parse_args(OptionParser(option_list=option_list))
# Rscript receiver_model/load_data.R --min_g 5 --max_g 8 --roll_type 'e'

if (!opt$roll_type %in% c("s", "t", "w", "m", "e", "r")) {
  cat("--roll_type must be one of `'s', 't', 'w', 'm', 'e', 'r'`")
  quit(status = 49)
}

# # Player IDs
# diggs <- "00-0031588"
# crowder <- "00-0031941"
# tay_hill <- "00-0033357"
# ertz <- "00-0030061"
# dk <- "00-0035640"

# Source helper functions
source("receiver_model/helpers.R")
# Model mutations
source("https://raw.githubusercontent.com/nflverse/nflfastR/665bbd56631e23bcf9d023e11f999f8710138bf9/R/helper_add_nflscrapr_mutations.R")
# xyac
source("https://raw.githubusercontent.com/nflverse/nflfastR/ef163986e629237211fc0444a546781635052aa7/R/helper_add_xyac.R")
# Utility functions
source("https://raw.githubusercontent.com/nflverse/nflfastR/7def369c0ebb8058dae829688bda7ff1ed7b81ea/R/utils.R")

# Clear cache
nflreadr::.clear_cache()

# Find the current season as default
season <- as.numeric(substr(Sys.Date(), start = 1, stop = 4))
month <- as.numeric(substr(Sys.Date(), start = 6, stop = 7))
current_season <- ifelse(month >= 7, season, season - 1)

# Seasons to load in
sns <- opt$start_season:current_season

cat(paste0("Loading data for fantasy football receiving model data set for 2013-", max(sns), "...\n"))

# Load in team map
team_map <- nflreadr::load_teams()

# Load in games
games <- nflreadr::load_schedules(seasons = sns) %>% 
  dplyr::mutate(home_team = nflreadr::clean_team_abbrs(home_team),
                away_team = nflreadr::clean_team_abbrs(away_team),
                away_implied_total = (total_line - spread_line)/2,
                home_implied_total = total_line - away_implied_total)
# Find key of weeks that matter for fantasy
week_key <- games %>% 
  dplyr::filter(game_type == "REG") %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(last_week = max(week)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(bad_weeks = purrr::map2(last_week, 23, seq)) %>% 
  tidyr::unnest(bad_weeks) %>% 
  dplyr::select(season, bad_weeks)
# Double games to find team schedules
schedule <- games %>% 
  dplyr::select(-c(home_score, away_score, total, overtime:espn, away_qb_name:stadium,
                   dplyr::ends_with("_moneyline"), dplyr::ends_with("_odds"))) %>% 
  dplyr::mutate(home_rest_diff = home_rest - away_rest,
                away_rest_diff = -home_rest_diff) %>% 
  nflreadr::clean_homeaway()
# Find current season-week
current_sn_wk <- games %>% 
  dplyr::filter(!is.na(result),
                weekday == "Monday") %>% 
  dplyr::filter(season == max(season)) %>% 
  dplyr::filter(week == max(week)) %>% 
  dplyr::distinct(season, week) 
# Find bye weeks
bye_weeks <- schedule %>% 
  dplyr::filter(game_type == "REG") %>%
  dplyr::group_by(season, team) %>% 
  dplyr::mutate(delta_week = week - dplyr::lag(week, default = 0),
                prev_game_id = dplyr::lag(game_id)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(delta_week == 2) %>% 
  dplyr::transmute(season, team, prev_game_id, next_game_id = game_id, bye = 1)

if (isTRUE(opt$live)) {
  # Load play-by-play (just current season, read in other data already saved)
  pbp <- nflreadr::load_pbp(seasons = current_season) %>%
    dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam),
                  play_type == "pass" | play_type == "run" | penalty == 1, 
                  qb_kneel != 1) %>% 
    dplyr::mutate(posteam = nflreadr::clean_team_abbrs(posteam),
                  defteam = nflreadr::clean_team_abbrs(defteam))
} else {
  # Load play-by-play (just current season, read in other data already saved)
  pbp <- nflreadr::load_pbp(seasons = opt$start_season:current_season) %>%
    dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam),
                  play_type == "pass" | play_type == "run" | penalty == 1, 
                  qb_kneel != 1) %>% 
    dplyr::mutate(posteam = nflreadr::clean_team_abbrs(posteam),
                  defteam = nflreadr::clean_team_abbrs(defteam))
}
# Find team pass rates by game
pass_rates <- pbp %>% 
  dplyr::group_by(game_id, season, week, team = posteam) %>% 
  dplyr::summarise(plays = dplyr::n(),
                   pass_oe = mean(pass_oe, na.rm = TRUE)/100,
                   .groups = "drop") %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::mutate(game_number = dplyr::row_number()) %>% 
  dplyr::ungroup()
# Find last game of stats for each team
last_games_wstats <- pbp %>% 
  dplyr::filter(season == current_season) %>% 
  dplyr::group_by(posteam) %>% 
  dplyr::slice_max(week, n = 1, with_ties = FALSE) %>% 
  dplyr::ungroup() %>% 
  dplyr::transmute(team = posteam, game_id, season, week, last_game = 1)
# Find next current game for each team
next_team_games <- schedule %>% 
  dplyr::left_join(last_games_wstats,
                   by = c("game_id", "season", "week", "team")) %>% 
  dplyr::group_by(team) %>% 
  tidyr::fill(last_game, .direction = "up") %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(is.na(last_game)) %>% 
  dplyr::group_by(team) %>% 
  dplyr::slice_min(gameday, n = 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(team, game_id, season, week)
# Filter down to only what we need
pbp <- pbp %>% 
  dplyr::filter(!is.na(receiver_id))

# Load injuries
injuries <- nflreadr::load_injuries(seasons = sns) %>% 
  dplyr::arrange(date_modified) %>% 
  dplyr::mutate(
    practice_status = dplyr::case_when(
      practice_status %in% c("Did Not Participate In Practice", "\n    ") ~ "DNP",
      practice_status == "Limited Participation in Practice" ~ "Limited",
      practice_status == "Full Participation in Practice" ~ "Full"),
    dplyr::across(dplyr::ends_with("y_injury"), stringr::str_to_lower),
    dplyr::across(dplyr::ends_with("y_injury"), ~ stringr::str_remove_all(., "(right )|(left )")),
    dplyr::across(where(is.character), clean_injuries),
    dplyr::across(dplyr::ends_with("y_injury"), ~ stringr::str_remove(., "s$")),
    dplyr::across(c(season, week), as.numeric)) %>% 
  tidyr::unite(primary_injury, report_primary_injury, practice_primary_injury, na.rm = TRUE) %>% 
  tidyr::unite(secondary_injury, report_secondary_injury, practice_secondary_injury, na.rm = TRUE) %>% 
  dplyr::mutate(dplyr::across(c(primary_injury, secondary_injury), ~ stringr::str_remove(., "_.*")),
                dplyr::across(c(primary_injury, secondary_injury), ~ dplyr::na_if(., "")),
                team = nflreadr::clean_team_abbrs(team)) %>% 
  dplyr::select(season, week, player_id = gsis_id, team,
                report_status, practice_status, primary_injury, secondary_injury) %>% 
  dplyr::distinct(season, week, player_id, .keep_all = TRUE)
# Load PFR injuries
pfr_inj <- readRDS("~/Desktop/RStudio/Fantasy_Football/data/pfr_inj/pfr_injuries.rds") %>% 
  dplyr::mutate(inj_type = stringr::str_remove(inj_type, "(right )|(right)"),
                inj_type = stringr::str_remove(inj_type, "(left )|(left)"),
                inj_type = clean_injuries(inj_type),
                inj_type_num = dplyr::case_when(
                  type == "Probable" ~ 1,
                  type %in% c("Questionable", "day-to-day", "C19") ~ 2,
                  type == "Doubtful" ~ 3,
                  TRUE ~ 4))
# pfr_inj %>% count(inj_type) %>% View()
# Load rosters
roster <- nflreadr::load_rosters(seasons = sns) %>% 
  dplyr::mutate(team = nflreadr::clean_team_abbrs(team))
# Summarize player information
player_info <- roster %>% 
  dplyr::filter(!is.na(gsis_id)) %>% 
  dplyr::arrange(season) %>% 
  dplyr::group_by(gsis_id) %>% 
  dplyr::summarise(dplyr::across(c(position, full_name, birth_date, height, headshot_url),
                                 ~ collapse::fmode(., na.rm = TRUE)),
                   current_team = dplyr::last(team),
                   .groups = "drop")
# Filter for only the positions we care about
roster_ids <- roster %>%
  dplyr::filter(!(is.na(gsis_id) & is.na(pfr_id))) %>% 
  # select only variables we need
  dplyr::select(season, team, position, full_name, gsis_id, pfr_id)

# Load player ID map
fantasy_ids <- nflreadr::load_ff_playerids() %>% 
  # dplyr::filter(position %in% c("RB", "WR", "TE", "FB") | name %in% c("Taysom Hill")) %>% 
  dplyr::select(gsis_id, pfr_id, fantasypros_id, name, position, merge_name, team) %>% 
  dplyr::filter(!(is.na(gsis_id) & is.na(pfr_id))) %>% 
  dplyr::mutate(team = nflreadr::clean_team_abbrs(team),
                pfr_id = dplyr::case_when(
                  gsis_id == "00-0037007" ~ "JonePa00",
                  gsis_id == "00-0029435" ~ "JohnDe22",
                  TRUE ~ pfr_id),
                fantasypros_id = dplyr::case_when(
                  gsis_id == "00-0029435" ~ "11654",
                  TRUE ~ fantasypros_id))

# Create a player key from the two sources to map `gsis_id` to `pfr_id`
player_key <- roster_ids %>% 
  dplyr::distinct(gsis_id, pfr_id) %>% 
  dplyr::left_join(fantasy_ids %>% 
                     dplyr::filter(!is.na(gsis_id)) %>% 
                     dplyr::distinct(gsis_id, pfr_id, fantasypros_id),
                   by = c("gsis_id")) %>% 
  dplyr::mutate(pfr_id = ifelse(is.na(pfr_id.x), pfr_id.y, pfr_id.x)) %>% 
  dplyr::left_join(fantasy_ids %>% 
                     dplyr::filter(!is.na(pfr_id)) %>% 
                     dplyr::distinct(gsis_id, pfr_id, fantasypros_id),
                   by = c("pfr_id")) %>% 
  dplyr::mutate(gsis_id = ifelse(is.na(gsis_id.x), gsis_id.y, gsis_id.x),
                fantasypros_id = ifelse(is.na(fantasypros_id.x), fantasypros_id.y, fantasypros_id.x)) %>% 
  dplyr::select(-c(gsis_id.x, gsis_id.y, pfr_id.y, pfr_id.x, fantasypros_id.x, fantasypros_id.y)) %>% 
  dplyr::distinct() %>% 
  # Manual pfr_id additions
  dplyr::mutate(
    pfr_id = dplyr::case_when(
      is.na(pfr_id) & gsis_id == "00-0028532" ~ "MorgJo01",
      is.na(pfr_id) & gsis_id == "00-0031118" ~ "BrowPh00",
      is.na(pfr_id) & gsis_id == "00-0029848" ~ "WillNi02",
      gsis_id == "00-0037007" ~ "JonePa00",
      gsis_id == "00-0029435" ~ "JohnDe22",
      TRUE ~ pfr_id))

# Save player key
saveRDS(player_key, "~/Desktop/RStudio/Fantasy_Football/data/player_key.rds")

# Key for just skill position players
rb_wr_te_key <- player_key %>% 
  dplyr::left_join(player_info,
                   by = "gsis_id") %>% 
  dplyr::filter(position %in% c("RB", "WR", "TE", "FB") | full_name %in% c("Taysom Hill", "Jamal Agnew"))

# Find the weeks a player is active
actives <- purrr::map_dfr(sns, function(sn) {
  readRDS(paste0("~/Desktop/RStudio/Fantasy_Football/data/actives/actives_", sn, ".rds"))
}) %>% 
  dplyr::select(season, gsis_id, weeks_active_reg) %>% 
  tidyr::separate_rows(weeks_active_reg, sep = ";", convert = TRUE) %>% 
  dplyr::filter(!is.na(weeks_active_reg)) %>% 
  dplyr::mutate(active = 1)

# Load snaps
snaps <- nflreadr::load_snap_counts(seasons = sns) %>% 
  # Fix for 2020 super bowl, bad team names
  dplyr::mutate(team = nflreadr::clean_team_abbrs(team),
                team = dplyr::case_when(
                  # game_id == "2014_21_NE_SEA" & team == "SEA" ~ "NE",
                  # game_id == "2014_21_NE_SEA" & team == "NE" ~ "SEA",
                  # game_id == "2015_21_CAR_DEN" & team == "DEN" ~ "CAR",
                  # game_id == "2015_21_CAR_DEN" & team == "CAR" ~ "DEN",
                  # game_id == "2016_21_NE_ATL" & team == "NE" ~ "ATL",
                  # game_id == "2016_21_NE_ATL" & team == "ATL" ~ "NE",
                  # game_id == "2017_21_PHI_NE" & team == "NE" ~ "PHI",
                  # game_id == "2017_21_PHI_NE" & team == "PHI" ~ "NE",
                  # game_id == "2018_21_NE_LA" & team == "NE" ~ "LA",
                  # game_id == "2018_21_NE_LA" & team == "LA" ~ "NE",
                  # game_id == "2019_21_SF_KC" & team == "SF" ~ "KC",
                  # game_id == "2019_21_SF_KC" & team == "KC" ~ "SF",
                  game_id == "2020_21_KC_TB" & team == "KC" ~ "TB",
                  game_id == "2020_21_KC_TB" & team == "TB" ~ "KC",
                  TRUE ~ team)
  ) %>% 
  # Find snap percentage better than just two decimals
  dplyr::group_by(game_id, team) %>% 
  dplyr::mutate(tot_offense_snaps = max(offense_snaps),
                tot_defense_snaps = max(defense_snaps)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(offense_pct = offense_snaps/tot_offense_snaps,
                defense_pct = defense_snaps/tot_defense_snaps) %>% 
  # Filter for only offensive skill positions
  dplyr::filter(stringr::str_detect(position, "(RB)|(HB)|(WR)|(TE)|(FB)") | player == "Taysom Hill") %>% 
  # Fill in missing games for players
  dplyr::group_by(season, team) %>% 
  tidyr::complete(game_id, pfr_player_id, fill = list(offense_snaps = 0, 
                                                      defense_snaps = 0,
                                                      st_snaps = 0,
                                                      offense_pct = 0)) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(week = as.integer(stringr::str_sub(game_id, start = 6, end = 7)),
                sum = offense_snaps + defense_snaps + st_snaps) %>% 
  # Fix cases where `complete` makes multiple rows per player-season-week
  dplyr::group_by(season, week, pfr_id = pfr_player_id) %>% 
  dplyr::filter(sum == max(sum)) %>% 
  dplyr::ungroup() %>% 
  dplyr::distinct(season, week, pfr_id, .keep_all = TRUE) %>% 
  dplyr::select(season, week, game_id, pfr_id = pfr_player_id, team, offense_snaps, offense_pct, tot_offense_snaps) %>%
  # Join in player key
  dplyr::inner_join(rb_wr_te_key %>% 
                      dplyr::select(pfr_id, gsis_id, position),
                    by = c("pfr_id")) %>% 
  # Find player game number
  dplyr::arrange(season, week)

# Calculate expected yards after catch for each play
xyac <- pbp %>% 
  # dplyr::filter(season == current_season) %>% 
  add_xyac2(summarize = F) %>% 
  dplyr::select(season, week, season_type, game_id, posteam, play_id, 
                player_id = receiver_id, yardline_100,
                ydstogo, air_yards, actual_yards_gained = yards_gained, 
                first_down, complete_pass, cp, yac_prob = prob, gain)

if (isTRUE(opt$live)) {
  xyac <- readRDS("~/Desktop/RStudio/Fantasy_Football/data/xyac2013_20.rds") %>% 
    dplyr::bind_rows(xyac %>% 
                       dplyr::filter(season == current_season))
  pass_rates <- readRDS("~/Desktop/RStudio/Fantasy_Football/data/pass_rates2013_20.rds") %>% 
    dplyr::bind_rows(pass_rates %>% 
                       dplyr::filter(season == current_season))
} else {
  # Save xyac
  saveRDS(xyac %>%
            dplyr::filter(season != current_season),
          "~/Desktop/RStudio/Fantasy_Football/data/xyac2013_20.rds")
  saveRDS(pass_rates %>%
            dplyr::filter(season != current_season),
          "~/Desktop/RStudio/Fantasy_Football/data/pass_rates2013_20.rds")
}

cat(paste0("Creating model dataset...\n"))

# Calculate player game-level statistics
player_stats <- xyac %>% 
  dplyr::mutate(gain = ifelse(yardline_100 == air_yards, yardline_100, gain),
                yac_prob = ifelse(yardline_100 == air_yards, 1, yac_prob),
                first_down = ifelse(gain >= ydstogo, 1, 0),
                touchdown = ifelse(gain == yardline_100, 1, 0),
                PPFD_points = gain/10 + ifelse(gain == yardline_100, 6, 0) + first_down,
                PPR_points = gain/10 + ifelse(gain == yardline_100, 6, 0) + 0.5*complete_pass,
                catch_run_prob = cp * yac_prob,
                exp_yards = gain * catch_run_prob,
                exp_first_down = first_down * catch_run_prob,
                exp_touchdown = touchdown * catch_run_prob,
                exp_PPFD_points = PPFD_points * catch_run_prob,
                exp_PPR_points = PPR_points * catch_run_prob,
                actual_outcome = ifelse(actual_yards_gained == gain & complete_pass == 1, 1, 0),
                actual_first_down = ifelse(actual_yards_gained == gain & first_down == 1, 1, 0),
                actual_touchdown = ifelse(actual_yards_gained == gain & touchdown == 1, 1, 0),
                actual_PPFD_points = ifelse(actual_outcome == 1, PPFD_points, 0),
                actual_PPR_points = ifelse(actual_outcome == 1, PPR_points, 0)) %>%
  dplyr::group_by(game_id, play_id) %>% 
  dplyr::mutate(target = ifelse(dplyr::row_number() == 1, 1, 0),
                air_yards = ifelse(dplyr::row_number() == 1, air_yards, 0)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(season, week, game_id, posteam, player_id) %>% 
  # Calculate game metrics
  dplyr::summarise(targets = sum(target, na.rm = T),
                   air_yards = sum(air_yards, na.rm = T),
                   catches = sum(actual_outcome, na.rm = T),
                   yards = sum(ifelse(actual_outcome == 1, gain, 0), na.rm = T),
                   first_downs = sum(actual_first_down, na.rm = T),
                   touchdowns = sum(actual_touchdown, na.rm = T),
                   exp_catches = mean(cp, na.rm = T) * targets,
                   exp_yards = sum(exp_yards, na.rm = T),
                   exp_first_downs = sum(exp_first_down, na.rm = T),
                   exp_touchdowns = sum(exp_touchdown, na.rm = T),
                   PPFD_pts = sum(actual_PPFD_points, na.rm = T),
                   exp_PPFD_pts = sum(exp_PPFD_points, na.rm = T),
                   PPR_pts = sum(actual_PPR_points, na.rm = T),
                   exp_PPR_pts = sum(exp_PPR_points, na.rm = T),
                   .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(season, week, game_id, posteam) %>% 
  dplyr::mutate(air_yards_share = air_yards / sum(air_yards, na.rm = T),
                target_share = targets / sum(targets, na.rm = T),
                # Cut off air yard share at 0
                air_yards_share = pmax(0, air_yards_share)) %>% 
  dplyr::ungroup() %>% 
  # Join in snap counts
  dplyr::full_join(snaps %>% 
                     dplyr::select(-position, -pfr_id),
                   by = c("season", "week", "game_id", "player_id" = "gsis_id", "posteam" = "team")) %>%
  # Fill in total offensive snaps for each team
  dplyr::group_by(game_id, posteam) %>% 
  tidyr::fill(tot_offense_snaps, .direction = "downup") %>% 
  dplyr::ungroup() %>% 
  # Find game number by player
  dplyr::arrange(season, week, player_id) %>% 
  dplyr::group_by(season, player_id) %>% 
  dplyr::mutate(game_number = dplyr::row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::relocate(c(game_number, player_id), .after = game_id) %>% 
  # Fill in all NA's with 0
  dplyr::mutate(dplyr::across(targets:offense_pct, ~ tidyr::replace_na(., 0)),
                # No play flag
                no_play = ifelse(targets == 0 & offense_snaps == 0, 1, 0)) %>% 
  # Find consecutive missed games for each player
  dplyr::group_by(player_id) %>% 
  dplyr::mutate(sequence = cumsum(no_play != dplyr::lag(no_play, default = 0)),
                career_game = cumsum(no_play == 0)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(player_id, sequence) %>% 
  dplyr::mutate(consec_missed = ifelse(no_play == 1, dplyr::row_number(), 0)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-sequence)

dupe_players <- player_stats %>% 
  dplyr::count(season, week, player_id, sort = TRUE) %>% 
  dplyr::filter(n > 1)

if (nrow(dupe_players) > 0) {
  cat("Duplicate player weeks in `player_stats`...\n")
  quit(status = 49)
}

cat(paste0("Computing weighted rolling moving-window averages...\n"))

# Defensive stats allowed
def_stats <- player_stats %>% 
  dplyr::group_by(season, game_id, posteam) %>% 
  dplyr::summarise(targets_allowed = sum(targets),
                   PPFD_allowed = sum(PPFD_pts),
                   .groups = "drop") %>% 
  dplyr::mutate(PPFD_allowed_target = PPFD_allowed/targets_allowed) %>% 
  dplyr::left_join(schedule %>% 
                     dplyr::select(game_id, week, team, defteam = opponent),
                   by = c("game_id", "posteam" = "team")) %>% 
  dplyr::select(game_id, season, week, team = defteam, 
                targets_allowed, PPFD_allowed, PPFD_allowed_target)

pred_team_stats <- pass_rates %>% 
  # Join in defensive stats
  dplyr::left_join(def_stats, by = c("game_id", "season", "week", "team")) %>% 
  # Add in next game to predict on
  dplyr::bind_rows(next_team_games) %>% 
  dplyr::group_by(season, team) %>% 
  # Minimum 5 game window of moving average, Maximum 10
  dplyr::mutate(window = pmin(pmax(opt$min_g, game_number), opt$max_g), 
                cur_pass_oe = pass_oe,
                dplyr::across(c(pass_oe),
                              ~ wt_mov_avg(var = .,
                                           weight = plays,
                                           window = window,
                                           type = opt$roll_type,
                                           moving = TRUE)),
                dplyr::across(c(PPFD_allowed, PPFD_allowed_target),
                              ~ wt_mov_avg(var = .,
                                           weight = targets_allowed,
                                           window = window,
                                           type = opt$roll_type,
                                           moving = TRUE)),
                PPFD_allowed_pred = dplyr::lag(PPFD_allowed),
                PPFD_allowed_target_pred = dplyr::lag(PPFD_allowed_target),
                pass_oe_pred = dplyr::lag(pass_oe)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(team) %>% 
  tidyr::fill(pass_oe_pred, PPFD_allowed_pred, PPFD_allowed_target_pred, .direction = "down") %>% 
  dplyr::ungroup() %>% 
  dplyr::select(game_id, team, pass_oe_pred, PPFD_allowed_pred, PPFD_allowed_target_pred)

# Find rest of season average fantasy points 
ros_avg_pts <- player_stats %>% 
  # filter(!player_id %in% dupe_players$player_id) %>% 
  dplyr::group_by(season, player_id) %>% 
  dplyr::mutate(prev_game_id = dplyr::lag(game_id),
                last_game_id = game_id[week == max(week)]) %>% 
  dplyr::ungroup() %>% 
  # Get rid of last week of regular season and playoffs (don't care about for fantasy football)
  dplyr::anti_join(week_key,
                   by = c("season", "week" = "bad_weeks")) %>% 
  dplyr::transmute(season, game_id, player_id, posteam, game_number, 
                   prev_game_number = game_number - 1, no_play,
                   prev_game_id, last_game_id,
                   next_PPFD_pts = PPFD_pts,
                   next_game_id = game_id,
                   next_season = season,
                   next_week = week,
                   next_team = posteam,
                   next_career_game = career_game) %>%
  # Find average rest of season fantasy points per game
  dplyr::group_by(season, player_id) %>% 
  dplyr::mutate(sum_lag = dplyr::lag(cumsum(next_PPFD_pts), default = 0),
                avg_ros_pts = (sum(next_PPFD_pts) - sum_lag)/(dplyr::n_distinct(game_number) - prev_game_number)) %>% 
  dplyr::ungroup() %>% 
  # Fill in game IDs to predict week 1's from week 17 or playoff of last year
  dplyr::group_by(player_id) %>% 
  dplyr::mutate(prev_game_id = ifelse(is.na(prev_game_id), dplyr::lag(last_game_id), prev_game_id)) %>%
  dplyr::ungroup() %>% 
  dplyr::select(season, posteam, player_id, prev_game_id, prev_game_number, last_game_id,
                next_game_id, next_season, next_week, next_team, next_career_game, 
                no_play, next_PPFD_pts, avg_ros_pts)

# Find weighted sliding-window-moving average of various metrics
rolling_ftsy <- player_stats %>% 
  # Filter for only games a player plays for rolling averages
  dplyr::filter(no_play == 0) %>% 
  dplyr::group_by(player_id) %>% 
  # Minimum 5 game window of moving average, Maximum 10
  dplyr::mutate(window = pmin(pmax(opt$min_g, game_number), opt$max_g), 
                cur_PPFD_pts = PPFD_pts,
                dplyr::across(c(targets:offense_pct),
                              ~ wt_mov_avg(var = .,
                                           weight = tot_offense_snaps,
                                           window = window,
                                           type = opt$roll_type,
                                           moving = TRUE))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(wpor = 1.5*target_share + 0.7*air_yards_share,
                racr = pmax(1, yards)/pmax(1, air_yards),
                exp_racr = pmax(1, exp_yards)/pmax(1, air_yards)) %>% 
  dplyr::relocate(c(wpor, racr, exp_racr), .after = target_share)

# Find first games for each player
first_games <- player_stats %>% 
  dplyr::filter(no_play == 0) %>% 
  dplyr::group_by(player_id) %>% 
  dplyr::filter(season == min(season)) %>% 
  dplyr::filter(week == min(week)) %>% 
  dplyr::ungroup() %>% 
  dplyr::transmute(game_id, season, week, player_id, first_game = 1) %>% 
  # Fix since week 17/18 does not matter for fantasy
  dplyr::mutate(week = ifelse(week >= 17 & season <= 2020, 16, week),
                week = ifelse(week >= 18 & season > 2020, 17, week))

# Players that never played
never_played <- player_stats %>% 
  dplyr::distinct(player_id) %>% 
  dplyr::anti_join(first_games, by = "player_id")

# Create final model dataset
model_dataset <- rolling_ftsy %>% 
  dplyr::select(-no_play) %>% 
  # Bind in games that player missed
  dplyr::bind_rows(player_stats %>% 
                     # Filter for games a player missed to add on
                     dplyr::filter(no_play == 1) %>% 
                     dplyr::mutate(cur_PPFD_pts = PPFD_pts) %>% 
                     dplyr::select(season, week, game_id, game_number, player_id, posteam, tot_offense_snaps, consec_missed)) %>% 
  dplyr::arrange(season, week, player_id) %>% 
  # Fill in player stats for missed games as last game played
  dplyr::group_by(player_id) %>% 
  tidyr::fill(targets, air_yards, catches, yards, first_downs, touchdowns, 
              exp_catches, exp_yards, exp_first_downs, exp_touchdowns, 
              PPFD_pts, exp_PPFD_pts, PPR_pts, exp_PPR_pts, air_yards_share,
              target_share, wpor, racr, exp_racr, offense_snaps, offense_pct, .direction = "down") %>% 
  dplyr::ungroup() %>% 
  # Join in target variables
  dplyr::left_join(ros_avg_pts %>% 
                     dplyr::select(-c(season, posteam, prev_game_number, last_game_id)),
                   by = c("player_id", "game_id" = "prev_game_id")) %>% 
  # Join in bye weeks
  dplyr::left_join(bye_weeks %>% 
                     dplyr::select(-next_game_id),
                   by = c("season", "posteam" = "team", "game_id" = "prev_game_id")) %>% 
  # Fill in whether the row is current season-week
  dplyr::mutate(
    current = dplyr::case_when(
      season == current_sn_wk$season & week == current_sn_wk$week ~ 1,
      season == current_sn_wk$season & week == current_sn_wk$week - 1 & bye == 1 ~ 1, # Don't get rid of current teams on bye weeks
      TRUE ~ 0),
    next_season = ifelse(current == 1, current_sn_wk$season, next_season),
    next_week = ifelse(current == 1, current_sn_wk$week + 1, next_week)) %>% 
  # Get rid of rows to predict week 17's or playoff games
  dplyr::filter(!(is.na(next_game_id) & current == 0)) %>%
  # Get rid of 2013 weeks 1-4 (first weeks - not enough data)
  dplyr::filter(!(next_season == opt$start_season & next_week <= 4)) %>%
  # Get rid of first ever player-games (no data)
  dplyr::left_join(first_games %>% 
                     dplyr::select(-game_id),
                   by = c("player_id", "next_season" = "season", "next_week" = "week")) %>% 
  dplyr::group_by(player_id) %>% 
  tidyr::fill(first_game, .direction = "up") %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(is.na(first_game)) %>%
  # Get rid of players who have never played
  dplyr::filter(!player_id %in% never_played$player_id) %>% 
  # Join in injuries
  dplyr::left_join(injuries %>% 
                     dplyr::select(season, week, player_id, report_status:secondary_injury),
                   by = c("player_id", "next_season" = "season", "next_week" = "week")) %>% 
  tidyr::replace_na(list(primary_injury = "none", 
                         secondary_injury = "none")) %>% 
  dplyr::mutate(on_injury_report = ifelse(!is.na(report_status) | !is.na(practice_status), 1, 0),
                report_status_num = dplyr::case_when(
                  is.na(report_status) ~ 0,
                  report_status == "Probable" ~ 1,
                  report_status == "Questionable" ~ 2,
                  report_status == "Doubtful" ~ 3,
                  report_status == "Out" ~ 4),
                practice_status_num = dplyr::case_when(
                  is.na(practice_status) ~ 0,
                  practice_status == "Full" ~ 1,
                  practice_status == "Limited" ~ 2,
                  practice_status == "DNP" ~ 3),
                diff_yards = yards - exp_yards,
                diff_PPFD_pts = PPFD_pts - exp_PPFD_pts) %>% 
  dplyr::select(dplyr::any_of(c(
    "prev_game_id" = "game_id", "prev_season" = "season", "prev_week" = "week",
    "next_game_id", "next_season", "next_week", "current", "player_id", "team" = "posteam", "next_team",
    "next_career_game", "pass_oe" = "pass_oe_pred", "targets", "target_share", "air_yards", "air_yards_share",
    "catches", "yards", "first_downs", "touchdowns", "PPR_pts", "PPFD_pts", 
    "exp_catches", "exp_yards", "exp_first_downs", "exp_touchdowns", "exp_PPR_pts", "exp_PPFD_pts", 
    "wpor", "racr", "exp_racr", "diff_yards", "diff_PPFD_pts", 
    "offense_snaps", "offense_pct", "tot_offense_snaps", "consec_missed", "on_injury_report",
    "report_status_num", "report_status", "practice_status_num", "practice_status", "primary_injury", "secondary_injury",
    "cur_PPFD_pts", "no_play", "next_PPFD_pts", "avg_ros_pts"
  ))) 

# Find target share rank by season week for each team
target_ranks <- model_dataset %>% 
  # Get rid of `Out` and `Doubtful`
  dplyr::filter(report_status_num <= 2) %>%
  dplyr::group_by(prev_game_id, team) %>% 
  dplyr::mutate(rank = rank(-target_share)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(prev_game_id, player_id, rank)

# Join in the correct next games
model_dataset <- model_dataset %>% 
  dplyr::filter(current == 0) %>% 
  dplyr::bind_rows(model_dataset %>% 
                     dplyr::filter(current == 1) %>% 
                     dplyr::select(-c(next_game_id, next_season, next_week)) %>% 
                     dplyr::left_join(next_team_games %>% 
                                        dplyr::rename_with(.cols = c(game_id, season, week), 
                                                           ~ paste0("next_", .)),
                                      by = "team")) %>% 
  # Join in target ranks
  dplyr::left_join(target_ranks, by = c("prev_game_id", "player_id")) %>% 
  # Fill in target ranks for `Out` and `Doubtful` players
  dplyr::group_by(player_id) %>% 
  tidyr::fill(rank, .direction = "downup") %>% 
  dplyr::mutate(change_team = ifelse(team != next_team, 1, 0),
                # game_number = dplyr::row_number(),
                dplyr::across(c(next_team, change_team), 
                              ~ ifelse(current == 1 & is.na(.), dplyr::lag(.), .))) %>% 
  dplyr::ungroup() %>% 
  # Cap rank at 15
  dplyr::mutate(rank = pmin(15, rank),
                next_team = ifelse(is.na(next_team), team, next_team),
                change_team = ifelse(is.na(change_team), 0, change_team)) %>% 
  dplyr::relocate(rank, .after = target_share) %>% 
  dplyr::relocate(c(change_team), .after = next_team) %>% 
  # Join in PFR injuries
  dplyr::left_join(pfr_inj %>% 
                     dplyr::filter(!is.na(gsis_id)) %>%
                     dplyr::select(game_id, gsis_id, type, inj_type_num, inj_type, dnp),
                   by = c("next_game_id" = "game_id", "player_id" = "gsis_id")) %>%
  tidyr::replace_na(list(dnp = 0, inj_type_num = 0)) %>% 
  dplyr::relocate(c(type, inj_type_num, inj_type, dnp), .after = secondary_injury) %>% 
  # Join in if a player is active/inactive
  dplyr::left_join(actives,
                   by = c("player_id" = "gsis_id", 
                          "next_season" = "season",
                          "next_week" = "weeks_active_reg")) %>% 
  tidyr::replace_na(list(active = 0,
                         type = "None",
                         inj_type = "none")) %>%
  dplyr::mutate(
    injury = dplyr::case_when(
      report_status_num == inj_type_num ~ report_status_num, # When equal, set to either
      report_status_num == 4 | inj_type_num == 4 ~ 4, # When either says out, out
      TRUE ~ (report_status_num + inj_type_num)/2) # Else, average the two
  ) %>% 
  dplyr::relocate(c(active, injury), .before = type) %>% 
  # Join in pass rate over expectations
  dplyr::left_join(pred_team_stats %>% 
                     dplyr::select(game_id, team, pass_oe_pred),
                   by = c("next_team" = "team", "next_game_id" = "game_id")) %>% 
  dplyr::relocate(pass_oe_pred, .after = next_team) %>% 
  # Fill in `pass_oe` by season-week-team for players who switch teams
  dplyr::group_by(next_season, team) %>% 
  tidyr::fill(pass_oe_pred, .direction = "downup") %>% 
  dplyr::ungroup() %>% 
  # Join in market lines
  dplyr::left_join(schedule %>%
                     dplyr::transmute(game_id, season, week, team, next_opp = opponent, location, team_implied_total,
                                      spread_line = ifelse(location == "home", spread_line, -spread_line),
                                      team_rest_diff, team_qb_id),
                   by = c("next_season" = "season",
                          "next_week" = "week",
                          "next_game_id" = "game_id",
                          "next_team" = "team")) %>% 
  # Join in team defensive statistics
  dplyr::left_join(pred_team_stats %>% 
                     dplyr::select(game_id, team, PPFD_allowed_pred, PPFD_allowed_target_pred),
                   by = c("next_opp" = "team", "next_game_id" = "game_id")) %>% 
  dplyr::relocate(c(PPFD_allowed_pred, PPFD_allowed_target_pred), .after = tot_offense_snaps)

# Players with no pfr_id mapping (removed)
bad_pfr_maps <- model_dataset %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarise(games = dplyr::n(),
                   offense_snaps = sum(offense_snaps)) %>%
  dplyr::filter(offense_snaps == 0) %>%
  dplyr::select(player_id)

# Save model_dataset
suff <- paste0(opt$min_g, "_", opt$max_g, "_", opt$roll_type)
saveRDS(model_dataset %>% 
          dplyr::filter(!player_id %in% bad_pfr_maps$player_id), 
        paste0("~/Desktop/RStudio/Fantasy_Football/data/model_dataset_", suff, ".rds"))

cat(paste0("Completed fantasy football receiving model dataset for 2013-", max(sns), "\n"))

# model_dataset %>%
#   summarise_na()
# 
# model_dataset %>% 
#   count(primary_injury) %>% View()
# 
#   
# model_dataset %>% 
#   filter(player_id == dk) %>% View()
# 
