### Find weeks where a player is active

# Load libraries
suppressMessages(suppressWarnings(library(nflreadr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))

option_list <- list(
  make_option("--live", type="logical", default = FALSE,
              help="Whether to find roster only for current season"),
  make_option("--start_season", type="integer", default = 2013,
              help="Start season")
)
opt <- parse_args(OptionParser(option_list=option_list))

# Clear cache
nflreadr::.clear_cache()

# Find the current season as default
season <- as.numeric(substr(Sys.Date(), start = 1, stop = 4))
month <- as.numeric(substr(Sys.Date(), start = 6, stop = 7))
current_season <- ifelse(month >= 7, season, season - 1)

if (isTRUE(opt$live)) {
  sns <- current_season
  cat(paste0("Loading active roster for ", max(sns), "...\n"))
} else {
  # Seasons to load in
  sns <- opt$start_season:current_season
  cat(paste0("Loading active roster for 2013-", max(sns), "...\n"))
}

valid_weeks <- nflreadr::load_schedules(seasons = sns) %>%
  dplyr::mutate(
    game_type = dplyr::case_when(game_type == 'REG' ~ 'REG',
                                 game_type %in% c('WC','DIV','CON','SB') ~ 'POST',
                                 T ~ NA_character_)) %>%
  dplyr::filter(!is.na(game_type)) %>%
  dplyr::group_by(season, game_type) %>%
  dplyr::mutate(week = as.numeric(as.factor(week))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(season, game_type, week) %>%
  dplyr::summarise(games_played = sum(!is.na(result)),
                   .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(filter_week = which(games_played == 0)[1]) %>%
  dplyr::filter(dplyr::row_number() <= filter_week) %>%
  dplyr::select(c(season, game_type = game_type, week))

scrape_teams <- function(year) {
  h <- httr::handle("https://www.nfl.info")
  r <- httr::GET(
    handle = h,
    path = glue::glue(
      "/nfldataexchange/dataexchange.asmx/getClubs?lseason={year}"
    ),
    httr::authenticate("media", "media"),
    url = NULL
  )
  teams_df <- httr::content(r) %>%
    XML::xmlParse() %>%
    XML::xmlToDataFrame()
  rm(h)
  return(teams_df)
}

teams <- purrr::map_dfr(sns, scrape_teams) %>%
  dplyr::filter(!(ClubCode %in% c("AFC", "NFC", "RIC", "SAN", "CRT", "IRV"))) %>% # remove all-star teams
  dplyr::mutate(Season = as.integer(Season)) %>%
  dplyr::select(club_code = ClubCode, season = Season)

teams <- teams %>%
  tidyr::expand_grid(game_type = c("REG"),
                     week = c(1:18)) %>%
  dplyr::inner_join(valid_weeks, by=c('season', 'game_type', 'week'))

scrape_rosters <- function(season, team, game_type, week) {
  cli::cli_process_start("Loading {season} {team}, week {week} of the {game_type} season")
  h <- httr::handle("https://www.nfl.info")
  r <- httr::GET(
    handle = h,
    path = glue::glue(
      "/nfldataexchange/dataexchange.asmx/getRoster?lseason={season}&lweek={week}&lseasontype={game_type}&lclub={team}"
    ),
    httr::authenticate("media", "media"),
    url = NULL
  )
  roster_df <- httr::content(r) %>%
    XML::xmlParse() %>%
    XML::xmlToDataFrame()
  rm(h)
  cli::cli_process_done()
  return(roster_df)
}

roster_df <- purrr::pmap_dfr(list(teams$season, teams$club_code, teams$game_type, teams$week), scrape_rosters)

weeks_rostered_reg <- roster_df %>%
  dplyr::filter(SeasonType == 'REG',
                StatusShortDescription == "Active") %>%
  dplyr::mutate(Season = as.double(Season),
                CurrentClub = dplyr::case_when(CurrentClub == 'ARZ' ~ 'ARI',
                                               CurrentClub == 'BLT' ~ 'BAL',
                                               CurrentClub == 'CLV' ~ 'CLE',
                                               CurrentClub == 'HST' ~ 'HOU',
                                               CurrentClub == 'SL' ~ 'STL',
                                               T~CurrentClub)) %>%
  dplyr::group_by(gsis_id = GsisID, season = Season) %>%
  dplyr::summarize(weeks_active_reg = paste0(Week,collapse=';'),
                   recent_team = CurrentClub[Week == max(Week)],
                   .groups = "drop") %>%
  dplyr::ungroup()

# Odell Beckham
# weeks_rostered_reg %>% 
#   filter(gsis_id == "00-0031235")
# 
# current_roster_df %>% 
#   filter(gsis_id == "00-0031235") %>% View()

current_roster_df <- nflreadr::load_rosters(seasons = sns) %>%
  dplyr::select(!dplyr::starts_with('weeks_active_')) %>%
  dplyr::left_join(weeks_rostered_reg, by=c('gsis_id','season'))

roster_split <- current_roster_df %>%
  dplyr::group_split(season)

purrr::walk(roster_split, function(x) {
  saveRDS(x, glue::glue("data/actives/actives_{unique(x$season)}.rds"))
  readr::write_csv(x, glue::glue("data/actives/actives_{unique(x$season)}.csv"))
})

