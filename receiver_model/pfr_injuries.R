### Scrape PFR for injury data

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))

option_list <- list(
  make_option("--start_season", type="integer", default = 2013,
              help="Start season"),
  make_option("--live", type="logical", default = TRUE,
              help="Only scrape current season injuries")
)
opt <- parse_args(OptionParser(option_list=option_list))
# Rscript receiver_model/pfr_injuries.R --live TRUE

# Find the current season as default
season <- as.numeric(substr(Sys.Date(), start = 1, stop = 4))
month <- as.numeric(substr(Sys.Date(), start = 6, stop = 7))
current_season <- ifelse(month >= 7, season, season - 1)

# Seasons to scrape
if (isTRUE(opt$live)) {
  sns <- current_season
} else {
  sns <- opt$start_season:current_season
}

cat(paste0("Current season: ", current_season, "...\n"))

if (isTRUE(opt$live)) {
  cat(paste0("Scraping PFR injury data for ", current_season, "...\n"))
} else {
  cat(paste0("Scraping PFR injury data for 2013-", max(sns), "...\n"))
}

# Clear cache
nflreadr::.clear_cache()

# Load in games
games <- nflreadr::load_schedules(seasons = sns) %>% 
  dplyr::mutate(home_team = nflreadr::clean_team_abbrs(home_team),
                away_team = nflreadr::clean_team_abbrs(away_team))

teams <- readr::read_csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/teams.csv",
                         col_types = readr::cols()) %>% 
  dplyr::filter(season >= min(sns)) %>% 
  dplyr::select(season, team, nfl_team_id, pfr) %>% 
  dplyr::mutate(pfr_lower = stringr::str_to_lower(pfr))

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

scrape_pfr_injuries <- function(team, sn) {
  cli::cli_process_start("Scraping {sn} {team}")
  path <- paste0("https://www.pro-football-reference.com/teams/", team, "/", sn, "_injuries.htm")
  
  html <- xml2::read_html(path)
  
  # Extract some metadata
  td <- html %>% 
    rvest::html_elements("#team_injuries td") 
  
  class <- td %>% 
    rvest::html_attr("class") %>% 
    stringr::str_remove(., "(center )") %>% 
    stringr::str_remove(., "poptip ") %>% 
    stringr::str_remove(., "iz$")
  
  st_week <- td %>% 
    rvest::html_attr("data-stat")
  
  text <- td %>% 
    rvest::html_attr("data-tip")
  
  meta <- dplyr::tibble(class = class,
                        week = as.integer(stringr::str_remove(st_week, "week_")),
                        text = text) %>% 
    dplyr::mutate(class = dplyr::na_if(class, ""))
  
  references <- html %>% 
    rvest::html_elements("#team_injuries a") %>% 
    rvest::html_attr("href")
  
  pfr_g_ids <- references %>% 
    stringr::str_subset(., "boxscores") %>% 
    stringr::str_extract(., "\\d{8,9}[a-z]{2,3}")
  
  pfr_ids <- references %>% 
    stringr::str_subset(., "players") %>% 
    stringr::str_remove(., "/players/[a-zA-Z]{1}/") %>% 
    stringr::str_remove(., ".htm$")
  
  inj_table <- html %>% 
    rvest::html_table() %>% 
    purrr::pluck(1) %>% 
    dplyr::bind_cols(pfr_id = pfr_ids)
  
  colnames(inj_table) <- c("player", pfr_g_ids, "pfr_id")
  
  inj_table %>% 
    tidyr::pivot_longer(cols = -c(player, pfr_id),
                        names_to = "game",
                        values_to = "designation") %>% 
    dplyr::mutate(designation = dplyr::na_if(designation, "")) %>%
    dplyr::bind_cols(meta) %>% 
    dplyr::filter(!(is.na(designation) & is.na(text)))
}

pfr_injuries <- teams %>% 
  dplyr::mutate(inj = purrr::map2(pfr_lower, season, scrape_pfr_injuries)) %>% 
  tidyr::unnest(inj) %>% 
  dplyr::left_join(games %>% 
                     dplyr::select(game_id, week, pfr),
            by = c("game" = "pfr", "week"))

pfr_inj <- pfr_injuries %>% 
  dplyr::filter(!(pfr == "NOR" & pfr_id == "LawrDe00"),
                !(pfr == "DET" & pfr_id == "BarcDo00"),
                !(pfr == "NYJ" & pfr_id == "RobiEd00")) %>% 
  dplyr::select(game_id, season, week, team, player, pfr_id, 
                class, text, designation) %>% 
  tidyr::separate(text, into = c("type", "inj_type"), sep = ": ") %>% 
  tidyr::separate(class, into = c(NA, "dnp"), sep = " ", fill = "right") %>% 
  dplyr::mutate(dnp = ifelse(is.na(dnp), 0, 1),
                inj_type = dplyr::na_if(inj_type, ""),
                inj_type = stringr::str_to_lower(inj_type),
                type = dplyr::case_when(
                  type == "injured rese" ~ "Injured Reserve",
                  TRUE ~ type)) %>% 
  dplyr::left_join(player_key %>% 
                     dplyr::filter(!is.na(pfr_id)), by = "pfr_id") %>% 
  dplyr::relocate(c(gsis_id, fantasypros_id), .after = pfr_id)

dupe_players <- pfr_inj %>% 
  dplyr::count(pfr_id, game_id, sort = T) %>% 
  dplyr::filter(n > 1)

if (nrow(dupe_players) > 0) {
  cat("Duplicate player weeks in `pfr_inj`...\n")
  quit(status = 49)
}

# Save data
if (isTRUE(opt$live)) {
  pfr_inj <- readRDS("~/Desktop/RStudio/Fantasy_Football/data/pfr_inj/pfr_injuries.rds") %>% 
    dplyr::filter(season != current_season) %>% 
    dplyr::bind_rows(pfr_inj)
  saveRDS(pfr_inj, "~/Desktop/RStudio/Fantasy_Football/data/pfr_inj/pfr_injuries.rds")
} else {
  saveRDS(pfr_inj, "~/Desktop/RStudio/Fantasy_Football/data/pfr_inj/pfr_injuries.rds")
}

if (isTRUE(opt$live)) {
  cat(paste0("Completed PFR injury scrape for ", current_season, "\n"))
} else {
  cat(paste0("Completed PFR injury scrape for 2013-", max(sns), "\n"))
}

# 
# pfr_inj %>% 
#   count(inj_type, sort = T) %>% View()
# 
# pfr_inj %>% 
#   count(class, type) %>% View()
# 
# model_dataset %>% 
#   left_join(pfr_inj %>% 
#               filter(!is.na(gsis_id)) %>% 
#               select(game_id, gsis_id, type, inj_type, dnp),
#             by = c("next_game_id" = "game_id", "player_id" = "gsis_id")) %>% 
#   tidyr::replace_na(list(dnp = 0)) %>% 
#   count(no_play, dnp) %>% View()
# count(report_status, type) %>% View()
