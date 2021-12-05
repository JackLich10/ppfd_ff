### Scrape market fantasy football predictions

# Load libraries
suppressMessages(suppressWarnings(library(nflreadr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(pracma)))

option_list <- list(
  make_option("--start_season", type="integer", default = 2015,
              help="Start season"),
  make_option("--live", type="logical", default = FALSE,
              help="Live")
)
opt <- parse_args(OptionParser(option_list=option_list))
# Rscript receiver_model/market.R --live TRUE

# Clear cache
nflreadr::.clear_cache()

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

cat(paste0("Scraping market fantasy football receiving projections for 2015-", max(sns), "...\n"))

# Load in games
games <- nflreadr::load_schedules(seasons = sns) %>% 
  dplyr::mutate(home_team = nflreadr::clean_team_abbrs(home_team),
                away_team = nflreadr::clean_team_abbrs(away_team))

ftsy_proj <- function(sn, wk) {
  
  cli::cli_process_start("Scraping {sn} {wk}")
  
  proj <- try(ffpros::fp_projections("flex", year = sn, week = wk), silent = TRUE) 
  
  # If error in flex, try each position individually
  if ("try-error" %in% class(proj)) {
    
    cli::cli_process_start("flex failed, trying individual positions...")
    
    proj <- purrr::map_dfr(c("wr", "te", "rb"), function(pos) {
      proj <- try(ffpros::fp_projections(pos, year = sn, week = wk), silent = TRUE)
      
      if ("try-error" %in% class(proj)) {
        return(tibble::tibble())
      }
      return(proj)
    })
  }
  
  if (nrow(proj) == 0) {
    cli::cli_process_done("no data available")
    return(proj)
  }
  
  return(proj %>% 
           dplyr::select(-dplyr::starts_with("rushing_"), -dplyr::starts_with("misc_")) %>% 
           # receiving TDs, yards, and about 22% of catches + 3% of yards resulting in first downs
           dplyr::mutate(proj_PPFD_pts = receiving_tds*6 + 0.1*receiving_yds + 0.22*receiving_rec + 0.03*receiving_yds))
}

weeks_to_scrape <- games %>% 
  dplyr::filter(game_type == "REG") %>% 
  dplyr::distinct(season, week)

f_pros_proj <- weeks_to_scrape %>% 
  dplyr::mutate(proj = purrr::map2(season, week, ~ ftsy_proj(sn = .x, wk = .y))) %>% 
  tidyr::unnest(proj)

# Read in saved market projections
market_proj <- purrr::map_dfr(2016:current_season, function(sn) {
  readRDS(paste0("data/market/proj_", sn, ".rds"))
})

split <- f_pros_proj %>% 
  dplyr::bind_rows(market_proj) %>% 
  dplyr::distinct(season, week, fantasypros_id, .keep_all = TRUE) %>% 
  dplyr::group_split(season)

# if (isTRUE(opt$live)) {
#   split <- purrr::map_dfr(sns, function(sn) {
#     readRDS(paste0("data/market/proj_", sn, ".rds"))
#   }) %>% 
#     dplyr::filter(season != current_season) %>% 
#     dplyr::bind_rows(f_pros_proj) %>% 
#     dplyr::group_split(season)
# } else {
#   split <- f_pros_proj %>% 
#     dplyr::group_split(season)
# }

purrr::walk(split, function(x) {
  saveRDS(x, glue::glue("data/market/proj_{unique(x$season)}.rds"))
  # readr::write_csv(x, glue::glue("data/market/proj_{unique(x$season)}.csv"))
})

# market_weeks <- f_pros_proj %>% 
#   distinct(season, week)
# 
# f_pros_proj %>% 
#   distinct(season, week) %>% View()

# ffpros::fp_projections("rb", year = 2019, week = 1)

# url_query <- glue::glue("https://fantasydata.com/NFL_Projections/Projections_Read/?") %>%
#   httr::modify_url(query = list(
#     `pageSize`=100,
#     `filters.position`=12,
#     `filters.season`=2021,
#     `filters.seasontype`=1,
#     `filters.scope`=2,
#     `filters.scoringsystem`=1,
#     `filters.startweek`=11,
#     `filters.endweek`=11
#     # `filters.showall`=`true`
#   ))
# 
# content <- httr::RETRY("POST", url = url_query) %>%
#   httr::content()
# 
# content$Data %>%
#   purrr::map_df(., unlist)
# 
# "https://fantasydata.com/NFL_Projections/Projections_Read/?sort=FantasyPoints-desc&pageSize=50&group=&filter=&filters.position=12&filters.team=&filters.teamkey=&filters.season=2021&filters.seasontype=1&filters.cheatsheettype=&filters.scope=2&filters.subscope=&filters.redzonescope=&filters.scoringsystem=1&filters.leaguetype=&filters.searchtext=&filters.week=&filters.startweek=11&filters.endweek=11&filters.minimumsnaps=&filters.teamaspect=&filters.stattype=&filters.exportType=&filters.desktop=&filters.dfsoperator=&filters.dfsslateid=&filters.dfsslategameid=&filters.dfsrosterslot=&filters.page=&filters.showfavs=&filters.posgroup=&filters.oddsstate=&filters.showall=&filters.aggregatescope=&filters.rangescope=&filters.range=&filters.type="
