# run_pipeline.R
# Master script to run the full panna ratings pipeline

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

# DATA SOURCE: "pannadata" only (worldfootballR removed)
data_source <- "pannadata"

# LEAGUES TO INCLUDE
# Options: "ENG", "ESP", "GER", "ITA", "FRA" (Big 5)
#          "UCL", "UEL" (European competitions)
leagues <- c("ENG", "ESP", "GER", "ITA", "FRA", "UCL", "UEL")

# SEASONS (NULL = all available, or specify like c("2023-2024", "2024-2025"))
seasons <- NULL

# WHICH STEPS TO RUN
# Set to FALSE to skip a step (uses cached data from previous run)
run_steps <- list(
  step_01_load_data        = TRUE,
  step_02_data_processing  = TRUE,
  step_03_splint_creation  = TRUE,
  step_04_rapm             = TRUE,
  step_05_spm              = TRUE,
  step_06_xrapm            = TRUE,
  step_07_seasonal_ratings = TRUE,
  step_08_panna_ratings    = TRUE
)

# FORCE REBUILD FROM STEP
# Set to a step number to clear cache and rebuild from that step onwards
# NULL = normal run (use cache), 1 = full refresh, 3 = rebuild from splints, etc.
force_rebuild_from <- 1 # NULL

# PANNADATA LOCATION (relative to panna directory)
pannadata_path <- "../pannadata/data"

# 3. Helper Functions ----

run_step <- function(step_name, step_num, code_block) {
  step_key <- sprintf("step_%02d_%s", step_num, step_name)
  if (!isTRUE(run_steps[[step_key]])) {
    message(sprintf("\n[%s] Step %d: %s - SKIPPED",
                    format(Sys.time(), "%H:%M:%S"), step_num, step_name))
    return(NULL)

  }

  message(sprintf("\n%s", paste(rep("=", 70), collapse = "")))
  message(sprintf("[%s] Step %d: %s",
                  format(Sys.time(), "%H:%M:%S"), step_num, step_name))
  message(sprintf("%s\n", paste(rep("=", 70), collapse = "")))

  start_time <- Sys.time()
  result <- tryCatch({
    code_block()
    "SUCCESS"
  }, error = function(e) {
    message(sprintf("ERROR: %s", e$message))
    "FAILED"
  })
  end_time <- Sys.time()

  duration <- difftime(end_time, start_time, units = "secs")

  list(
    step = step_num,
    name = step_name,
    status = result,
    duration_secs = as.numeric(duration),
    duration_formatted = format_duration(as.numeric(duration))
  )
}

format_duration <- function(secs) {

  if (secs < 60) {
    sprintf("%.1f seconds", secs)
  } else if (secs < 3600) {
    sprintf("%.1f minutes", secs / 60)
  } else {
    sprintf("%.1f hours", secs / 3600)
  }
}

validate_pipeline_config <- function(pannadata_path, leagues, seasons) {
  errors <- character(0)

  # Check pannadata path exists
  if (!dir.exists(pannadata_path)) {
    errors <- c(errors, sprintf("pannadata_path not found: %s", pannadata_path))
  }

  # Validate leagues
  valid_leagues <- c("ENG", "ESP", "GER", "ITA", "FRA", "UCL", "UEL",
                     "FA_CUP", "COPA_DEL_REY", "DFB_POKAL", "COPPA_ITALIA", "COUPE_DE_FRANCE")
  invalid_leagues <- setdiff(leagues, valid_leagues)
  if (length(invalid_leagues) > 0) {
    errors <- c(errors, sprintf("Invalid leagues: %s", paste(invalid_leagues, collapse = ", ")))
  }

  # Validate seasons format if specified
  if (!is.null(seasons) && length(seasons) > 0) {
    if (!all(grepl("^\\d{4}-\\d{4}$", seasons))) {
      errors <- c(errors, "Seasons must be in format 'YYYY-YYYY' (e.g., '2023-2024')")
    }
  }

  if (length(errors) > 0) {
    stop("Pipeline configuration errors:\n  - ", paste(errors, collapse = "\n  - "))
  }

  message("Configuration validated successfully")
  invisible(TRUE)
}

# 4. Validate Configuration ----

validate_pipeline_config(pannadata_path, leagues, seasons)

# 5. Initialize Pipeline ----

cache_dir <- file.path("data-raw", "cache")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Handle force rebuild - delete cache files from specified step onwards
if (!is.null(force_rebuild_from) && force_rebuild_from >= 1 && force_rebuild_from <= 8) {
  cache_files <- list(
    "1" = c("01_raw_data.rds", "01_config.rds"),
    "2" = "02_processed_data.rds",
    "3" = "03_splints.rds",
    "4" = "04_rapm.rds",
    "5" = "05_spm.rds",
    "6" = "06_xrapm.rds",
    "7" = c("07_seasonal_ratings.rds", "seasonal_spm.csv", "seasonal_rapm.csv", "seasonal_xrapm.csv"),
    "8" = c("08_panna.rds", "panna_ratings.csv")
  )

  files_to_delete <- unlist(cache_files[as.character(force_rebuild_from:8)])
  deleted <- 0
  for (f in files_to_delete) {
    fpath <- file.path(cache_dir, f)
    if (file.exists(fpath)) {
      file.remove(fpath)
      deleted <- deleted + 1
    }
  }
  message(sprintf("\n[Force rebuild] Cleared %d cache files from step %d onwards\n",
                  deleted, force_rebuild_from))
}

pipeline_start <- Sys.time()
step_results <- list()

message("\n")
message(paste(rep("#", 70), collapse = ""))
message("#")
message("#   PANNA RATINGS PIPELINE")
message("#")
message(sprintf("#   Data source: %s", data_source))
message(sprintf("#   Leagues: %s", paste(leagues, collapse = ", ")))
message(sprintf("#   Seasons: %s", if(is.null(seasons)) "All available" else paste(seasons, collapse = ", ")))
message("#")
message(paste(rep("#", 70), collapse = ""))

# 6. Step 1: Load Data ----

step_results[[1]] <- run_step("load_data", 1, function() {
  raw_data_path <- file.path(cache_dir, "01_raw_data.rds")
  config_path <- file.path(cache_dir, "01_config.rds")

  # Check if we can skip loading (cache exists and config matches)
  if (file.exists(raw_data_path) && file.exists(config_path)) {
    cached_config <- readRDS(config_path)
    current_config <- list(leagues = leagues, seasons = seasons, pannadata_path = pannadata_path)

    if (identical(cached_config, current_config)) {
      message("Cache is up to date - skipping data load")
      message(sprintf("  Leagues: %s", paste(leagues, collapse = ", ")))
      message(sprintf("  Seasons: %s", if(is.null(seasons)) "All" else paste(seasons, collapse = ", ")))
      raw_data <<- readRDS(raw_data_path)
      message(sprintf("\nLoaded from cache: %d matches, %d lineups, %s shots",
                      nrow(raw_data$results),
                      nrow(raw_data$lineups),
                      if(!is.null(raw_data$shooting)) nrow(raw_data$shooting) else "0"))
      return()
    }
    message("Configuration changed - reloading data")
  }

  pannadata_dir(pannadata_path)

  message("Loading metadata from pannadata...")
  metadata <- aggregate_cached_matches("metadata")

  if (!is.null(leagues)) {
    metadata <- metadata %>% filter(league %in% leagues)
  }
  if (!is.null(seasons)) {
    metadata <- metadata %>% filter(season %in% seasons)
  }

  message(sprintf("  Found %d matches", nrow(metadata)))

  message("Loading player summary stats...")
  summary_stats <- aggregate_cached_matches("summary")

  if (!is.null(leagues)) {
    summary_stats <- summary_stats %>% filter(league %in% leagues)
  }
  if (!is.null(seasons)) {
    summary_stats <- summary_stats %>% filter(season %in% seasons)
  }

  message(sprintf("  Found %d player-match records", nrow(summary_stats)))

  message("Loading shots data...")
  shots <- aggregate_cached_matches("shots")

  if (!is.null(shots)) {
    if (!is.null(leagues)) {
      shots <- shots %>% filter(league %in% leagues)
    }
    if (!is.null(seasons)) {
      shots <- shots %>% filter(season %in% seasons)
    }
    message(sprintf("  Found %d shots", nrow(shots)))
  }

  # Calculate match-level xG from player stats
  message("Calculating match-level xG...")

  summary_stats <- summary_stats %>%
    mutate(
      x_g = as.numeric(as.character(x_g)),
      npx_g = as.numeric(as.character(npx_g)),
      x_ag = as.numeric(as.character(x_ag))
    )

  match_xg <- summary_stats %>%
    group_by(match_url, is_home) %>%
    summarise(
      team_xg = sum(.data$x_g, na.rm = TRUE),
      team_npxg = sum(.data$npx_g, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      id_cols = match_url,
      names_from = is_home,
      values_from = c(team_xg, team_npxg),
      values_fn = sum
    ) %>%
    rename(
      home_xg = team_xg_TRUE,
      away_xg = team_xg_FALSE,
      home_npxg = team_npxg_TRUE,
      away_npxg = team_npxg_FALSE
    )

  # Format results
  results <- metadata %>%
    left_join(match_xg, by = "match_url") %>%
    rename(
      home = home_team,
      away = away_team,
      date = match_date,
      home_goals = home_score,
      away_goals = away_score
    ) %>%
    mutate(
      season_end_year = as.numeric(substr(season, 6, 9))
    )

  # Format lineups
  lineups <- summary_stats %>%
    rename(
      player_name = player,
      shirt_number = number,
      position = pos,
      minutes = min,
      goals = gls,
      assists = ast,
      xg = x_g,
      npxg = npx_g,
      xag = x_ag
    ) %>%
    left_join(
      results %>% select(match_url, home, away, date, league, season),
      by = c("match_url", "league", "season")
    ) %>%
    mutate(is_starter = TRUE)

  # Load additional stats types
  message("Loading passing stats...")
  stats_passing <- tryCatch(
    aggregate_cached_matches("passing") %>%
      { if (!is.null(leagues)) filter(., league %in% leagues) else . } %>%
      { if (!is.null(seasons)) filter(., season %in% seasons) else . },
    error = function(e) NULL
  )

  message("Loading defense stats...")
  stats_defense <- tryCatch(
    aggregate_cached_matches("defense") %>%
      { if (!is.null(leagues)) filter(., league %in% leagues) else . } %>%
      { if (!is.null(seasons)) filter(., season %in% seasons) else . },
    error = function(e) NULL
  )

  message("Loading possession stats...")
  stats_possession <- tryCatch(
    aggregate_cached_matches("possession") %>%
      { if (!is.null(leagues)) filter(., league %in% leagues) else . } %>%
      { if (!is.null(seasons)) filter(., season %in% seasons) else . },
    error = function(e) NULL
  )

  message("Loading misc stats...")
  stats_misc <- tryCatch(
    aggregate_cached_matches("misc") %>%
      { if (!is.null(leagues)) filter(., league %in% leagues) else . } %>%
      { if (!is.null(seasons)) filter(., season %in% seasons) else . },
    error = function(e) NULL
  )

  message("Loading passing types stats...")
  stats_passing_types <- tryCatch(
    aggregate_cached_matches("passing_types") %>%
      { if (!is.null(leagues)) filter(., league %in% leagues) else . } %>%
      { if (!is.null(seasons)) filter(., season %in% seasons) else . },
    error = function(e) NULL
  )

  message("Loading keeper stats...")
  stats_keeper <- tryCatch(
    aggregate_cached_matches("keeper") %>%
      { if (!is.null(leagues)) filter(., league %in% leagues) else . } %>%
      { if (!is.null(seasons)) filter(., season %in% seasons) else . },
    error = function(e) NULL
  )

  raw_data <<- list(
    results = results,
    lineups = lineups,
    shooting = shots,
    events = NULL,
    stats_summary = summary_stats,
    stats_passing = stats_passing,
    stats_defense = stats_defense,
    stats_possession = stats_possession,
    stats_misc = stats_misc,
    stats_passing_types = stats_passing_types,
    stats_keeper = stats_keeper
  )

  saveRDS(raw_data, file.path(cache_dir, "01_raw_data.rds"))

  # Save config so we can detect if it changes
  current_config <- list(leagues = leagues, seasons = seasons, pannadata_path = pannadata_path)
  saveRDS(current_config, file.path(cache_dir, "01_config.rds"))

  message(sprintf("\nLoaded: %d matches, %d lineups, %s shots",
                  nrow(raw_data$results),
                  nrow(raw_data$lineups),
                  if(!is.null(raw_data$shooting)) nrow(raw_data$shooting) else "0"))
})

# 7. Step 2: Data Processing ----

step_results[[2]] <- run_step("data_processing", 2, function() {
  source("data-raw/player-ratings/02_data_processing.R", local = TRUE)
})

# 8. Step 3: Splint Creation ----

step_results[[3]] <- run_step("splint_creation", 3, function() {
  source("data-raw/player-ratings/03_splint_creation.R", local = TRUE)
})

# 9. Step 4: RAPM ----

step_results[[4]] <- run_step("rapm", 4, function() {
  source("data-raw/player-ratings/04_rapm.R", local = TRUE)
})

# 10. Step 5: SPM ----

step_results[[5]] <- run_step("spm", 5, function() {
  source("data-raw/player-ratings/05_spm.R", local = TRUE)
})

# 11. Step 6: xRAPM ----

step_results[[6]] <- run_step("xrapm", 6, function() {
  source("data-raw/player-ratings/06_xrapm.R", local = TRUE)
})

# 12. Step 7: Seasonal Ratings (SPM, RAPM, xRAPM) ----

step_results[[7]] <- run_step("seasonal_ratings", 7, function() {
  source("data-raw/player-ratings/07_seasonal_ratings.R", local = TRUE)
})

# 13. Step 8: Final Ratings ----

step_results[[8]] <- run_step("panna_ratings", 8, function() {
  source("data-raw/player-ratings/08_panna_ratings.R", local = TRUE)
})

# 14. Summary ----

pipeline_end <- Sys.time()
total_duration <- difftime(pipeline_end, pipeline_start, units = "secs")

message("\n")
message(paste(rep("=", 70), collapse = ""))
message("PIPELINE COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\nStep Summary:")
message(sprintf("%-25s %-10s %s", "Step", "Status", "Duration"))
message(paste(rep("-", 50), collapse = ""))

for (result in step_results) {
  if (!is.null(result)) {
    message(sprintf("%-25s %-10s %s",
                    result$name,
                    result$status,
                    result$duration_formatted))
  }
}

message(paste(rep("-", 50), collapse = ""))
message(sprintf("%-25s %-10s %s", "TOTAL", "", format_duration(as.numeric(total_duration))))

message("\nOutput files:")
message(sprintf("  - %s", file.path(cache_dir, "07_seasonal_ratings.rds")))
message(sprintf("  - %s", file.path(cache_dir, "seasonal_spm.csv")))
message(sprintf("  - %s", file.path(cache_dir, "seasonal_rapm.csv")))
message(sprintf("  - %s", file.path(cache_dir, "seasonal_xrapm.csv")))
message(sprintf("  - %s", file.path(cache_dir, "08_panna.rds")))
message(sprintf("  - %s", file.path(cache_dir, "panna_ratings.csv")))

message("\nDone!")
