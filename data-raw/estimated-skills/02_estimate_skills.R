# 02_estimate_skills.R
# Generate skill tables using default (or optimized) decay rates
#
# Produces one row per player per season with decay-weighted skill estimates.
# This output format matches aggregate_opta_stats() and is a drop-in
# replacement for the SPM training input.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
min_minutes <- if (exists("min_minutes_spm")) min_minutes_spm else 450
min_weighted_90s <- if (exists("min_weighted_90s")) min_weighted_90s else 5
use_xmetrics_features <- if (exists("use_xmetrics_features")) use_xmetrics_features else TRUE

# 3. Load Data ----

cat("\n=== Loading Match Stats ===\n")

match_stats <- readRDS(file.path(cache_dir, "01_match_stats.rds"))
cat("Match-level rows:", nrow(match_stats), "\n")

# 4. Load or Use Default Decay Params ----

decay_params_path <- file.path(cache_dir, "02b_decay_params.rds")
if (file.exists(decay_params_path)) {
  cat("Loading optimized decay parameters...\n")
  decay_params <- readRDS(decay_params_path)
  n_lambdas <- sum(!names(decay_params) %in% c("rate", "efficiency", "xmetrics",
                                                  "prior_90s", "prior_attempts", "stat_priors"))
  n_priors <- length(decay_params$stat_priors)
  cat(sprintf("  Loaded %d stat-specific lambdas", n_lambdas))
  if (n_priors > 0) cat(sprintf(", %d per-stat priors", n_priors))
  cat("\n")
} else {
  cat("Using default decay parameters\n")
  decay_params <- get_default_decay_params()
}

# 5. Enrich with xMetrics ----

if (use_xmetrics_features) {
  cat("\n=== Enriching with xMetrics ===\n")

  # Check if xMetrics columns already exist from the match-level computation
  xm_cols <- c("xg_per90", "npxg_per90", "xa_per90_xmetrics",
                "xpass_overperformance_per90_xmetrics")
  existing_xm <- intersect(xm_cols, names(match_stats))

  if (length(existing_xm) == 0) {
    cat("No xMetrics columns in match stats. Loading separately...\n")

    # Load xMetrics data and merge
    leagues <- if (exists("leagues")) leagues else unique(match_stats$competition)
    all_xmetrics <- list()

    for (league in leagues) {
      available_seasons <- tryCatch(list_opta_seasons(league), error = function(e) character(0))
      for (season in available_seasons) {
        xm <- tryCatch(load_opta_xmetrics(league, season = season), error = function(e) NULL)
        if (!is.null(xm) && nrow(xm) > 0) {
          xm$league <- league
          xm$season <- season
          all_xmetrics[[paste(league, season)]] <- xm
        }
      }
    }

    if (length(all_xmetrics) > 0) {
      xmetrics_dt <- data.table::rbindlist(all_xmetrics, fill = TRUE)
      cat(sprintf("  Loaded %d xMetrics rows\n", nrow(xmetrics_dt)))
      # Would need to merge by match_id + player - skip for now,
      # will be handled in the match-level computation
    }
  } else {
    cat(sprintf("  xMetrics columns already present: %s\n", paste(existing_xm, collapse = ", ")))
  }
}

# 6. Estimate Skills ----

cat("\n=== Estimating Skills ===\n")

skill_features <- aggregate_skills_for_spm(
  match_stats = match_stats,
  decay_params = decay_params,
  min_minutes = min_minutes,
  min_weighted_90s = min_weighted_90s
)

cat(sprintf("Skill-based features: %d player-seasons\n", nrow(skill_features)))
cat(sprintf("Unique players: %d\n", data.table::uniqueN(skill_features$player_id)))
cat(sprintf("Seasons: %s\n", paste(sort(unique(skill_features$season_end_year)), collapse = ", ")))
cat(sprintf("Features per player: %d\n", ncol(skill_features)))

# 7. Compare with Raw Stats ----

cat("\n=== Comparing with Raw Stats ===\n")

# Load the existing SPM results from the Opta pipeline for comparison
opta_spm_path <- file.path("data-raw", "cache-opta", "05_spm.rds")
if (file.exists(opta_spm_path)) {
  opta_spm <- readRDS(opta_spm_path)
  raw_stats <- opta_spm$player_stats

  # Find overlapping players
  overlap <- intersect(skill_features$player_id, raw_stats$player_id)
  cat(sprintf("Overlapping players with raw stats: %d\n", length(overlap)))

  # Compare a few key stats
  key_stats <- c("goals_p90", "assists_p90", "tackles_won_p90", "shots_p90")
  for (stat in key_stats) {
    if (stat %in% names(skill_features) && stat %in% names(raw_stats)) {
      sf <- skill_features[player_id %in% overlap, .(player_id, skill = get(stat))]
      rf <- raw_stats[raw_stats$player_id %in% overlap, c("player_id", stat)]
      names(rf)[2] <- "raw"
      merged <- merge(sf, rf, by = "player_id")
      r <- cor(merged$skill, merged$raw, use = "complete.obs")
      cat(sprintf("  %s: skill vs raw r = %.3f\n", stat, r))
    }
  }
}

# 8. Save ----

cat("\n=== Saving Results ===\n")

saveRDS(skill_features, file.path(cache_dir, "02_skill_features.rds"))
cat("Saved to cache-skills/02_skill_features.rds\n")

cat("\n=== COMPLETE ===\n")
