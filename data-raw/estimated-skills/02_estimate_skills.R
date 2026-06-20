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
# Skills are a CONTINUOUS career trait, not season-gated — see
# CLAUDE_TODO_CONTINUOUS_SKILLS.md. The per-season minutes gate (`min_minutes`,
# formerly 450) was a publishing filter that dropped players with rich career
# history but low current-season minutes (e.g. F. Chiesa). We now gate ONLY on
# CAREER sample via `min_career_w90` (decay-weighted 90s, applied below), which is
# self-limiting for the long-retired. min_minutes = 0 keeps the "played at least
# once this season" snapshot membership without a minutes floor.
# NOTE: `min_weighted_90s` is NOT an inclusion gate — it's the estimator's
# regression threshold (shrinkage is handled by the Bayesian prior; see
# estimate_player_skills docs). The real inclusion gate is `min_career_w90`.
min_minutes <- if (exists("min_minutes_spm")) min_minutes_spm else 0
min_weighted_90s <- if (exists("min_weighted_90s")) min_weighted_90s else 5
min_career_w90 <- if (exists("min_career_w90")) min_career_w90 else 3
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
#
# Join per-MATCH xG / finishing over-performance onto match_stats by
# (player_id, match_id). This is the real join that the old stub never did —
# without it the value model has no xG at all. Source: xmetrics_bymatch/
# (produced by 03_calculate_player_xmetrics.R via aggregate_player_xmetrics(
# by_match = TRUE)). Columns are renamed with an _xmetrics suffix where they
# would collide with box-score names.

if (use_xmetrics_features) {
  match_stats <- data.table::as.data.table(match_stats)

  # xmetrics per-90 columns to attach (source name -> match_stats name)
  xm_map <- c(
    xg_per90 = "xg_per90", npxg_per90 = "npxg_per90",
    xa_per90 = "xa_per90_xmetrics",
    xpass_overperformance_per90 = "xpass_overperformance_per90_xmetrics",
    npg_minus_npxg_per90 = "npg_minus_npxg_per90",
    ibox_g_minus_xg_per90 = "ibox_g_minus_xg_per90",
    obox_g_minus_xg_per90 = "obox_g_minus_xg_per90",
    placement_added_per90 = "placement_added_per90",  # xGOT - xG: placement skill
    gsaa_per90 = "gsaa_per90"
  )

  if (!all(c("league", "season", "match_id", "player_id") %in% names(match_stats))) {
    warning("match_stats missing league/season/match_id/player_id — skipping xMetrics join",
            call. = FALSE)
  } else {
    ls_pairs <- unique(match_stats[, .(league, season)])
    cat(sprintf("  Joining per-match xMetrics over %d league-seasons...\n", nrow(ls_pairs)))

    xm_list <- vector("list", nrow(ls_pairs))
    n_missing <- 0L
    for (i in seq_len(nrow(ls_pairs))) {
      lg <- ls_pairs$league[i]; sn <- ls_pairs$season[i]
      xm_list[[i]] <- tryCatch({
        x <- data.table::as.data.table(
          load_opta_xmetrics(lg, season = sn, source = "local", by_match = TRUE))
        keep <- intersect(c("player_id", "match_id", names(xm_map)), names(x))
        x[, ..keep]
      }, error = function(e) { n_missing <<- n_missing + 1L; NULL })
    }
    xm <- data.table::rbindlist(Filter(Negate(is.null), xm_list), fill = TRUE)

    if (nrow(xm) == 0) {
      warning(sprintf(paste0(
        "No per-match xMetrics found (xmetrics_bymatch/ absent for all %d league-seasons). ",
        "Re-run data-raw/epv/03_calculate_player_xmetrics.R to generate them. ",
        "Proceeding WITHOUT xG features."), nrow(ls_pairs)), call. = FALSE)
    } else {
      # rename source -> target, then left-join onto match_stats
      old <- intersect(names(xm_map), names(xm))
      data.table::setnames(xm, old, unname(xm_map[old]))
      xm <- unique(xm, by = c("player_id", "match_id"))
      match_stats <- merge(match_stats, xm, by = c("player_id", "match_id"),
                           all.x = TRUE)
      added <- intersect(unname(xm_map), names(match_stats))
      # Over-performance / xG are genuine 0 for players with no shots that match
      for (col in added) data.table::set(match_stats, which(is.na(match_stats[[col]])), col, 0)
      cat(sprintf("  xMetrics joined: %d cols (%s); %d/%d league-seasons missing bymatch files\n",
                  length(added), paste(added, collapse = ", "), n_missing, nrow(ls_pairs)))
    }
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

if (is.null(skill_features) || nrow(skill_features) == 0) {
  stop("aggregate_skills_for_spm() returned no results. Check min_minutes threshold and data availability.")
}

# Career-sample inclusion gate (continuous-skills design). With the per-season
# minutes floor removed, this is the ONLY gate: keep player-seasons whose
# decay-weighted career sample clears min_career_w90, dropping thin-sample noise
# (estimates near the prior). Self-limiting — a player's weighted_90s decays
# below the threshold a few years after they stop playing. Coverage 80% -> 85.6%.
skill_features <- data.table::as.data.table(skill_features)
n_pre <- nrow(skill_features)
skill_features <- skill_features[weighted_90s >= min_career_w90]
cat(sprintf("Career-sample gate (weighted_90s >= %s): kept %d of %d player-seasons (dropped %d thin-sample)\n",
            min_career_w90, nrow(skill_features), n_pre, n_pre - nrow(skill_features)))

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

  if (length(overlap) > 0) {
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
  } else {
    warning(sprintf(
      paste0("Zero overlap in player_id between skills (%s, e.g. '%s') and SPM (%s, e.g. '%s'). ",
             "Diagnostic comparison skipped. If IDs should match, downstream joins will also fail."),
      class(skill_features$player_id), skill_features$player_id[1],
      class(raw_stats$player_id), raw_stats$player_id[1]
    ), call. = FALSE, immediate. = TRUE)
  }
}

# 8. Save ----

cat("\n=== Saving Results ===\n")

saveRDS(skill_features, file.path(cache_dir, "02_skill_features.rds"))
cat("Saved to cache-skills/02_skill_features.rds\n")

cat("\n=== COMPLETE ===\n")
