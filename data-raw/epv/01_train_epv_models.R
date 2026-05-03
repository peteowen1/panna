# 01_train_epv_models.R
# Train xG, xPass, and EPV models on Opta event data
#
# Run from panna directory: Rscript data-raw/epv/01_train_epv_models.R
#
# Outputs:
#   - data-raw/cache/epv/xg_model.rds
#   - data-raw/cache/epv/xpass_model.rds
#   - data-raw/cache/epv/epv_model.rds

library(cli)
devtools::load_all()

# 1. Configuration ----

# Train on all 15 leagues (uses OPTA_LEAGUES from opta_loaders.R)
if (!exists("LEAGUES")) LEAGUES <- names(OPTA_LEAGUES)
if (!exists("SEASONS")) SEASONS <- c("2020-2021", "2021-2022", "2022-2023", "2023-2024")

# Tournament seasons use "YYYY Country" format (not "YYYY-YYYY")
TOURNAMENT_SEASONS <- list(
  WC   = c("2014 Brazil", "2018 Russia"),
  EURO = c("2016 France", "2024 Germany")
)

# EPV method: "goal" (multinomial) or "xg" (regression on signed next-shot xG)
EPV_METHOD <- "xg"

# XGBoost parameters
XGB_PARAMS <- list(
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 0
)

# Output directory
CACHE_DIR <- "data-raw/cache/epv"
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

cli_h1("EPV Model Training Pipeline")
cli_alert_info("Leagues: {paste(LEAGUES, collapse = ', ')}")
cli_alert_info("Seasons: {paste(SEASONS, collapse = ', ')}")

# 2. Load Data, Build Chains, Create Labels (per-chunk to fit in 7GB) ----
#
# Streams each (league, season) through the full chain pipeline immediately
# after loading and writes the labeled output to a per-chunk parquet under
# cache_dir before moving to the next iteration. Replaces an in-memory
# `all_spadl` accumulation + a single `create_possession_chains()` on 22M
# rows — the latter peaked at ~10GB working memory and OOM-killed the
# 7GB GHA runner. All chain operations are by = match_id, so chunking by
# (league, season) is safe — chains never span matches.
#
# Mirrors the `step-08b` chunk-stream playbook: flat-named chunks under
# cache_dir (not tempdir, which has been observed to vanish mid-run on
# GHA), no leading dot (list.files default skips hidden), gc(full = TRUE)
# every 5th iteration to bound peak.

cli_h2("Step 1: Load Opta + Build Labeled Chains (per-chunk)")

LABELED_CHUNKS_DIR <- file.path(CACHE_DIR, "labeled_chunks")
dir.create(LABELED_CHUNKS_DIR, recursive = TRUE, showWarnings = FALSE)

# Cleanup residue from any prior crashed run before we begin
.cleanup_labeled_chunks <- function() {
  files <- list.files(LABELED_CHUNKS_DIR, pattern = "^chunk_.*\\.parquet$", full.names = TRUE)
  if (length(files) > 0) unlink(files, force = TRUE)
}
.cleanup_labeled_chunks()

all_shots <- list()
all_lineups <- list()
total_events <- 0L
total_actions <- 0L
total_chains <- 0L
loaded_leagues <- character(0)
iter_count <- 0L

# Build league → season list (domestic use SEASONS, tournaments use TOURNAMENT_SEASONS)
league_seasons <- list()
for (league in LEAGUES) {
  if (league %in% names(TOURNAMENT_SEASONS)) {
    league_seasons[[league]] <- TOURNAMENT_SEASONS[[league]]
  } else {
    league_seasons[[league]] <- SEASONS
  }
}

for (league in LEAGUES) {
  for (season in league_seasons[[league]]) {
    key <- paste(league, season)
    iter_count <- iter_count + 1L

    chunk_name <- sprintf("chunk_%s_%s.parquet",
                          gsub("[^A-Za-z0-9]", "_", league),
                          gsub("[^A-Za-z0-9]", "_", season))
    chunk_path <- file.path(LABELED_CHUNKS_DIR, chunk_name)

    tryCatch({
      events <- load_opta_match_events(league, season = season, source = "local")
      shot_events <- load_opta_shot_events(league, season = season, source = "local")
      lineups <- load_opta_lineups(league, season = season, source = "local")

      # Convert to SPADL and tag with league
      spadl_chunk <- convert_opta_to_spadl(events)
      spadl_chunk$league <- league

      # Run the full chain + label pipeline on just this chunk. Free each
      # intermediate as soon as the next one is built.
      chains_chunk <- create_possession_chains(spadl_chunk)
      rm(spadl_chunk); gc(verbose = FALSE)

      outcomes_chunk <- classify_chain_outcomes(chains_chunk)
      outcomes_chunk <- add_next_chain_outcome(outcomes_chunk)
      labeled_chunk <- label_actions_with_outcomes(chains_chunk, outcomes_chunk)
      rm(chains_chunk); gc(verbose = FALSE)

      labeled_chunk <- create_next_goal_labels(labeled_chunk)
      if (EPV_METHOD == "xg") {
        labeled_chunk <- create_next_xg_labels(labeled_chunk)
      }

      arrow::write_parquet(labeled_chunk, chunk_path)

      all_shots[[key]] <- shot_events
      all_lineups[[key]] <- lineups
      total_events <- total_events + nrow(events)
      total_actions <- total_actions + nrow(labeled_chunk)
      total_chains <- total_chains + nrow(outcomes_chunk)
      loaded_leagues <- union(loaded_leagues, league)

      cli_alert_success("  {key}: {format(nrow(events), big.mark=',')} events -> {format(nrow(labeled_chunk), big.mark=',')} labeled actions, {format(nrow(outcomes_chunk), big.mark=',')} chains -> {basename(chunk_path)}")

      rm(labeled_chunk, outcomes_chunk, events, shot_events, lineups)
      # Full gc every 5 iterations to bound long-running heap creep
      gc(verbose = FALSE, full = (iter_count %% 5 == 0))
    }, error = function(e) {
      cli_alert_warning("  Skipping {key}: {e$message}")
    })
  }
}

# Concat the small per-chunk shots/lineups (these don't OOM — total size is
# in the hundreds of MB, well below the chain-creation peak).
shots <- do.call(rbind, all_shots)
lineups <- do.call(rbind, all_lineups)
n_leagues_loaded <- length(loaded_leagues)
rm(all_shots, all_lineups); gc(verbose = FALSE, full = TRUE)

n_chunks <- length(list.files(LABELED_CHUNKS_DIR, pattern = "^chunk_.*\\.parquet$"))
cli_alert_success("Total: {format(total_events, big.mark=',')} events -> {format(total_actions, big.mark=',')} labeled actions, {format(total_chains, big.mark=',')} chains from {n_leagues_loaded} leagues across {n_chunks} chunks")

# 3. Per-Chunk Feature Generation + Sampling for Training ----
#
# Materializing the full 22M-row spadl_labeled in one shot OOMs at ~10GB on
# the 7GB runner (arrow buffers + data.table conversion overhead), even after
# the chain-creation peak is gone. So instead, read one chunk at a time,
# generate features per-chunk, and accumulate a pre-sampled training set.
#
# Why per-chunk for EPV and not row-level after-the-fact: create_epv_features_simple
# uses shift(..., by = .(match_id, period_id)) for previous-action context.
# Random row sampling would shred those lags. Per-(league, season) chunks
# preserve match continuity (chains never span matches), so feature-gen
# inside the chunk is safe.
#
# Why row-level sampling is safe for xPass: create_pass_features is fully
# vectorized — no shift, no setorder, no by-match operations. Sample
# anywhere.

cli_h2("Step 2: Per-chunk feature generation + sampling")

MAX_XPASS_ROWS <- 2000000L
MAX_EPV_ROWS <- 5000000L

chunk_files <- list.files(LABELED_CHUNKS_DIR, pattern = "^chunk_.*\\.parquet$", full.names = TRUE)
# 5% / 10% overshoot so the per-chunk ceiling absorbs uneven chunk sizes;
# we trim to exact targets after the loop.
epv_target_per_chunk <- ceiling(MAX_EPV_ROWS / length(chunk_files) * 1.05)
xpass_target_per_chunk <- ceiling(MAX_XPASS_ROWS / length(chunk_files) * 1.10)

cli_alert_info("Per-chunk targets: EPV {format(epv_target_per_chunk, big.mark=',')} rows, xPass {format(xpass_target_per_chunk, big.mark=',')} rows × {length(chunk_files)} chunks")

epv_features_acc <- vector("list", length(chunk_files))
spadl_labeled_acc <- vector("list", length(chunk_files))
pass_features_acc <- vector("list", length(chunk_files))

set.seed(42)
for (i in seq_along(chunk_files)) {
  chunk_dt <- arrow::read_parquet(chunk_files[[i]]) |> data.table::as.data.table()

  # EPV features (must be created on full chunk because of per-match lags)
  epv_features_chunk <- create_epv_features_simple(chunk_dt) |> data.table::as.data.table()
  if (nrow(chunk_dt) > 0) {
    n_take <- min(epv_target_per_chunk, nrow(chunk_dt))
    sample_idx <- sample(nrow(chunk_dt), n_take)
    epv_features_acc[[i]] <- epv_features_chunk[sample_idx]
    spadl_labeled_acc[[i]] <- chunk_dt[sample_idx]
  }
  rm(epv_features_chunk)

  # xPass features (vectorized, safe to sample after generation)
  passes_chunk <- chunk_dt[action_type == "pass"]
  if (nrow(passes_chunk) > 0) {
    pass_features_chunk <- prepare_passes_for_xpass(passes_chunk) |> data.table::as.data.table()
    n_take <- min(xpass_target_per_chunk, nrow(pass_features_chunk))
    pass_features_acc[[i]] <- pass_features_chunk[sample(nrow(pass_features_chunk), n_take)]
    rm(pass_features_chunk)
  }
  rm(chunk_dt, passes_chunk)
  if (i %% 5 == 0) gc(verbose = FALSE, full = TRUE)
}

# Concatenate samples
epv_features <- data.table::rbindlist(epv_features_acc, fill = TRUE)
spadl_labeled <- data.table::rbindlist(spadl_labeled_acc, fill = TRUE)
pass_features <- data.table::rbindlist(pass_features_acc, fill = TRUE)
rm(epv_features_acc, spadl_labeled_acc, pass_features_acc); gc(verbose = FALSE, full = TRUE)

# Trim to exact targets if per-chunk overshoot pushed us over
if (nrow(epv_features) > MAX_EPV_ROWS) {
  trim_idx <- sample(nrow(epv_features), MAX_EPV_ROWS)
  epv_features <- epv_features[trim_idx]
  spadl_labeled <- spadl_labeled[trim_idx]
}
if (nrow(pass_features) > MAX_XPASS_ROWS) {
  pass_features <- pass_features[sample(nrow(pass_features), MAX_XPASS_ROWS)]
}

cli_alert_success("Sampled — epv: {format(nrow(epv_features), big.mark=',')} rows / spadl_labeled: {format(nrow(spadl_labeled), big.mark=',')} rows / pass_features: {format(nrow(pass_features), big.mark=',')} rows")

# 5. Train xG Model ----

cli_h2("Step 4: Train xG Model")

shot_features <- prepare_shots_for_xg(shots)
xg_model <- fit_xg_model(shot_features,
                          nrounds = XGB_PARAMS$nrounds,
                          early_stopping_rounds = XGB_PARAMS$early_stopping_rounds,
                          verbose = XGB_PARAMS$verbose)

cli_alert_success("xG Model: best iter={xg_model$best_nrounds}, logloss={round(xg_model$best_logloss, 4)}")
saveRDS(xg_model, file.path(CACHE_DIR, "xg_model.rds"))

# 6. Train xPass Model ----

cli_h2("Step 5: Train xPass Model")

# pass_features was generated and sampled per-chunk in Step 2 to bound peak
# memory. Subsampling here would be redundant.
xpass_model <- fit_xpass_model(pass_features,
                                nrounds = XGB_PARAMS$nrounds,
                                early_stopping_rounds = XGB_PARAMS$early_stopping_rounds,
                                verbose = XGB_PARAMS$verbose)

cli_alert_success("xPass Model: best iter={xpass_model$best_nrounds}, logloss={round(xpass_model$best_logloss, 4)}")
saveRDS(xpass_model, file.path(CACHE_DIR, "xpass_model.rds"))

# 7. Train EPV Model (simple features with league) ----

cli_h2("Step 6: Train EPV Model (simple features)")

# epv_features and spadl_labeled were generated and aligned-sampled per-chunk
# in Step 2 to bound peak memory. Subsampling here would be redundant.
epv_model <- fit_epv_model(epv_features, spadl_labeled,
                            method = EPV_METHOD,
                            nrounds = XGB_PARAMS$nrounds,
                            early_stopping_rounds = XGB_PARAMS$early_stopping_rounds,
                            verbose = XGB_PARAMS$verbose)

# Tag model metadata for simple feature mode
epv_model$panna_metadata$feature_mode <- "simple"

metric_name <- if (EPV_METHOD == "goal") "mlogloss" else "rmse"
cli_alert_success("EPV Model: best iter={epv_model$best_nrounds}, {metric_name}={round(epv_model$best_metric, 4)}")
cli_alert_info("  Trained on {n_leagues_loaded} leagues, {format(epv_model$panna_metadata$n_actions, big.mark=',')} actions")
# Save with method suffix (epv_model_goal.rds or epv_model_xg.rds)
epv_method_file <- paste0("epv_model_", EPV_METHOD, ".rds")
saveRDS(epv_model, file.path(CACHE_DIR, epv_method_file))
saveRDS(epv_model, file.path(CACHE_DIR, "epv_model.rds"))  # also save as default
cli_alert_success("Saved: {epv_method_file} + epv_model.rds (default)")

# 8. Save to Pannadata ----

cli_h2("Step 7: Save Models to Pannadata")

# Save models to pannadata/opta/models/ for package use
pannadata_models <- file.path(opta_data_dir(), "models")
if (!dir.exists(pannadata_models)) dir.create(pannadata_models, recursive = TRUE)

saveRDS(xg_model, file.path(pannadata_models, "xg_model.rds"))
saveRDS(xpass_model, file.path(pannadata_models, "xpass_model.rds"))
saveRDS(epv_model, file.path(pannadata_models, epv_method_file))
saveRDS(epv_model, file.path(pannadata_models, "epv_model.rds"))

cli_alert_success("Models saved to {pannadata_models}/")
cli_alert_info("  - xg_model.rds")
cli_alert_info("  - xpass_model.rds")
cli_alert_info("  - {epv_method_file} + epv_model.rds (default)")

# 9. Summary ----

cli_h1("Training Complete!")

cli_alert_info("Models also cached to {CACHE_DIR}/")

cat("\nFeature Importance (EPV model, top 15):\n")
print(head(epv_model$importance, 15))

cat("\nLeague distribution in training data:\n")
print(table(spadl_labeled$league))
