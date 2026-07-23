# 13c_prior_swap_gate.R
#
# Wave 4 posterior gate (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 5.2, "test the
# thing that ships"): Pete promoted S6 for prior duty (D-W2, 2026-07-22);
# this gate checks the PROMOTED PRIOR'S downstream effect -- does per-season
# xRAPM fit with the S6-hybrid prior predict NEXT-season raw (prior-free)
# seasonal RAPM at least as well as xRAPM fit with the legacy SPM prior?
# The prior can help SPM's own score while hurting the posterior; nothing
# regenerates until this passes.
#
# Design (fairness over production-fidelity):
#   - BOTH arms are fit fresh, per season, on the CURRENT 03_splints.rds
#     (Jul-17) -- the cached 06/07 artifacts are Jun-12 vintage and NOT
#     comparable to fresh fits. Arms differ ONLY in the prior table:
#       arm A (legacy): 05_spm.rds career offense/defense_spm_ratings
#       arm B (S6):     hybrid table -- S6 panel predictions (2026 vintage,
#                       window [2021,2026) per-90s) for outfield players,
#                       legacy values for GK + players absent from the panel
#     NB production 07 scores the SPM models on season-S stats for a
#     season-specific prior; both arms here use CAREER tables so the
#     comparison isolates the prior-table swap. The production integration
#     will make its own season-scoring choice; this gate answers the
#     promotion question.
#   - Endpoint: raw seasonal RAPM for S+1, fit fresh in the same run (same
#     data vintage as the arms). Pairs >= 900 season-S minutes. Paired
#     bootstrap (n=2000) on cor(B, raw_{S+1}) - cor(A, raw_{S+1}).
#   - Seasons S in 2022:2025 (endpoints 2023:2026).
#
# Output: data-raw/spm-redesign/wave4_prior_swap_gate.csv + printed verdict.
#
# Run from panna/:
#   Rscript data-raw/spm-redesign/13c_prior_swap_gate.R

source(file.path("data-raw", "spm-redesign", "05c_candidates.R"))
suppressMessages(library(data.table))
suppressMessages(library(dplyr))

cache_dir <- file.path("data-raw", "cache-opta")
out_dir <- file.path("data-raw", "spm-redesign")
gate_seasons <- 2022:2025
endpoint_seasons <- 2023:2026
min_minutes_pairs <- 900

# --- 1. S6 fits (S4a glmnet + player-grouped-CV xgb, per target) ----

panel_bundle <- readRDS(file.path(cache_dir, "spm_panel.rds"))
panel <- panel_bundle$panel
attr(panel, "target_provenance") <- panel_bundle$target_provenance
predictor_cols <- .spm_opta_predictor_cols(panel)
xgb_params <- list(objective = "reg:squarederror", max_depth = 4, eta = 0.1,
                   subsample = 0.8, colsample_bytree = 0.8, eval_metric = "rmse")

fit_panel_xgb <- function(train_panel, target_col, seed = 1, nfolds = 5,
                          nrounds = 500, early_stopping_rounds = 20) {
  X <- as.matrix(as.data.frame(train_panel)[, predictor_cols, drop = FALSE])
  y <- train_panel[[target_col]]
  w <- train_panel$window_minutes / mean(train_panel$window_minutes, na.rm = TRUE)
  ok <- stats::complete.cases(X, y)
  X <- X[ok, , drop = FALSE]; y <- y[ok]; w <- w[ok]
  pid <- as.character(train_panel$player_id[ok])
  foldid <- make_grouped_player_foldid(pid, nfolds = nfolds, seed = seed)
  assert_grouped_player_folds(foldid, pid)
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y, weight = w)
  cv <- xgboost::xgb.cv(params = xgb_params, data = dtrain, nrounds = nrounds,
                        folds = split(seq_along(foldid), foldid),
                        early_stopping_rounds = early_stopping_rounds, verbose = 0)
  best_n <- cv$best_iteration
  if (is.null(best_n) || length(best_n) == 0) best_n <- which.min(cv$evaluation_log$test_rmse_mean)
  xgboost::xgb.train(params = xgb_params, data = dtrain, nrounds = best_n)
}

cli::cli_h1("Fitting S6 (S4a glmnet + xgb) on the full panel")
s4a_fits <- run_candidate(panel, candidate_configs$S4a, seed = 1)
xgb_off <- fit_panel_xgb(panel, "offense_target", seed = 1)
xgb_def <- fit_panel_xgb(panel, "defense_target", seed = 1)

panel_2026 <- panel[vintage_year == max(vintage_year)]
g_off <- predict_spm_panel(s4a_fits$offense, panel_2026)
g_def <- predict_spm_panel(s4a_fits$defense, panel_2026)
stopifnot(identical(g_off$player_id, panel_2026$player_id))
xg_off <- as.numeric(stats::predict(xgb_off, xgboost::xgb.DMatrix(
  as.matrix(as.data.frame(panel_2026)[, predictor_cols, drop = FALSE]))))
xg_def <- as.numeric(stats::predict(xgb_def, xgboost::xgb.DMatrix(
  as.matrix(as.data.frame(panel_2026)[, predictor_cols, drop = FALSE]))))

s6_table <- data.table(
  player_id = panel_2026$player_id,
  offense_spm_s6 = 0.5 * g_off$pred + 0.5 * xg_off,
  defense_spm_s6 = 0.5 * g_def$pred + 0.5 * xg_def
)

# --- 2. Prior tables: legacy (arm A) + S6 hybrid (arm B) ----

spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))
legacy_off <- data.table::as.data.table(spm_results$offense_spm_ratings)[, .(player_id, offense_spm)]
legacy_def <- data.table::as.data.table(spm_results$defense_spm_ratings)[, .(player_id, defense_spm)]
rm(spm_results); invisible(gc(verbose = FALSE))

hybrid_off <- merge(legacy_off, s6_table[, .(player_id, offense_spm_s6)],
                    by = "player_id", all = TRUE)
hybrid_off[, offense_spm := data.table::fifelse(!is.na(offense_spm_s6), offense_spm_s6, offense_spm)]
hybrid_off <- hybrid_off[!is.na(offense_spm), .(player_id, offense_spm)]
hybrid_def <- merge(legacy_def, s6_table[, .(player_id, defense_spm_s6)],
                    by = "player_id", all = TRUE)
hybrid_def[, defense_spm := data.table::fifelse(!is.na(defense_spm_s6), defense_spm_s6, defense_spm)]
hybrid_def <- hybrid_def[!is.na(defense_spm), .(player_id, defense_spm)]

n_s6 <- nrow(s6_table); n_legacy_only <- nrow(hybrid_off) - sum(hybrid_off$player_id %in% s6_table$player_id)
cli::cli_alert_info(sprintf("Hybrid prior: %d S6 outfield values, %d legacy fallback (GK + off-panel).",
                            n_s6, n_legacy_only))

# --- 3. Per-season fresh fits: raw (endpoints) + both xRAPM arms ----

splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))

season_fit <- function(season, arms = TRUE) {
  season_splints <- splint_data$splints[splint_data$splints$season_end_year == season, ]
  season_players <- splint_data$players[splint_data$players$splint_id %in% season_splints$splint_id, ]
  sd_list <- list(splints = season_splints, players = season_players,
                  match_info = splint_data$match_info)
  rapm_data <- prepare_rapm_data(sd_list, min_minutes = 200, include_covariates = TRUE)
  cli::cli_alert_info(sprintf("Season %d: %d players in design", season, rapm_data$n_players))
  n_folds <- max(3, min(10, floor(nrow(rapm_data$X) / 20)))

  raw_model <- fit_rapm(rapm_data, alpha = 0, nfolds = n_folds,
                        use_weights = TRUE, parallel = FALSE)
  raw <- extract_rapm_ratings(raw_model, lambda = "min")
  rm(raw_model); invisible(gc(verbose = FALSE))

  out <- list(raw = raw)
  if (arms) {
    pm <- rapm_data$player_mapping
    for (arm in c("A", "B")) {
      op <- build_prior_vector(if (arm == "A") legacy_off else hybrid_off, "offense_spm", pm)
      dp <- build_prior_vector(if (arm == "A") legacy_def else hybrid_def, "defense_spm", pm)
      cli::cli_alert_info(sprintf("Season %d arm %s: priors set %d off / %d def",
                                  season, arm, sum(op != 0), sum(dp != 0)))
      xr_model <- fit_rapm_with_prior(rapm_data, offense_prior = op, defense_prior = dp,
                                      alpha = 0, nfolds = n_folds,
                                      use_weights = TRUE, penalize_covariates = FALSE)
      out[[paste0("xrapm_", arm)]] <- extract_xrapm_ratings(xr_model, lambda = "min")
      rm(xr_model); invisible(gc(verbose = FALSE))
    }
  }
  rm(rapm_data); invisible(gc(verbose = FALSE))
  out
}

fits <- list()
for (s in sort(unique(c(gate_seasons, endpoint_seasons)))) {
  fits[[as.character(s)]] <- season_fit(s, arms = s %in% gate_seasons)
}

# --- 4. Pairs + gate ----

paired_bootstrap_delta <- function(b, a, target, n_boot = 2000, seed = 1) {
  set.seed(seed)
  n <- length(b)
  deltas <- vapply(seq_len(n_boot), function(i) {
    idx <- sample.int(n, n, replace = TRUE)
    stats::cor(b[idx], target[idx]) - stats::cor(a[idx], target[idx])
  }, numeric(1))
  list(mean = mean(deltas),
       lo = as.numeric(stats::quantile(deltas, 0.025, names = FALSE)),
       hi = as.numeric(stats::quantile(deltas, 0.975, names = FALSE)),
       p_gt0 = mean(deltas > 0))
}

rows <- list(); all_pairs <- list()
for (s in gate_seasons) {
  f <- fits[[as.character(s)]]
  nxt <- fits[[as.character(s + 1)]]$raw
  a <- data.table::as.data.table(f$xrapm_A)[, .(player_id, xrapm_A = xrapm, total_minutes)]
  b <- data.table::as.data.table(f$xrapm_B)[, .(player_id, xrapm_B = xrapm)]
  tgt <- data.table::as.data.table(nxt)[, .(player_id, raw_next = rapm)]
  pairs <- Reduce(function(x, y) merge(x, y, by = "player_id"), list(a, b, tgt))
  pairs <- pairs[total_minutes >= min_minutes_pairs]
  pairs[, season := s]
  all_pairs[[as.character(s)]] <- pairs
  rows[[as.character(s)]] <- data.table(
    season = s, n = nrow(pairs),
    cor_legacy = stats::cor(pairs$xrapm_A, pairs$raw_next),
    cor_s6 = stats::cor(pairs$xrapm_B, pairs$raw_next)
  )
}
pooled <- data.table::rbindlist(all_pairs)
boot <- paired_bootstrap_delta(pooled$xrapm_B, pooled$xrapm_A, pooled$raw_next)
gate <- data.table::rbindlist(rows)
gate <- rbind(gate, data.table(season = NA_integer_, n = nrow(pooled),
                               cor_legacy = stats::cor(pooled$xrapm_A, pooled$raw_next),
                               cor_s6 = stats::cor(pooled$xrapm_B, pooled$raw_next)))
gate[, `:=`(boot_delta = boot$mean, ci_lo = boot$lo, ci_hi = boot$hi, p_gt0 = boot$p_gt0)]
data.table::fwrite(gate, file.path(out_dir, "wave4_prior_swap_gate.csv"))

# cat(), not cli_h1: cli glue-interpolates {braces}, and "raw_{S+1}" cost
# this script its first full 45-minute run at the final print (2026-07-22).
cat("\n== Wave 4 prior-swap gate (xRAPM_S -> raw next season, both arms fresh) ==\n")
print(gate)
if (!is.na(boot$lo) && boot$lo > 0) {
  cat("GATE PASSED: S6 prior BEATS legacy (95% CI > 0).\n")
} else if (!is.na(boot$hi) && boot$hi < 0) {
  cat("GATE FAILED: S6 prior LOSES to legacy (95% CI < 0) -- do not regenerate.\n")
} else {
  cat("GATE NEUTRAL: no significant difference (CI straddles 0) -- swap is prediction-neutral; promotion rests on the D-W2 structural grounds.\n")
}
