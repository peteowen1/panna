# 09c_blend_candidate_eval.R
#
# Wave 2 extension: S6 = S4a + XGBoost half (per-target 50/50 blend), the
# BLEND-PARITY candidate. Motivation (2026-07-22 bake-off finding): no
# glmnet-only candidate clears S0's sec 5.2 CI bar (pooled 0.612-0.625 vs
# 0.675), and the gap is near-uniform across vintages -- S0's structural
# advantages are its glmnet+XGB blend (~+0.03-0.04 next-season correlation
# per FABLE-MULTITARGET's prior-layer estimate) and all-history feature
# aggregation. Plan sec 3.3 keeps the blend for PRIOR duty (per-game
# pricing stays glmnet-only regardless -- trees export no per-stat
# prices), so the seasonal-promotion comparison against a blended S0 is
# only apples-to-apples with a blended candidate. S6 answers: does the
# panel design win once blend parity is granted?
#
# Leak discipline identical to 06c: per eval vintage Y, BOTH halves train
# only on vintage_year <= Y (glmnet via run_candidate_asof; xgb on the
# same asserted subset). XGBoost CV folds are PLAYER-GROUPED
# (make_grouped_player_foldid -> xgb.cv folds=), NOT fit_spm_xgb()'s
# random folds -- panel rows repeat players across vintages, so random
# folds would leak players between train/test and overfit nrounds
# (plan R5).
#
# Outputs (data-raw/spm-redesign/):
#   wave2_blend_gate.csv  S6 per-vintage + pooled rows, bootstrap vs S0,
#                         and (if wave2_bakeoff_results.rds is present)
#                         bootstrap vs the cached S4a pairs.
#
# Run from panna/:
#   Rscript data-raw/spm-redesign/09c_blend_candidate_eval.R

EVAL_NEXTWINDOW_SKIP_MAIN <- TRUE
source(file.path("data-raw", "spm-redesign", "06c_eval_nextwindow.R"))  # helpers + load_all()
source(file.path("data-raw", "spm-redesign", "05c_candidates.R"))
suppressMessages(library(data.table))

cache_dir <- file.path("data-raw", "cache-opta")
out_dir <- file.path("data-raw", "spm-redesign")
blend_w_glmnet <- 0.5  # production SPM_BLEND_WEIGHT_GLMNET parity
xgb_params <- list(objective = "reg:squarederror", max_depth = 4, eta = 0.1,
                   subsample = 0.8, colsample_bytree = 0.8, eval_metric = "rmse")

panel_bundle <- readRDS(file.path(cache_dir, "spm_panel.rds"))
panel <- panel_bundle$panel
attr(panel, "target_provenance") <- panel_bundle$target_provenance
rapm_window_targets <- readRDS(file.path(cache_dir, "rapm_window_targets.rds"))
s0_pred <- load_s0_baseline(cache_dir)

panel_vintages <- sort(unique(panel$vintage_year))
eval_vintages <- panel_vintages[panel_vintages < max(panel_vintages)]
predictor_cols <- .spm_opta_predictor_cols(panel)

#' Player-grouped-CV XGBoost fit on a (pre-subset, as-of) panel slice.
fit_panel_xgb <- function(train_panel, target_col, seed = 1, nfolds = 5,
                          nrounds = 500, early_stopping_rounds = 20) {
  X <- as.matrix(as.data.frame(train_panel)[, predictor_cols, drop = FALSE])
  y <- train_panel[[target_col]]
  mins <- train_panel$window_minutes
  w <- mins / mean(mins, na.rm = TRUE)  # linear weights, S4a parity

  ok <- stats::complete.cases(X, y)
  X <- X[ok, , drop = FALSE]; y <- y[ok]; w <- w[ok]
  pid <- as.character(train_panel$player_id[ok])

  foldid <- make_grouped_player_foldid(pid, nfolds = nfolds, seed = seed)
  assert_grouped_player_folds(foldid, pid)
  folds <- split(seq_along(foldid), foldid)

  dtrain <- xgboost::xgb.DMatrix(data = X, label = y, weight = w)
  cv <- xgboost::xgb.cv(params = xgb_params, data = dtrain, nrounds = nrounds,
                        folds = folds, early_stopping_rounds = early_stopping_rounds,
                        verbose = 0)
  best_n <- cv$best_iteration
  if (is.null(best_n) || length(best_n) == 0) best_n <- which.min(cv$evaluation_log$test_rmse_mean)
  xgboost::xgb.train(params = xgb_params, data = dtrain, nrounds = best_n)
}

predict_panel_xgb <- function(model, newdata) {
  X <- as.matrix(as.data.frame(newdata)[, predictor_cols, drop = FALSE])
  as.numeric(stats::predict(model, xgboost::xgb.DMatrix(X)))
}

per_vintage_rows <- list()
pairs_list <- list()

for (Y in eval_vintages) {
  next_entry <- rapm_window_targets[[as.character(Y + 1)]]
  if (is.null(next_entry)) next
  panel_Y <- panel[vintage_year == Y]
  if (nrow(panel_Y) == 0) next
  cli::cli_h2(sprintf("S6 as-of %d", Y))

  # glmnet half: S4a config, leak-free (asserts vintage_year <= Y inside)
  fits <- run_candidate_asof(panel, Y, candidate_configs$S4a, seed = 1)

  # xgb half: same as-of subset, structurally asserted like run_candidate_asof
  train_panel <- panel[vintage_year <= Y]
  assert_asof_panel_window(train_panel, Y)
  xgb_off <- fit_panel_xgb(train_panel, "offense_target", seed = 1)
  xgb_def <- fit_panel_xgb(train_panel, "defense_target", seed = 1)

  g_off <- predict_spm_panel(fits$offense, panel_Y)
  g_def <- predict_spm_panel(fits$defense, panel_Y)
  stopifnot(identical(g_off$player_id, panel_Y$player_id),
            identical(g_def$player_id, panel_Y$player_id))
  x_off <- predict_panel_xgb(xgb_off, panel_Y)
  x_def <- predict_panel_xgb(xgb_def, panel_Y)

  blend_off <- blend_w_glmnet * g_off$pred + (1 - blend_w_glmnet) * x_off
  blend_def <- blend_w_glmnet * g_def$pred + (1 - blend_w_glmnet) * x_def
  # Net = offense + defense (defense positive=good since 2026-09-04; see
  # predict_spm_panel_net()'s docstring -- the two conventions must always
  # travel together, don't revert this citing the 2026-07-22 sign-bug note,
  # which was about the OLD convention).
  candidate_pred <- data.table(player_id = panel_Y$player_id,
                               pred_net = blend_off + blend_def)

  pairs <- build_vintage_pairs(candidate_pred, panel_Y, s0_pred,
                               as.data.table(next_entry$ratings), min_minutes = 900)
  pairs_list[[as.character(Y)]] <- pairs
  per_vintage_rows[[as.character(Y)]] <- pearson_row(pairs, Y)
  print(per_vintage_rows[[as.character(Y)]])
}

pooled_pairs <- rbindlist(pairs_list, idcol = "vintage")
pooled_row <- pearson_row(pooled_pairs, "pooled")
boot_s0 <- paired_bootstrap_delta(pooled_pairs$candidate, pooled_pairs$s0,
                                  pooled_pairs$target_next, n_boot = 2000, seed = 1)

cli::cli_h1("S6 (S4a + xgb blend): pooled")
print(pooled_row)
cli::cli_alert_info(sprintf(
  "S6 vs S0 paired bootstrap: mean=%.4f, 95%% CI [%.4f, %.4f], P(delta>0)=%.3f",
  boot_s0$mean_delta, boot_s0$ci_lo, boot_s0$ci_hi, boot_s0$p_gt0))

# S6 vs S4a paired bootstrap on the SAME (vintage, player) pairs, if the
# bake-off cache is present.
boot_s4a <- NULL
bakeoff_path <- file.path(out_dir, "wave2_bakeoff_results.rds")
if (file.exists(bakeoff_path)) {
  s4a_pairs <- readRDS(bakeoff_path)$S4a$pairs[, .(vintage, player_id, s4a = candidate)]
  merged <- merge(pooled_pairs, s4a_pairs, by = c("vintage", "player_id"))
  cli::cli_alert_info(sprintf("S6-vs-S4a pair overlap: %d of %d S6 pairs", nrow(merged), nrow(pooled_pairs)))
  boot_s4a <- paired_bootstrap_delta(merged$candidate, merged$s4a, merged$target_next,
                                     n_boot = 2000, seed = 1)
  cli::cli_alert_info(sprintf(
    "S6 vs S4a paired bootstrap: mean=%.4f, 95%% CI [%.4f, %.4f], P(delta>0)=%.3f",
    boot_s4a$mean_delta, boot_s4a$ci_lo, boot_s4a$ci_hi, boot_s4a$p_gt0))
}

gate <- rbindlist(c(per_vintage_rows, list(pooled_row)))
gate[, `:=`(candidate = "S6",
            sd_pred_pooled = stats::sd(pooled_pairs$candidate),
            sd_target_pooled = stats::sd(pooled_pairs$target_next),
            boot_vs_s0_delta = boot_s0$mean_delta, boot_vs_s0_lo = boot_s0$ci_lo,
            boot_vs_s0_hi = boot_s0$ci_hi,
            boot_vs_s4a_delta = if (!is.null(boot_s4a)) boot_s4a$mean_delta else NA_real_,
            boot_vs_s4a_lo = if (!is.null(boot_s4a)) boot_s4a$ci_lo else NA_real_,
            boot_vs_s4a_hi = if (!is.null(boot_s4a)) boot_s4a$ci_hi else NA_real_)]
fwrite(gate, file.path(out_dir, "wave2_blend_gate.csv"))
cli::cli_alert_success("Wrote {.path {file.path(out_dir, 'wave2_blend_gate.csv')}}")
