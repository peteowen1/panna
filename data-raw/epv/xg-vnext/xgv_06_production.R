# xG next round, step 6: the production fit of set F4 (Pete, 2026-09-24).
# =============================================================================
# F4 = today's inputs + season + shot tags (23, 89, 117, 215, 154) + rebound +
# assist / possession build-up + score difference, minute, weak foot. The
# tagging rate (F5) and tag 468 are out: F5 was worse on both future seasons.
# Trained on EVERY season (the time split only chose the set). Settings from
# xgv_05: depth 4, min_child_weight 50, eta 0.1. Rounds from 5-fold CV grouped
# by match; the out-of-fold predictions give calibration by season and by
# header on shots each fold never saw.
#
# Output: data-raw/cache/epv/xg-vnext/xg_model_v5.rds (panna's xG object shape:
# model, feature_cols, cv, calibration) and calib_by_season.csv.
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_06_production.R  (detached)
suppressMessages({library(data.table); library(arrow); library(xgboost)})
source(file.path(Sys.getenv("USERPROFILE"), ".claude/lib/runtime_log.R"))
X <- "data-raw/cache/epv/xg-vnext"
say <- function(...) { cat(format(Sys.time(), "%H:%M:%S "), ..., "\n", sep = ""); flush.console() }
OUT <- file.path(X, "xg_model_v5.rds")

d <- as.data.table(read_parquet(file.path(X, "shot_features.parquet")))
d <- d[is_penalty == 0 & period_id %in% 1:4 & !is.na(rebound) & !is.na(season_num)]
cov <- d[, .(nongoal = sum(goal == 0)), by = .(competition, season)]
d <- d[!cov[nongoal == 0], on = .(competition, season)]
FEAT <- readRDS(file.path(X, "fits", "model_F4_state_foot.rds"))$features
for (v in FEAT) if (is.logical(d[[v]])) set(d, j = v, value = as.integer(d[[v]]))
say("training shots ", nrow(d), " from ", uniqueN(d$match_id), " matches, seasons ", min(d$season_num), "-", max(d$season_num),
    " | ", length(FEAT), " inputs")
PAR <- list(objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist", max_depth = 4L,
            min_child_weight = 50L, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8, nthread = 22)
set.seed(2026)
mids <- unique(d$match_id); fold_of <- setNames(sample(rep(1:5, length.out = length(mids))), mids)
folds <- lapply(1:5, function(k) which(fold_of[d$match_id] == k))
dm <- xgb.DMatrix(as.matrix(d[, ..FEAT]), label = d$goal, missing = NA)

CVF <- file.path(X, "prod_cv.rds")      # saved the moment it exists: a crash below must not cost the 20-min CV
if (file.exists(CVF)) cvs <- readRDS(CVF) else {
  cv <- rt_stage("production cv", xgb.cv(params = PAR, data = dm, nrounds = 5000, folds = folds, early_stopping_rounds = 50,
                                         prediction = TRUE, verbose = 1, print_every_n = 200))
  # xgboost 3.x: out-of-fold predictions in cv$cv_predict$pred, best round in cv$early_stop
  cvs <- list(el = cv$evaluation_log, best = cv$early_stop$best_iteration %||% cv$best_iteration,
              oof = as.numeric(cv$cv_predict$pred %||% cv$pred))
  saveRDS(cvs, CVF)
}
el <- cvs$el; best <- cvs$best %||% which.min(el$test_logloss_mean)
stopifnot(length(cvs$oof) == nrow(d), all(is.finite(cvs$oof)))
say("rounds ", best, " | cv logloss ", round(el$test_logloss_mean[best], 5), " (sd ", round(el$test_logloss_std[best], 5), ")")
d[, oof := cvs$oof]
m <- rt_stage("production fit", xgb.train(params = PAR, data = dm, nrounds = best, verbose = 0))

cal <- rbind(
  d[, .(cut = "all", shots = .N, goals = sum(goal), goals_per_xg = sum(goal) / sum(oof)), by = season_num],
  d[is_header == 1, .(cut = "header", shots = .N, goals = sum(goal), goals_per_xg = sum(goal) / sum(oof)), by = season_num],
  d[is_set_piece == 1 | is_corner == 1, .(cut = "set piece", shots = .N, goals = sum(goal), goals_per_xg = sum(goal) / sum(oof)), by = season_num])
setorder(cal, cut, season_num); fwrite(cal, file.path(X, "calib_by_season.csv"))
say("out-of-fold goals per xG by season (1.00 is perfect)")
print(dcast(cal[season_num >= 2014], season_num ~ cut, value.var = "goals_per_xg"), digits = 3)
bins <- d[, .(shots = .N, pred = mean(oof), actual = mean(goal)), by = .(bin = cut(oof, c(0, .02, .05, .1, .2, .3, .5, .7, 1)))][order(bin)]
say("out-of-fold calibration by predicted bin"); print(bins, digits = 3)
imp <- as.data.table(xgb.importance(model = m))[, .(Feature, gain = round(100 * Gain, 1))]
saveRDS(list(model = m, feature_names = FEAT, best_nrounds = best, best_logloss = el$test_logloss_mean[best],
             cv_log = el, calibration_by_season = cal, calibration_bins = bins, importance = imp,
             panna_metadata = list(type = "xg_model", version = "v5 (2026-09-24, xg-vnext F4)", feature_cols = FEAT,
                                   n_shots = nrow(d), n_goals = sum(d$goal), goal_rate = mean(d$goal), params = PAR,
                                   seasons = range(d$season_num), exclude_penalties = TRUE,
                                   note = "needs xgv_03/xgv_04 context features at scoring time; not yet wired into add_xg_to_spadl")),
        OUT)
say("saved ", OUT); say("DONE")
