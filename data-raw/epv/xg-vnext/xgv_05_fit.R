# xG next round, step 5: five nested feature sets, validated on time.
# =============================================================================
# Train: seasons ending <= 2025 (to 2024-25). Test A: 2025-26. Test B: 2026-27.
# Training shots: non-penalty, periods 1-4, with an event row (context), not from
# a goal-only feed, not an own goal. Excluded as outcome-revealing: tag 217 (goals
# only), tag 468 (meaning unknown, 1.74x xG; held back until Pete decides).
#
# Settings: a small grid (depth x min_child_weight) tuned once, on set F4, on a
# 30% sample of training MATCHES (whole matches kept), with 5-fold CV GROUPED BY
# MATCH; every set then refits on all training shots with the winner, nrounds
# chosen by the same grouped CV with early stopping and no practical cap.
# Learning rate 0.1 throughout (first design used 0.05 and 8 threads: ~10 h).
# Every fit is saved to OUT/fits/ and skipped on a re-run.
#
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_05_fit.R  (launch detached)
suppressMessages({library(data.table); library(arrow); library(xgboost)})
source(file.path(Sys.getenv("USERPROFILE"), ".claude/lib/runtime_log.R"))
X <- "data-raw/cache/epv/xg-vnext"; FD <- file.path(X, "fits"); dir.create(FD, showWarnings = FALSE)
say <- function(...) { cat(format(Sys.time(), "%H:%M:%S "), ..., "\n", sep = ""); flush.console() }

d <- as.data.table(read_parquet(file.path(X, "shot_features.parquet")))
n0 <- nrow(d)
d <- d[is_penalty == 0 & period_id %in% 1:4 & !is.na(rebound) & !is.na(season_num)]
cov <- d[, .(nongoal = sum(goal == 0)), by = .(competition, season)]
d <- d[!cov[nongoal == 0], on = .(competition, season)]
say("shots: ", n0, " -> ", nrow(d), " after filters")

BASE <- c("x", "y", "distance_to_goal", "angle_to_goal", "in_penalty_area", "in_six_yard_box",
          "is_header", "is_right_foot", "is_left_foot", "is_open_play", "is_set_piece", "is_corner", "is_big_chance")
SETS <- list(
  F0_today     = BASE,
  F1_season    = c(BASE, "season_num"),
  F2_tags      = c(BASE, "season_num", "q23", "q89", "q117", "q215", "q154", "rebound"),
  F3_buildup   = c(BASE, "season_num", "q23", "q89", "q117", "q215", "q154", "rebound",
                   "has_assist", "a2", "a4", "a195", "a155", "a156", "a168", "a1", "a5", "a6", "a107",
                   "a_len", "a_x", "a_y", "poss_secs", "poss_passes"),
  F4_state_foot = NULL, F5_tagrate = NULL)
SETS$F4_state_foot <- c(SETS$F3_buildup, "score_diff", "minute", "foot_share")
SETS$F5_tagrate    <- c(SETS$F4_state_foot, "bc_rate_prev", "bc_rate_asof")
for (v in unique(unlist(SETS))) if (is.logical(d[[v]])) set(d, j = v, value = as.integer(d[[v]]))

tr <- d[season_num <= 2025]; teA <- d[season_num == 2026]; teB <- d[season_num == 2027]
say("train ", nrow(tr), " (", uniqueN(tr$match_id), " matches) | test 2025-26 ", nrow(teA), " | test 2026-27 ", nrow(teB))
set.seed(2026)
mids <- unique(tr$match_id); fold_of <- setNames(sample(rep(1:5, length.out = length(mids))), mids)
folds <- lapply(1:5, function(k) which(fold_of[tr$match_id] == k))
tune_mids <- sample(mids, round(0.3 * length(mids)))
tt <- tr[match_id %chin% tune_mids]
tfolds <- lapply(1:5, function(k) which(fold_of[tt$match_id] == k))
say("tuning sample: ", nrow(tt), " shots from ", length(tune_mids), " matches")
dm <- function(dt, f) xgb.DMatrix(as.matrix(dt[, ..f]), label = dt$goal, missing = NA)
base_par <- list(objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist",
                 subsample = 0.8, colsample_bytree = 0.8, nthread = 22)

cv_fit <- function(tag, f, par, eta, cap = 5000, data = tr, fl = folds) {
  path <- file.path(FD, paste0(tag, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  t0 <- Sys.time()
  say("start ", tag)
  cv <- xgb.cv(params = c(base_par, par, eta = eta), data = dm(data, f), nrounds = cap, folds = fl,
               early_stopping_rounds = 50, verbose = 1, print_every_n = 100)
  el <- cv$evaluation_log
  # xgboost 3.x keeps the early-stopping round under cv$early_stop; older under cv$best_iteration
  best <- cv$early_stop$best_iteration %||% cv$best_iteration %||% which.min(el$test_logloss_mean)
  stopifnot(length(best) == 1, is.finite(best), best >= 1)
  res <- list(tag = tag, features = f, par = par, eta = eta, best = best, hit_cap = best >= cap - 50,
              cv_logloss = el$test_logloss_mean[best], cv_sd = el$test_logloss_std[best],
              mins = as.numeric(difftime(Sys.time(), t0, units = "mins")))
  saveRDS(res, path)
  say(sprintf("%-28s rounds %4d  cv logloss %.5f (sd %.5f)  %.1f min", tag, best, res$cv_logloss, res$cv_sd, res$mins))
  res
}

# ---- 1. tune on F4 ------------------------------------------------------------
GRID <- data.table(max_depth = c(4L, 6L, 8L, 6L), min_child_weight = c(50L, 50L, 50L, 200L))
grid_res <- rbindlist(lapply(seq_len(nrow(GRID)), function(i) {
  p <- as.list(GRID[i]); r <- rt_stage(paste("grid", i), cv_fit(sprintf("grid_d%d_m%d", p$max_depth, p$min_child_weight),
                                                                 SETS$F4_state_foot, p, eta = 0.1, data = tt, fl = tfolds))
  data.table(max_depth = p$max_depth, min_child_weight = p$min_child_weight, rounds = r$best, cv_logloss = r$cv_logloss, cv_sd = r$cv_sd)
}))
fwrite(grid_res, file.path(X, "grid.csv")); print(grid_res[order(cv_logloss)])
BEST <- as.list(grid_res[which.min(cv_logloss), .(max_depth, min_child_weight)])
say("chosen settings: depth ", BEST$max_depth, ", min_child_weight ", BEST$min_child_weight)

# ---- 2. every set with the chosen settings --------------------------------------
fits <- lapply(names(SETS), function(s) {
  cvr <- rt_stage(paste("cv", s), cv_fit(paste0("set_", s), SETS[[s]], BEST, eta = 0.1))
  mpath <- file.path(FD, paste0("model_", s, ".rds"))
  if (!file.exists(mpath)) {
    m <- xgb.train(params = c(base_par, BEST, eta = 0.1), data = dm(tr, SETS[[s]]), nrounds = cvr$best, verbose = 0)
    saveRDS(list(model = m, features = SETS[[s]], cv = cvr), mpath)
  }
  readRDS(mpath)
})
names(fits) <- names(SETS)

# ---- 3. report card -----------------------------------------------------------------
ll <- function(y, p) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
cuts <- function(te) list(all = rep(TRUE, nrow(te)), header = te$is_header == 1, foot = te$is_header == 0,
                          set_piece = te$is_set_piece == 1 | te$is_corner == 1, open_play = te$is_open_play == 1,
                          big_chance = te$is_big_chance == 1, not_big_chance = te$is_big_chance == 0,
                          fast_break = te$q23 == 1, rebound = te$rebound == 1, through_ball = te$a4 %in% 1,
                          weak_foot = !is.na(te$foot_share) & te$foot_share <= 0.2)
card <- rbindlist(lapply(list(`2025-26` = teA, `2026-27` = teB), function(te) {
  preds <- c(lapply(fits, function(f) { fs <- f$features; predict(f$model, xgb.DMatrix(as.matrix(te[, ..fs]), missing = NA)) }), list(published = te$xg_pub))
  cs <- cuts(te)
  rbindlist(lapply(names(preds), function(nm) rbindlist(lapply(names(cs), function(cn) {
    k <- cs[[cn]] & !is.na(preds[[nm]])
    data.table(model = nm, cut = cn, shots = sum(k), goals = sum(te$goal[k]),
               goals_per_xg = sum(te$goal[k]) / sum(preds[[nm]][k]),
               logloss = if (cn == "all") ll(te$goal[k], preds[[nm]][k]) else NA_real_,
               brier = if (cn == "all") mean((te$goal[k] - preds[[nm]][k])^2) else NA_real_)
  }))))
}), idcol = "test")
fwrite(card, file.path(X, "report_card.csv"))
imp <- rbindlist(lapply(names(fits), function(s) {
  i <- as.data.table(xgb.importance(model = fits[[s]]$model)); i[, set := s][, .(set, Feature, gain = round(100 * Gain, 1))]
}))
fwrite(imp, file.path(X, "importance.csv"))
say("CV summary"); print(rbindlist(lapply(fits, function(f) data.table(set = f$cv$tag, rounds = f$cv$best, hit_cap = f$cv$hit_cap,
                                                                         cv_logloss = f$cv$cv_logloss, cv_sd = f$cv$cv_sd))))
say("held-out log-loss (lower is better)"); print(dcast(card[cut == "all"], model ~ test, value.var = "logloss"))
say("DONE")
