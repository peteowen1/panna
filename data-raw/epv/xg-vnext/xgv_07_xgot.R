# xGOT next round: same design as the xG round (xgv_05), on on-target shots.
# =============================================================================
# Population: prepare_shots_for_xgot() (on target, not blocked, goal-mouth point
# known, seasons ending 2021+), minus penalties, goal-only feeds and shots with
# no event row. Train seasons ending <= 2025; test 2025-26 and 2026-27.
# Sets:
#   G0 today     the published xGOT's 17 inputs (shot spot, body, situation,
#                big chance, where it crossed the line)
#   G1 season    + season_num (big-chance tagging drifts; xGOT never had it)
#   G2 context   + the xG round's F4 context (fast break, 1 on 1, lob, individual
#                play, intentional assist, rebound, assist type / length / origin,
#                possession, score difference, minute, weak foot)
# Settings: depth {4, 6, 8} x min_child_weight 50 tuned on G2 (grouped CV by
# match), eta 0.1, no rounds cap. Each fit saved to fits_xgot/ and skipped on
# a re-run. Yardsticks on the test rows: the xGOT published today (pack) and
# the previous one (2026-07-23).
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_07_xgot.R  (detached)
suppressMessages({library(data.table); library(arrow); library(dplyr); library(xgboost); devtools::load_all(quiet = TRUE)})
source(file.path(Sys.getenv("USERPROFILE"), ".claude/lib/runtime_log.R"))
OD <- "C:/dev/pannaverse/pannadata/data/opta/"; X <- "data-raw/cache/epv/xg-vnext"
FD <- file.path(X, "fits_xgot"); dir.create(FD, showWarnings = FALSE)
say <- function(...) { cat(format(Sys.time(), "%H:%M:%S "), ..., "\n", sep = ""); flush.console() }

sh <- as.data.table(collect(open_dataset(paste0(OD, "opta_shot_events.parquet"))))
sh[, event_id := as.character(event_id)]
ft <- as.data.table(prepare_shots_for_xgot(as.data.frame(sh)))
ft[, event_id := as.character(event_id)]
ft <- ft[is_penalty == 0]
sf <- as.data.table(read_parquet(file.path(X, "shot_features.parquet")))
CTX <- c("season_num", "competition", "season", "period_id", "q23", "q89", "q117", "q215", "q154", "rebound",
         "has_assist", "a2", "a4", "a195", "a155", "a156", "a168", "a1", "a5", "a6", "a107", "a_len", "a_x", "a_y",
         "poss_secs", "poss_passes", "score_diff", "minute", "foot_share")
ft <- merge(ft, sf[, c("match_id", "event_id", CTX), with = FALSE], by = c("match_id", "event_id"))
ft <- ft[period_id %in% 1:4 & !is.na(rebound) & !is.na(season_num)]
cov <- ft[, .(nongoal = sum(is_goal == 0)), by = .(competition, season)]
ft <- ft[!cov[nongoal == 0], on = .(competition, season)]
ft[, goal := is_goal]
for (v in CTX) if (is.logical(ft[[v]])) set(ft, j = v, value = as.integer(ft[[v]]))

pub_new <- readRDS("data-raw/cache/epv/pack-2026-09/xgot_model.rds")
pub_old <- readRDS("C:/dev/_model-backups/2026-09-23/pannamodels-epv/xgot_model.rds")
G0 <- pub_new$panna_metadata$feature_cols
SETS <- list(G0_today = G0, G1_season = c(G0, "season_num"),
             G2_context = c(G0, setdiff(CTX, c("competition", "season", "period_id"))))
tr <- ft[season_num <= 2025]; teA <- ft[season_num == 2026]; teB <- ft[season_num == 2027]
say("on-target shots ", nrow(ft), " | train ", nrow(tr), " (", uniqueN(tr$match_id), " matches) | test 2025-26 ", nrow(teA),
    " | 2026-27 ", nrow(teB))
set.seed(2026)
mids <- unique(tr$match_id); fold_of <- setNames(sample(rep(1:5, length.out = length(mids))), mids)
folds <- lapply(1:5, function(k) which(fold_of[tr$match_id] == k))
dm <- function(dt, f) xgb.DMatrix(as.matrix(dt[, ..f]), label = dt$goal, missing = NA)
BASE <- list(objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist", eta = 0.1,
             subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 50L, nthread = 22)
cv_fit <- function(tag, f, depth) {
  path <- file.path(FD, paste0(tag, ".rds")); if (file.exists(path)) return(readRDS(path))
  say("start ", tag); t0 <- Sys.time()
  cv <- xgb.cv(params = c(BASE, max_depth = depth), data = dm(tr, f), nrounds = 5000, folds = folds,
               early_stopping_rounds = 50, verbose = 1, print_every_n = 200)
  el <- cv$evaluation_log; best <- cv$early_stop$best_iteration %||% cv$best_iteration %||% which.min(el$test_logloss_mean)
  r <- list(tag = tag, features = f, depth = depth, best = best, cv_logloss = el$test_logloss_mean[best],
            cv_sd = el$test_logloss_std[best], mins = as.numeric(difftime(Sys.time(), t0, units = "mins")))
  saveRDS(r, path)
  say(sprintf("%-22s rounds %4d  cv logloss %.5f (sd %.5f)  %.1f min", tag, best, r$cv_logloss, r$cv_sd, r$mins)); r
}
grid <- rbindlist(lapply(c(4L, 6L, 8L), function(dp) { r <- cv_fit(sprintf("grid_d%d", dp), SETS$G2_context, dp)
  data.table(depth = dp, rounds = r$best, cv_logloss = r$cv_logloss, cv_sd = r$cv_sd) }))
print(grid); DEPTH <- grid[which.min(cv_logloss), depth]; say("chosen depth ", DEPTH)
fits <- lapply(names(SETS), function(s) {
  cvr <- cv_fit(paste0("set_", s), SETS[[s]], DEPTH)
  mp <- file.path(FD, paste0("model_", s, ".rds"))
  if (!file.exists(mp)) saveRDS(list(model = xgb.train(params = c(BASE, max_depth = DEPTH), data = dm(tr, SETS[[s]]),
                                                       nrounds = cvr$best, verbose = 0), features = SETS[[s]], cv = cvr), mp)
  readRDS(mp)
}); names(fits) <- names(SETS)

ll <- function(y, p) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
card <- rbindlist(lapply(list(`2025-26` = teA, `2026-27` = teB), function(te) {
  preds <- c(lapply(fits, function(f) { fs <- f$features; predict(f$model, xgb.DMatrix(as.matrix(te[, ..fs]), missing = NA)) }),
             list(published_today = predict_xgot(pub_new, as.data.frame(te)),
                  published_0723 = suppressWarnings(predict_xgot(pub_old, as.data.frame(te)))))
  cs <- list(all = rep(TRUE, nrow(te)), header = te$is_header == 1, foot = te$is_header == 0,
             set_piece = te$is_set_piece == 1 | te$is_corner == 1, big_chance = te$is_big_chance == 1,
             not_big_chance = te$is_big_chance == 0, fast_break = te$q23 == 1, rebound = te$rebound == 1,
             weak_foot = !is.na(te$foot_share) & te$foot_share <= 0.2)
  rbindlist(lapply(names(preds), function(nm) rbindlist(lapply(names(cs), function(cn) {
    k <- cs[[cn]] & !is.na(preds[[nm]])
    data.table(model = nm, cut = cn, shots = sum(k), goals = sum(te$goal[k]), goals_per_xgot = sum(te$goal[k]) / sum(preds[[nm]][k]),
               logloss = if (cn == "all") ll(te$goal[k], preds[[nm]][k]) else NA_real_)
  }))))
}), idcol = "test")
fwrite(card, file.path(X, "xgot_report_card.csv"))
imp <- rbindlist(lapply(names(fits), function(s) as.data.table(xgb.importance(model = fits[[s]]$model))[, .(set = s, Feature, gain = round(100 * Gain, 1))]))
fwrite(imp, file.path(X, "xgot_importance.csv"))
say("held-out log-loss (lower is better)"); print(dcast(card[cut == "all"], model ~ test, value.var = "logloss"))
say("DONE")
