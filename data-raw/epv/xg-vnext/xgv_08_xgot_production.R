# xGOT next round, production fit of set G2 (Pete, 2026-09-24): every season 2021+,
# depth 6 / min_child_weight 50 / eta 0.1, rounds from grouped 5-fold CV, and
# out-of-fold calibration by season and cut. CV saved the moment it exists.
# (Header below is the shared data prep from xgv_07_xgot.R.)
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

G2 <- readRDS(file.path(FD, "model_G2_context.rds"))$features
d <- ft
say("production xGOT: ", nrow(d), " on-target shots, seasons ", min(d$season_num), "-", max(d$season_num), " | ", length(G2), " inputs")
PAR <- list(objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist", max_depth = 6L,
            min_child_weight = 50L, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8, nthread = 22)
set.seed(2026)
mids <- unique(d$match_id); fold_of <- setNames(sample(rep(1:5, length.out = length(mids))), mids)
folds <- lapply(1:5, function(k) which(fold_of[d$match_id] == k))
dm <- xgb.DMatrix(as.matrix(d[, ..G2]), label = d$goal, missing = NA)
CVF <- file.path(X, "xgot_prod_cv.rds")
if (file.exists(CVF)) cvs <- readRDS(CVF) else {
  cv <- xgb.cv(params = PAR, data = dm, nrounds = 5000, folds = folds, early_stopping_rounds = 50,
               prediction = TRUE, verbose = 1, print_every_n = 200)
  cvs <- list(el = cv$evaluation_log, best = cv$early_stop$best_iteration %||% cv$best_iteration,
              oof = as.numeric(cv$cv_predict$pred %||% cv$pred))
  saveRDS(cvs, CVF)
}
stopifnot(length(cvs$oof) == nrow(d), all(is.finite(cvs$oof)))
best <- cvs$best; el <- cvs$el
say("rounds ", best, " | cv logloss ", round(el$test_logloss_mean[best], 5), " (sd ", round(el$test_logloss_std[best], 5), ")")
d[, oof := cvs$oof]
m <- xgb.train(params = PAR, data = dm, nrounds = best, verbose = 0)
cal <- rbind(d[, .(cut = "all", shots = .N, goals = sum(goal), goals_per_xgot = sum(goal) / sum(oof)), by = season_num],
             d[is_header == 1, .(cut = "header", shots = .N, goals = sum(goal), goals_per_xgot = sum(goal) / sum(oof)), by = season_num],
             d[is_set_piece == 1 | is_corner == 1, .(cut = "set piece", shots = .N, goals = sum(goal), goals_per_xgot = sum(goal) / sum(oof)), by = season_num])
fwrite(cal, file.path(X, "xgot_calib_by_season.csv"))
say("out-of-fold goals per xGOT by season"); print(dcast(cal, season_num ~ cut, value.var = "goals_per_xgot"), digits = 3)
bins <- d[, .(shots = .N, pred = mean(oof), actual = mean(goal)), by = .(bin = cut(oof, c(0, .05, .1, .2, .3, .5, .7, .9, 1)))][order(bin)]
say("out-of-fold calibration by predicted bin"); print(bins, digits = 3)
saveRDS(list(model = m, feature_names = G2, best_nrounds = best, best_logloss = el$test_logloss_mean[best], cv_log = el,
             calibration_by_season = cal, calibration_bins = bins,
             importance = as.data.table(xgb.importance(model = m))[, .(Feature, gain = round(100 * Gain, 1))],
             panna_metadata = list(type = "xgot_model", version = "v3 (2026-09-24, xg-vnext G2)", feature_cols = G2,
                                   n_shots = nrow(d), n_goals = sum(d$goal), goal_rate = mean(d$goal), params = PAR,
                                   seasons = range(d$season_num), exclude_penalties = TRUE,
                                   note = "needs xgv_03/xgv_04 context features at scoring time; not yet wired")),
        file.path(X, "xgot_model_v3.rds"))
say("saved xgot_model_v3.rds"); say("DONE")
