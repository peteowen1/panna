# panna#273 prototype: threat consistent by construction.
# WP0 is fitted WITHOUT epv (score margin from the possession side, time, red cards, home, half, ET),
# with per-match team weights. Live threat then enters as a next-goal mixture:
#   epv >= 0: wp = (1 - epv) * WP0(m) + epv * WP0(m + 1)
#   epv <  0: wp = (1 + epv) * WP0(m) - epv * WP0(m - 1)
# so threat moves win chance exactly as much as that fraction of a goal is worth at that moment.
library(data.table); library(xgboost)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
fd <- file.path(out, "feats"); wm <- readRDS(file.path(out, "models", "wp_model.rds")); arms <- readRDS(file.path(out, "wp_arms.rds"))
files <- list.files(fd, full.names = TRUE); hold_f <- grep("ENG_2025-2026", files, value = TRUE)
f0 <- c("time_remaining", "margin", "red_card_diff", "is_home", "is_second_half", "is_extra_time")
prep <- function(d) d[, margin := round(xmargin - epv)]
tr <- rbindlist(lapply(setdiff(files, hold_f), arrow::read_parquet))[!is.na(wp_label)]; prep(tr)
cat("margin check (should be small integers):", paste(head(sort(unique(tr$margin)), 12), collapse = " "), "\n")
tr[, n_mt := .N, by = .(match_id, team_id)][, n_m := .N, by = match_id][, w := (n_m / 2) / n_mt]
mono <- paste0("(", paste(as.integer(f0 == "margin"), collapse = ","), ")")
params <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist",
               max_depth = 4, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 50,
               monotone_constraints = mono, nthread = 6)
set.seed(42)
wp0 <- xgb.train(params, xgb.DMatrix(as.matrix(tr[, ..f0]), label = tr$wp_label, weight = tr$w), nrounds = 285, verbose = 0)
saveRDS(wp0, file.path(out, "wp0_structural.rds")); rm(tr); gc(verbose = FALSE)

ho <- as.data.table(arrow::read_parquet(hold_f))[!is.na(wp_label)]; prep(ho)
P0 <- function(d, add) { x <- copy(d); x[, margin := margin + add]; predict(wp0, as.matrix(x[, ..f0])) }
p_m <- P0(ho, 0); p_up <- P0(ho, 1); p_dn <- P0(ho, -1)
e <- ho$epv
structural <- ifelse(e >= 0, (1 - e) * p_m + e * p_up, (1 + e) * p_m - e * p_dn)
preds <- list(published = predict(wm$model, as.matrix(ho[, wm$feature_names, with = FALSE])),
              match_weighted = predict(arms$match, as.matrix(ho[, wm$feature_names, with = FALSE])),
              wp0_no_threat = p_m, structural = structural)

setorder_idx <- order(ho$match_id, ho$period_id, ho$time_seconds)
ho[, rid := .I]; ho <- ho[setorder_idx]; preds <- lapply(preds, function(p) p[setorder_idx])
ho[, `:=`(tn = shift(team_id, type = "lead"), sdn = shift(score_diff, type = "lead")), by = match_id]
ho[, same := tn == team_id][, chg := sdn - score_diff]
ho[, kind := fcase(is.na(tn), NA_character_, chg != 0 & sign(chg) == fifelse(is_home == 1, 1, -1), "goal",
                   chg != 0, "goal_against", same, "jitter", default = "turnover")]
ll <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
res <- rbindlist(lapply(names(preds), function(nm) {
  pp <- preds[[nm]]; ho[, p := pp][, pn := shift(p, type = "lead"), by = match_id][, d := fifelse(same, pn - p, (1 - pn) - p)]
  s <- ho[!is.na(kind), .(v = mean(abs(d), na.rm = TRUE)), by = kind]; g <- function(k) s[kind == k, v]
  zz <- ho[score_diff == 0 & time_seconds < 900]
  data.table(model = nm, logloss = round(ll(pp, ho$wp_label), 4), brier = round(mean((pp - ho$wp_label)^2), 4),
             swing_0_15 = round(zz[is_home == 1, mean(p)] - (1 - zz[is_home == 0, mean(p)]), 3),
             goal = round(g("goal"), 3), turnover = round(g("turnover"), 4), jitter = round(g("jitter"), 4),
             goal_per_turnover = round(g("goal") / g("turnover"), 1))
}))
cat("\n=== Held out EPL 2025-26 (380 matches): log loss/Brier lower is better; swing near 0 is right;\n    jitter small is right; goal_per_turnover: EPV's is ~35 ===\n")
print(res)
saveRDS(list(ho = ho[, .(match_id, team_id, period_id, time_seconds, epv, margin, is_home, kind)], preds = preds),
        file.path(out, "wp_structural_holdout.rds"))
