# panna#273 fix 1: does weighting the WP training rows remove the "ball premium"?
# Arms, identical data and params (published config: binary:logistic, depth 4, eta 0.1, mcw 50,
# subsample/colsample 0.8, monotone +1 on xmargin and epv, 285 rounds = the published model's):
#   base   - every action one row (production)
#   match  - each team's actions in a match weighted to the same total
#   bucket - each team's actions in each 5-minute slice of a match weighted to the same total
# Held out: EPL 2025-26 (not in training). Also scored: the published model.
library(data.table); library(xgboost)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
fd <- file.path(out, "feats")
wm <- readRDS(file.path(out, "models", "wp_model.rds")); feats <- wm$feature_names
files <- list.files(fd, full.names = TRUE)
hold_f <- grep("ENG_2025-2026", files, value = TRUE)
tr <- rbindlist(lapply(setdiff(files, hold_f), arrow::read_parquet))
ho <- as.data.table(arrow::read_parquet(hold_f))
cat(sprintf("train: %d rows, %d matches, %d league-seasons | holdout: %d rows, %d matches\n",
            nrow(tr), uniqueN(tr$match_id), uniqueN(tr[, paste(league, season)]), nrow(ho), uniqueN(ho$match_id)))
cat("wp_label values:", paste(sort(unique(tr$wp_label)), collapse = " "), "\n")
tr <- tr[!is.na(wp_label)]

# 0. The label itself, before any model: possession team's win rate at 0-0 in the first 15 minutes.
cat("\n=== Training labels, 0-0, minutes 0-15: mean wp_label of the team in possession ===\n")
z <- tr[score_diff == 0 & time_seconds < 900]
print(z[, .(rows = .N, team_matches = uniqueN(paste(match_id, team_id)), mean_label = round(mean(wp_label), 3),
            mean_label_teammatch_weighted = round(weighted.mean(wp_label, 1 / ave(rep(1, .N), match_id, team_id, FUN = sum)), 3)),
        by = .(side = fifelse(is_home == 1, "home has ball", "away has ball"))])

# Weights
tr[, n_mt := .N, by = .(match_id, team_id)][, n_m := .N, by = match_id]
tr[, w_match := (n_m / 2) / n_mt]
tr[, b := floor(time_seconds / 300)]
tr[, n_bt := .N, by = .(match_id, b, team_id)][, n_b := .N, by = .(match_id, b)]
tr[, w_bucket := pmin((n_b / 2) / n_bt, 10)]
tr[, w_bucket := w_bucket / mean(w_bucket)]
cat(sprintf("\nweights: match mean %.3f sd %.3f | bucket mean %.3f sd %.3f max %.2f\n",
            mean(tr$w_match), sd(tr$w_match), mean(tr$w_bucket), sd(tr$w_bucket), max(tr$w_bucket)))

mono <- paste0("(", paste(as.integer(feats %in% c("xmargin", "epv")), collapse = ","), ")")
params <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist",
               max_depth = 4, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 50,
               monotone_constraints = mono, nthread = 6)
X <- as.matrix(tr[, ..feats])
fit <- function(w) { set.seed(42); xgb.train(params, xgb.DMatrix(X, label = tr$wp_label, weight = w), nrounds = wm$optimal_nrounds, verbose = 0) }
t0 <- Sys.time()
models <- list(published = wm$model, base = fit(NULL), match = fit(tr$w_match), bucket = fit(tr$w_bucket))
cat(sprintf("trained 3 arms in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
saveRDS(models[-1], file.path(out, "wp_arms.rds"))

# Held-out scoring
Xh <- as.matrix(ho[, ..feats]); ho <- ho[!is.na(wp_label)]; Xh <- as.matrix(ho[, ..feats])
ll <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
res <- rbindlist(lapply(names(models), function(nm) {
  p <- predict(models[[nm]], Xh); ho[, (nm) := p]
  zz <- ho[score_diff == 0]
  zz[, band := cut(time_seconds / 60, c(-1, 15, 30, 45, 60, 75, 200))]
  sw <- zz[, .(h = mean(get(nm)[is_home == 1]), a = mean(get(nm)[is_home == 0])), by = band]
  sw[, swing := h - (1 - a)]
  data.table(model = nm, logloss = round(ll(p, ho$wp_label), 4), brier = round(mean((p - ho$wp_label)^2), 4),
             swing_0_15 = round(sw[band == "(-1,15]", swing], 3), swing_30_45 = round(sw[band == "(30,45]", swing], 3),
             swing_60_75 = round(sw[band == "(60,75]", swing], 3))
}))
cat("\n=== Held out, EPL 2025-26 (380 matches): log loss / Brier vs the final result (lower is better);\n    swing = home's win chance with the ball minus without it at 0-0 (near 0 is right) ===\n")
print(res)

# Per-action change on held-out: how big is a failed-pass-style possession flip vs a goal?
setorder(ho, match_id, period_id, time_seconds)
for (nm in names(models)) {
  ho[, nx := shift(get(nm), type = "lead"), by = match_id][, tn := shift(team_id, type = "lead"), by = match_id]
  ho[, (paste0("d_", nm)) := fifelse(tn == team_id, nx - get(nm), (1 - nx) - get(nm))]
}
ho[, flip := shift(team_id, type = "lead") != team_id, by = match_id]
ho[, goal_next := shift(score_diff, type = "lead") != score_diff & shift(team_id, type = "lead") == team_id |
                    shift(score_diff, type = "lead") != -score_diff & shift(team_id, type = "lead") != team_id, by = match_id]
cat("\n=== Held out: mean |change| on a possession flip at 0-0 with no goal, by minute ===\n")
q <- ho[flip %in% TRUE & score_diff == 0 & !goal_next %in% TRUE]
q[, band := cut(time_seconds / 60, c(-1, 15, 45, 75, 200))]
print(q[, c(list(n = .N), lapply(.SD, function(x) round(mean(abs(x), na.rm = TRUE), 4))), by = band,
        .SDcols = paste0("d_", names(models))][order(band)])
