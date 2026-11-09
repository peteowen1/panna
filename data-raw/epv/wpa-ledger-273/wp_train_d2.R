# panna#273: depth-2 time-interacted WP arms (the 2026-06-19 config), unweighted and per-match weighted.
# Same training data and held-out season as wp_train_arms.R. Scored with the same table.
library(data.table); library(xgboost)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
fd <- file.path(out, "feats")
wm <- readRDS(file.path(out, "models", "wp_model.rds"))
d2_june <- readRDS("C:/dev/pannaverse/panna/data-raw/cache/epv/wp_final_d2repl_reg/wp_model.rds")
f2 <- d2_june$feature_names
add_time <- function(d) d[, tef := 1 - time_remaining][, `:=`(xmargin_x_time = xmargin * tef, epv_x_time = epv * tef)]
files <- list.files(fd, full.names = TRUE); hold_f <- grep("ENG_2025-2026", files, value = TRUE)
tr <- rbindlist(lapply(setdiff(files, hold_f), arrow::read_parquet))[!is.na(wp_label)]; add_time(tr)
tr[, n_mt := .N, by = .(match_id, team_id)][, n_m := .N, by = match_id][, w_match := (n_m / 2) / n_mt]
mono <- paste0("(", paste(as.integer(f2 %in% c("xmargin_x_time", "epv_x_time")), collapse = ","), ")")
params <- list(booster = "gbtree", objective = "reg:squarederror", eval_metric = "rmse", tree_method = "hist",
               max_depth = 2, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 50,
               monotone_constraints = mono, nthread = 6)
X <- as.matrix(tr[, ..f2])
fit <- function(w) { set.seed(42); xgb.train(params, xgb.DMatrix(X, label = tr$wp_label, weight = w), nrounds = 500, verbose = 0) }
t0 <- Sys.time()
new <- if (file.exists(file.path(out, "wp_arms_d2.rds"))) readRDS(file.path(out, "wp_arms_d2.rds")) else list(d2 = fit(NULL), d2_match = fit(tr$w_match))
cat(sprintf("trained 2 depth-2 arms in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
saveRDS(new, file.path(out, "wp_arms_d2.rds")); rm(tr, X); gc(verbose = FALSE)

old <- readRDS(file.path(out, "wp_arms.rds"))
models <- list(published = list(m = wm$model, f = wm$feature_names), match = list(m = old$match, f = wm$feature_names),
               d2_june = list(m = d2_june$model, f = f2), d2 = list(m = new$d2, f = f2), d2_match = list(m = new$d2_match, f = f2))
ho <- as.data.table(arrow::read_parquet(hold_f))[!is.na(wp_label)]; add_time(ho)
setorder(ho, match_id, period_id, time_seconds)
ho[, `:=`(tn = shift(team_id, type = "lead"), sdn = shift(score_diff, type = "lead")), by = match_id]
ho[, same := tn == team_id][, chg := sdn - score_diff]   # score_diff is home frame
ho[, kind := fcase(is.na(tn), NA_character_, chg != 0 & sign(chg) == fifelse(is_home == 1, 1, -1), "goal",
                   chg != 0, "goal_against", same, "jitter", default = "turnover")]
ll <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
res <- rbindlist(lapply(names(models), function(nm) {
  pp <- pmin(pmax(predict(models[[nm]]$m, as.matrix(ho[, models[[nm]]$f, with = FALSE])), 0), 1)
  ho[, p := pp][, pn := shift(p, type = "lead"), by = match_id][, d := fifelse(same, pn - p, (1 - pn) - p)]
  s <- ho[!is.na(kind), .(v = mean(abs(d), na.rm = TRUE)), by = kind]; g <- function(k) s[kind == k, v]
  zz <- ho[score_diff == 0 & time_seconds < 900]
  data.table(model = nm, logloss = round(ll(pp, ho$wp_label), 4), brier = round(mean((pp - ho$wp_label)^2), 4),
             swing_0_15 = round(zz[is_home == 1, mean(p)] - (1 - zz[is_home == 0, mean(p)]), 3),
             goal = round(g("goal"), 3), turnover = round(g("turnover"), 4), jitter = round(g("jitter"), 4),
             goal_per_turnover = round(g("goal") / g("turnover"), 1))
}))
cat("\n=== Held out EPL 2025-26: log loss/Brier lower is better; swing near 0 is right; jitter small is right;\n    goal_per_turnover: EPV's equivalent is ~35 ===\n")
print(res)
