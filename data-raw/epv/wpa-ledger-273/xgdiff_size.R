library(data.table); library(xgboost)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
wm <- readRDS(file.path(out, "models", "wp_model.rds")); f <- wm$feature_names
ho <- as.data.table(arrow::read_parquet(file.path(out, "feats", "wpfeat_ENG_2025-2026.parquet")))[!is.na(wp_label)]
cat("xg_diff on held-out: share non-zero", round(mean(ho$xg_diff != 0), 3), "| sd", round(sd(ho$xg_diff), 3), "\n")
p_live <- predict(wm$model, as.matrix(ho[, ..f]))
z <- copy(ho)[, xg_diff := 0]; p_dead <- predict(wm$model, as.matrix(z[, ..f]))
ll <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
cat(sprintf("log loss vs final result: xg_diff live (as trained) %.4f | xg_diff = 0 (as 10b serves) %.4f  [lower is better]\n",
            ll(p_live, ho$wp_label), ll(p_dead, ho$wp_label)))
cat(sprintf("|win chance difference| per row: mean %.4f, 90th pct %.4f, max %.3f (n = %d rows, 380 matches)\n",
            mean(abs(p_live - p_dead)), quantile(abs(p_live - p_dead), .9), max(abs(p_live - p_dead)), nrow(ho)))
