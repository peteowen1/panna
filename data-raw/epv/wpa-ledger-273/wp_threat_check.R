# panna#273: is the WP model's reaction to live threat (epv) consistent with its reaction to a goal?
# epv is in goals (expected goals from this possession), so a consistent model moves win chance about
# as much per 1.0 of epv as per 1 goal of lead. Partial dependence on real held-out rows (EPL 2025-26):
# take level-score rows in a minute band, set epv to a value (xmargin moves with it, since
# xmargin = margin + epv), average the prediction. Same for margin +1 at the rows' own epv.
library(data.table); library(xgboost)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
wm <- readRDS(file.path(out, "models", "wp_model.rds")); feats <- wm$feature_names
arms <- readRDS(file.path(out, "wp_arms.rds"))
models <- list(published = wm$model, match_weighted = arms$match)
ho <- as.data.table(arrow::read_parquet(file.path(out, "feats", "wpfeat_ENG_2025-2026.parquet")))[!is.na(wp_label)]
cat("epv on held-out rows: quantiles 1/50/90/99%:", paste(signif(quantile(ho$epv, c(.01, .5, .9, .99)), 3), collapse = " / "),
    "| share of rows with epv > 0.1:", round(mean(ho$epv > 0.1), 3), "\n")
lvl <- ho[score_diff == 0]
lvl[, band := cut(time_seconds / 60, c(-1, 15, 45, 75, 200), labels = c("0-15", "15-45", "45-75", "75+"))]
set.seed(1); lvl <- lvl[, .SD[sample(.N, min(.N, 4000))], by = band]
pd <- function(m, d, epv_to = NULL, margin_add = 0) {
  x <- copy(d); base_margin <- x$xmargin - x$epv
  if (!is.null(epv_to)) x[, epv := epv_to]
  x[, xmargin := base_margin + margin_add + epv]
  mean(predict(m, as.matrix(x[, ..feats])))
}
res <- rbindlist(lapply(names(models), function(nm) rbindlist(lapply(levels(lvl$band), function(b) {
  d <- lvl[band == b]; m <- models[[nm]]
  e0 <- pd(m, d, 0); e10 <- pd(m, d, 0.10); e30 <- pd(m, d, 0.30)
  g0 <- pd(m, d); g1 <- pd(m, d, margin_add = 1)
  data.table(model = nm, minutes = b, n = nrow(d),
             wp_epv0 = round(e0, 3), wp_epv0.1 = round(e10, 3), wp_epv0.3 = round(e30, 3),
             per_epv_unit = round((e30 - e0) / 0.30, 3), per_goal = round(g1 - g0, 3),
             ratio_epv_to_goal = round(((e30 - e0) / 0.30) / (g1 - g0), 2))
}))))
cat("\n=== Level score: win chance of the team in possession as its threat rises, vs one goal of lead ===\n")
cat("per_epv_unit = change in win chance per 1.0 of epv (0 -> 0.3, scaled); per_goal = change for +1 goal;\n")
cat("ratio near 1 = consistent; well above 1 = threat over-weighted\n")
print(res)

# What does a typical same-team step change epv by? (the jitter's input)
setorder(ho, match_id, period_id, time_seconds)
ho[, `:=`(en = shift(epv, type = "lead"), tn = shift(team_id, type = "lead")), by = match_id]
st <- ho[tn == team_id]
cat(sprintf("\nSame-team consecutive steps: mean |change in epv| = %.4f goals (n = %d)\n", mean(abs(st$en - st$epv), na.rm = TRUE), nrow(st)))
cat(sprintf("Possession flips: mean epv before %.4f, after (new team) %.4f\n",
            ho[tn != team_id, mean(epv)], ho[tn != team_id, mean(en)]))

cat("
=== Inside the real range of epv (1st to 99th percentile is -0.04 to 0.08) ===
")
grid <- c(-0.04, 0, 0.02, 0.04, 0.06, 0.08)
g2 <- rbindlist(lapply(names(models), function(nm) rbindlist(lapply(c("0-15", "75+"), function(b) {
  d <- lvl[band == b]; m <- models[[nm]]
  v <- sapply(grid, function(e) pd(m, d, e)); g <- pd(m, d, margin_add = 1) - pd(m, d)
  data.table(model = nm, minutes = b, epv = grid, wp = round(v, 3),
             per_goal = round(g, 3), slope_vs_goal = round(c(NA, diff(v) / diff(grid)) / g, 1))
}))))
print(dcast(g2, model + minutes + per_goal ~ epv, value.var = "wp"))
cat("local slope (win chance per 1.0 of epv, between grid points) divided by per-goal change; 1 = consistent:
")
print(dcast(g2[!is.na(slope_vs_goal)], model + minutes ~ epv, value.var = "slope_vs_goal"))
