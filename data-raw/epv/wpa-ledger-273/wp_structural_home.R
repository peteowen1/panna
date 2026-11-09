# panna#273 prototype 2: no possession premium by construction.
# H = P(home wins, draw = 0.5) from HOME-frame score margin and time only (no "who has the ball").
# The possession team's win chance is H (home) or 1 - H (away); threat enters as the next-goal mixture.
library(data.table); library(xgboost)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
fd <- file.path(out, "feats"); S <- readRDS(file.path(out, "wp_structural_holdout.rds"))
files <- list.files(fd, full.names = TRUE); hold_f <- grep("ENG_2025-2026", files, value = TRUE)
tr <- rbindlist(lapply(setdiff(files, hold_f), arrow::read_parquet))[!is.na(wp_label)]
# Frame check: score_diff is home frame (established). red_card_diff: does it flip with possession?
setorder(tr, match_id, period_id, time_seconds)
rc <- tr[red_card_diff != 0][, .(flips = uniqueN(sign(red_card_diff)), sides = uniqueN(is_home)), by = match_id]
cat(sprintf("red_card_diff: of %d matches with a red card, %d show both signs while possession changes sides -> %s frame\n",
            nrow(rc), rc[sides == 2 & flips == 2, .N], if (rc[sides == 2 & flips == 2, .N] > 0.8 * rc[sides == 2, .N]) "POSSESSION" else "HOME"))
poss_frame_red <- rc[sides == 2 & flips == 2, .N] > 0.8 * rc[sides == 2, .N]
to_home <- function(d) {
  d[, `:=`(y_home = fifelse(is_home == 1, wp_label, 1 - wp_label), m_home = score_diff,
           red_home = if (poss_frame_red) fifelse(is_home == 1, red_card_diff, -red_card_diff) else red_card_diff)]
}
to_home(tr)
fh <- c("time_remaining", "m_home", "red_home", "is_second_half", "is_extra_time")
params <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist",
               max_depth = 4, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 50,
               monotone_constraints = "(0,1,1,0,0)", nthread = 6)
set.seed(42)
H <- xgb.train(params, xgb.DMatrix(as.matrix(tr[, ..fh]), label = tr$y_home), nrounds = 285, verbose = 0)
saveRDS(H, file.path(out, "wp_home_frame.rds")); rm(tr); gc(verbose = FALSE)

ho <- as.data.table(arrow::read_parquet(hold_f))[!is.na(wp_label)]; to_home(ho)
setorder(ho, match_id, period_id, time_seconds)
stopifnot(identical(ho$match_id, S$ho$match_id), identical(ho$time_seconds, S$ho$time_seconds))
Pposs <- function(d, add) {  # possession team's win chance if it scored `add` more goals
  x <- copy(d); x[, m_home := m_home + add * fifelse(is_home == 1, 1, -1)]
  h <- predict(H, as.matrix(x[, ..fh])); fifelse(d$is_home == 1, h, 1 - h)
}
p_m <- Pposs(ho, 0); p_up <- Pposs(ho, 1); p_dn <- Pposs(ho, -1); e <- ho$epv
preds <- c(S$preds, list(home_no_threat = p_m,
                         home_structural = ifelse(e >= 0, (1 - e) * p_m + e * p_up, (1 + e) * p_m - e * p_dn)))
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
saveRDS(list(ho = ho[, .(match_id, team_id, period_id, time_seconds, epv, is_home, kind)], preds = preds),
        file.path(out, "wp_structural_holdout2.rds"))
