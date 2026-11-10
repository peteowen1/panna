# panna#273 prototype 3 (Pete, 2026-11-10): add pre-match team strength to the home-frame model.
# Question: once the model knows how good each team is (pre-match Elo, leak-free), does
# "who has the ball" still carry a win-chance premium? If not, possession was team quality.
# Arms (all home frame, threat as the next-goal mixture, same xgboost settings as wp_structural_home.R):
#   S0  margin, time, red, half, ET                 (reproduces the structural candidate)
#   SP  S0 + home_has_ball                          (possession without strength)
#   SE  S0 + elo_diff                               (strength without possession)
#   SEP S0 + elo_diff + home_has_ball               (both: what possession is worth net of strength)
# Team-agnostic view for the site = predict SE/SEP with elo_diff = 0.
library(data.table); library(xgboost)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
fd <- file.path(out, "feats")
files <- list.files(fd, full.names = TRUE); hold_f <- grep("ENG_2025-2026", files, value = TRUE)

md <- as.data.table(readRDS("C:/dev/pannaverse/panna/data-raw/cache-predictions-opta/04_match_dataset.rds"))
elo <- unique(md[, .(match_id, home_elo, away_elo, elo_diff)], by = "match_id")
cat(sprintf("Elo table: %d matches, elo_diff = home_elo - away_elo holds on %.1f%%\n", nrow(elo),
            100 * elo[, mean(abs(elo_diff - (home_elo - away_elo)) < 1e-6, na.rm = TRUE)]))
rm(md); gc(verbose = FALSE)

keep <- c("match_id", "team_id", "period_id", "time_seconds", "score_diff", "is_home", "red_card_diff",
          "time_remaining", "is_second_half", "is_extra_time", "epv", "wp_label")
rd <- function(f) as.data.table(arrow::read_parquet(f, col_select = dplyr::any_of(keep)))[!is.na(wp_label)]
prep <- function(d) {
  n0 <- uniqueN(d$match_id)
  d <- elo[d, on = "match_id"]
  cov <- d[, mean(!is.na(elo_diff))]
  cat(sprintf("  Elo coverage: %.2f%% of rows (%d matches)\n", 100 * cov, n0))
  stopifnot(cov > 0.98)
  # Frames established in wp_structural_home.R: score_diff and red_card_diff are home frame.
  d[, `:=`(y_home = fifelse(is_home == 1, wp_label, 1 - wp_label), m_home = score_diff,
           red_home = red_card_diff, home_has_ball = as.integer(is_home == 1))]
  d[!is.na(elo_diff)]
}
tr <- rbindlist(lapply(setdiff(files, hold_f), rd)); tr <- prep(tr)
cat(sprintf("train: %d rows, %d matches\n", nrow(tr), uniqueN(tr$match_id)))

base <- c("time_remaining", "m_home", "red_home", "is_second_half", "is_extra_time")
arms <- list(S0 = base, SP = c(base, "home_has_ball"), SE = c(base, "elo_diff"),
             SEP = c(base, "elo_diff", "home_has_ball"))
mono <- function(f) sprintf("(%s)", paste(c(time_remaining = 0, m_home = 1, red_home = 1, is_second_half = 0,
                                             is_extra_time = 0, elo_diff = 1, home_has_ball = 0)[f], collapse = ","))
# One arm at a time, freeing the DMatrix between fits (the 2026-11-10 first run was killed on low memory).
models <- list()
for (nm in names(arms)) {
  f <- arms[[nm]]; t0 <- Sys.time()
  p <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist",
            max_depth = 4, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 50,
            monotone_constraints = mono(f), nthread = 6)
  dm <- xgb.DMatrix(as.matrix(tr[, ..f]), label = tr$y_home)
  set.seed(42); models[[nm]] <- xgb.train(p, dm, nrounds = 285, verbose = 0)
  rm(dm); gc(verbose = FALSE)
  message(sprintf("fitted %s in %.1f min", nm, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
saveRDS(models, file.path(out, "wp_strength_arms.rds")); rm(tr); gc(verbose = FALSE)

ho <- prep(rd(hold_f)); setorder(ho, match_id, period_id, time_seconds)
cat(sprintf("holdout: %d rows, %d matches\n", nrow(ho), uniqueN(ho$match_id)))
Pposs <- function(m, f, d, add, elo0 = FALSE) {
  x <- copy(d); x[, m_home := m_home + add * fifelse(is_home == 1, 1, -1)]
  if (elo0) x[, elo_diff := 0]
  h <- predict(m, as.matrix(x[, ..f])); fifelse(d$is_home == 1, h, 1 - h)
}
structural <- function(nm, elo0 = FALSE) {
  m <- models[[nm]]; f <- arms[[nm]]; e <- ho$epv
  pm <- Pposs(m, f, ho, 0, elo0); pu <- Pposs(m, f, ho, 1, elo0); pd <- Pposs(m, f, ho, -1, elo0)
  ifelse(e >= 0, (1 - e) * pm + e * pu, (1 + e) * pm - e * pd)
}
preds <- list(S0 = structural("S0"), SP = structural("SP"), SE = structural("SE"), SEP = structural("SEP"),
              SE_agnostic = structural("SE", TRUE), SEP_agnostic = structural("SEP", TRUE))

ho[, `:=`(tn = shift(team_id, type = "lead"), sdn = shift(score_diff, type = "lead")), by = match_id]
ho[, same := tn == team_id][, chg := sdn - score_diff]
ho[, kind := fcase(is.na(tn), NA_character_, chg != 0 & sign(chg) == fifelse(is_home == 1, 1, -1), "goal",
                   chg != 0, "goal_against", same, "jitter", default = "turnover")]
ll <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
# Possession premium read straight off the model: home win chance with the ball minus without,
# at 0-0, first 15 minutes, averaged over the holdout's real rows (elo as observed).
premium <- function(nm) {
  f <- arms[[nm]]; if (!"home_has_ball" %in% f) return(NA_real_)
  z <- ho[score_diff == 0 & time_seconds < 900]
  a <- copy(z)[, home_has_ball := 1L]; b <- copy(z)[, home_has_ball := 0L]
  mean(predict(models[[nm]], as.matrix(a[, ..f])) - predict(models[[nm]], as.matrix(b[, ..f])))
}
res <- rbindlist(lapply(names(preds), function(nm) {
  pp <- preds[[nm]]; ho[, p := pp][, pn := shift(p, type = "lead"), by = match_id][, d := fifelse(same, pn - p, (1 - pn) - p)]
  s <- ho[!is.na(kind), .(v = mean(abs(d), na.rm = TRUE)), by = kind]; g <- function(k) s[kind == k, v]
  arm <- sub("_agnostic", "", nm)
  data.table(model = nm, logloss = round(ll(pp, ho$wp_label), 4),
             possession_premium = round(premium(arm), 4),
             goal = round(g("goal"), 3), turnover = round(g("turnover"), 4), jitter = round(g("jitter"), 4),
             goal_per_turnover = round(g("goal") / g("turnover"), 1))
}))
cat("\n=== Held out EPL 2025-26: log loss lower is better (published model 0.5196; *_agnostic log loss is not a\n",
    "   fair score, it deliberately ignores strength); possession_premium = home win chance with the ball minus\n",
    "   without at 0-0 in the first 15 min (0 = possession worth nothing); goal_per_turnover: EPV's is ~35 ===\n")
print(res)
saveRDS(list(ho = ho[, .(match_id, team_id, period_id, time_seconds, epv, is_home, kind, elo_diff)], preds = preds, res = res),
        file.path(out, "wp_strength_holdout.rds"))
