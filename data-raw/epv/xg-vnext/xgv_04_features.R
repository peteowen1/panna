# xG next round, step 3b + 4: one feature table for every shot, and the table
# Pete reads before anything is fitted.
# =============================================================================
# Joins the shot table, today's xG features (prepare_shots_for_xg) and the event
# context (xgv_03), then adds the leak-free inputs:
#   bc_rate_prev    league big-chance tagging rate from EARLIER seasons only,
#                   each older season weighted DECAY_S times the next, shrunk
#                   toward the all-league rate of those same earlier seasons
#   bc_rate_asof    the same from EARLIER DATES only (same-day matches excluded),
#                   exponential decay with half-life HALF_DAYS, same shrinkage
#   foot_share      share of the shooter's EARLIER foot shots taken with this
#                   foot (NA under MIN_FOOT earlier foot shots or for headers)
# Leak test: recompute both rates with every shot after CUT removed; every
# shot before CUT must get the identical value.
#
# Output: data-raw/cache/epv/xg-vnext/shot_features.parquet (one row per
# non-own-goal shot) and feature_table.csv (coverage, goal rate, goals per
# published xG by feature value; that last column says whether a feature
# carries information today's model does not).
#
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_04_features.R
suppressMessages({library(data.table); library(arrow); library(dplyr); devtools::load_all(quiet = TRUE)})
OD  <- "C:/dev/pannaverse/pannadata/data/opta/"
X   <- "data-raw/cache/epv/xg-vnext"
DECAY_S <- 0.5; HALF_DAYS <- 365; MIN_FOOT <- 10; CUT <- as.Date("2022-01-01")
say <- function(...) cat("\n", ..., "\n", sep = "")

sh <- as.data.table(collect(open_dataset(paste0(OD, "opta_shot_events.parquet"))))
fx <- unique(as.data.table(collect(open_dataset(paste0(OD, "opta_fixtures.parquet")) |> select(match_id, match_date))), by = "match_id")
sh <- merge(sh, fx, by = "match_id", all.x = TRUE)
sh[, match_date := as.Date(match_date)]
ctx <- rbindlist(lapply(list.files(file.path(X, "context"), "^ctx_.*[.]parquet$", full.names = TRUE), read_parquet), fill = TRUE)
ctx[, event_id := as.character(event_id)]
sh[, event_id := as.character(event_id)]
ctx <- unique(ctx, by = c("match_id", "event_id"))
say("shots ", nrow(sh), " | with context ", sum(paste(sh$match_id, sh$event_id) %chin% paste(ctx$match_id, ctx$event_id)),
    " | with a date ", sum(!is.na(sh$match_date)))

ft <- as.data.table(prepare_shots_for_xg(as.data.frame(sh)))           # drops own goals
ft[, event_id := as.character(event_id)]
ft <- merge(ft, sh[, .(match_id, event_id, competition, season, match_date, type_id, big_chance)],
            by = c("match_id", "event_id"), all.x = TRUE)
keep_ctx <- setdiff(names(ctx), c("type_id", "team_id", "player_id", "player_name", "x", "y", "competition", "season"))
ft <- merge(ft, ctx[, ..keep_ctx], by = c("match_id", "event_id"), all.x = TRUE)
ft[, goal := is_goal]

# ---- leak-free league big-chance rates ----------------------------------------
np <- ft[is_penalty == 0 & !is.na(match_date)]
eb_shrink <- function(tag, n) {                                   # beta-binomial, method of moments
  p0 <- sum(tag) / sum(n); r <- tag / n
  v_obs <- sum(n * (r - p0)^2) / sum(n); v_bin <- p0 * (1 - p0) * length(n) / sum(n)
  k <- if (v_obs > v_bin) p0 * (1 - p0) / (v_obs - v_bin) - 1 else 1e6
  list(p0 = p0, k = k)
}
bc_prev <- function(d) {                                          # by earlier seasons
  ls <- d[, .(tag = sum(big_chance %in% c(TRUE, 1)), n = .N), by = .(competition, yr = season_num)]
  out <- CJ(competition = unique(ls$competition), yr = sort(unique(ls$yr)))
  out[, c("t_w", "n_w", "p0", "k") := NA_real_]
  for (y in sort(unique(ls$yr))) {
    past <- ls[yr < y]; if (!nrow(past)) next
    past[, w := DECAY_S^(y - yr - 1)]
    agg <- past[, .(t_w = sum(w * tag), n_w = sum(w * n)), by = competition]
    pr <- eb_shrink(agg$t_w, agg$n_w)
    out[yr == y, c("t_w", "n_w") := agg[.SD, on = "competition", .(t_w, n_w)]]
    out[yr == y, `:=`(p0 = pr$p0, k = pr$k)]
  }
  out[, bc_rate_prev := fifelse(is.na(t_w), p0, (fcoalesce(t_w, 0) + k * p0) / (fcoalesce(n_w, 0) + k))]
  out[, .(competition, season_num = yr, bc_rate_prev)]
}
decayed_before <- function(dates, x, lam) {                      # sum of x over EARLIER dates, decayed
  out <- numeric(length(x)); acc <- 0
  for (i in seq_along(x)) {
    if (i > 1) acc <- (acc + x[i - 1]) * exp(-lam * as.numeric(dates[i] - dates[i - 1]))
    out[i] <- acc
  }
  out
}
bc_asof <- function(d, K) {                                       # by earlier dates; K fixed from training seasons
  lam <- log(2) / HALF_DAYS
  dd <- d[, .(tag = sum(big_chance %in% c(TRUE, 1)), n = .N), by = .(competition, match_date)]
  setorder(dd, competition, match_date)
  dd[, `:=`(t_w = decayed_before(match_date, tag, lam), n_w = decayed_before(match_date, n, lam)), by = competition]
  al <- d[, .(tag = sum(big_chance %in% c(TRUE, 1)), n = .N), by = match_date][order(match_date)]
  al[, `:=`(pt = decayed_before(match_date, tag, lam), pn = decayed_before(match_date, n, lam))]
  dd[al, on = "match_date", p0 := fifelse(i.pn > 0, i.pt / i.pn, NA_real_)]
  dd[, bc_rate_asof := (t_w + K * p0) / (n_w + K)]
  dd[, .(competition, match_date, bc_rate_asof)]
}
tr <- np[season_num <= 2024, .(tag = sum(big_chance %in% c(TRUE, 1)), n = .N), by = competition]
K_ASOF <- eb_shrink(tr$tag, tr$n)$k
say("as-of shrinkage strength K (fitted on seasons to 2024): ", round(K_ASOF), " shots")
bp <- bc_prev(np); ba <- bc_asof(np, K_ASOF)
ft[bp, on = .(competition, season_num), bc_rate_prev := i.bc_rate_prev]
ft[ba, on = .(competition, match_date), bc_rate_asof := i.bc_rate_asof]

# leak test: truncate at CUT, recompute, compare every shot before CUT
bp2 <- bc_prev(np[season_num <= as.integer(format(CUT, "%Y"))]); ba2 <- bc_asof(np[match_date < CUT], K_ASOF)
chk_p <- merge(bp[season_num <= as.integer(format(CUT, "%Y"))], bp2, by = c("competition", "season_num"))
chk_a <- merge(ba[match_date < CUT], ba2, by = c("competition", "match_date"))
d_p <- max(abs(chk_p$bc_rate_prev.x - chk_p$bc_rate_prev.y), na.rm = TRUE)
d_a <- max(abs(chk_a$bc_rate_asof.x - chk_a$bc_rate_asof.y), na.rm = TRUE)
say("LEAK TEST (cut ", format(CUT), "): earlier-seasons rate max change ", signif(d_p, 3), " over ", nrow(chk_p),
    " league-seasons; earlier-dates rate max change ", signif(d_a, 3), " over ", nrow(chk_a), " league-dates")
stopifnot(d_p < 1e-12, d_a < 1e-12, nrow(chk_p) > 0, nrow(chk_a) > 0)

# ---- weak foot from the shooter's earlier shots --------------------------------
fs <- ft[is_header == 0 & (is_right_foot == 1 | is_left_foot == 1) & !is.na(match_date) & !is.na(player_id),
         .(player_id, match_date, right = is_right_foot)]
fd <- fs[, .(r = sum(right), n = .N), by = .(player_id, match_date)][order(player_id, match_date)]
fd[, `:=`(r_prev = cumsum(r) - r, n_prev = cumsum(n) - n), by = player_id]
ft[fd, on = .(player_id, match_date), `:=`(r_prev = i.r_prev, n_prev = i.n_prev)]
ft[, foot_share := fifelse(is_header == 0 & fcoalesce(as.numeric(n_prev), 0) >= MIN_FOOT,
                           fifelse(is_right_foot == 1, r_prev / n_prev, 1 - r_prev / n_prev), NA_real_)]
ft[, c("r_prev", "n_prev") := NULL]

# ---- published xG, as the yardstick ---------------------------------------------
pub <- readRDS("C:/dev/_model-backups/2026-09-23/pannamodels-epv/xg_model.rds")
ok <- ft$is_penalty == 0 & !is.na(ft$season_num)
ft[, xg_pub := NA_real_]
ft[ok, xg_pub := predict_xg(pub, as.data.frame(.SD))]
write_parquet(ft, file.path(X, "shot_features.parquet"))
say("wrote shot_features.parquet: ", nrow(ft), " shots, ", ncol(ft), " columns")

# ---- the table for Pete ------------------------------------------------------------
d <- ft[ok & period_id %in% 1:4]
row <- function(v, lab, grp) d[, .(feature = lab, value = as.character(grp), shots = .N, share = .N / nrow(d),
                                    goal_rate = mean(goal), goals_per_pub_xg = sum(goal) / sum(xg_pub)), by = .(grp = grp)][, grp := NULL][]
tab <- rbind(
  row(NULL, "fast break (23)", d$q23), row(NULL, "1 on 1 (89)", d$q89), row(NULL, "individual play (215)", d$q215),
  row(NULL, "intentional assist (154)", d$q154), row(NULL, "2nd assist (217)", d$q217), row(NULL, "lob (117)", d$q117),
  row(NULL, "tag 328 (2017+)", d$q328), row(NULL, "tag 388 (2020+)", d$q388), row(NULL, "tag 458 (2021+)", d$q458), row(NULL, "tag 468 (2022+)", d$q468),
  row(NULL, "rebound", d$rebound),
  row(NULL, "assist: cross", d$a2), row(NULL, "assist: through ball", d$a4), row(NULL, "assist: pull back", d$a195),
  row(NULL, "assist: chipped", d$a155), row(NULL, "assist: lay-off", d$a156), row(NULL, "assist: flick-on", d$a168),
  row(NULL, "assist length (m)", cut(d$a_len, c(0, 10, 20, 30, 100))),
  row(NULL, "seconds since other team had the ball", cut(d$poss_secs, c(-1, 5, 10, 20, 40, 5000))),
  row(NULL, "passes in the possession", cut(d$poss_passes, c(-1, 0, 2, 5, 10, 200))),
  row(NULL, "score difference", pmax(pmin(d$score_diff, 2), -2)),
  row(NULL, "minute", cut(d$minute, c(-1, 15, 30, 45, 60, 75, 90, 200))),
  row(NULL, "share of earlier foot shots with this foot", cut(d$foot_share, c(-0.01, 0.2, 0.4, 0.6, 0.8, 1))),
  row(NULL, "league big-chance rate, earlier seasons", cut(d$bc_rate_prev, c(0, 0.11, 0.13, 0.15, 0.17, 1))),
  row(NULL, "league big-chance rate, earlier dates", cut(d$bc_rate_asof, c(0, 0.11, 0.13, 0.15, 0.17, 1))))
tab[, `:=`(share = round(share, 4), goal_rate = round(goal_rate, 4), goals_per_pub_xg = round(goals_per_pub_xg, 3))]
fwrite(tab, file.path(X, "feature_table.csv"))
say("FEATURE TABLE (non-penalty shots, periods 1-4, n = ", nrow(d), "). goals_per_pub_xg: 1.00 = today's xG already prices it; ",
    "above 1 = these shots score more than today's xG says.")
print(tab, nrows = 300)
