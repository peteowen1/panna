# panna#273: the Liverpool 2-0 walk-through rows under the strength-aware arms from wp_strength_arms.R.
# Columns are WPA in the ACTING team's frame (+ good for the player). S0 = scoreboard only,
# SE = + pre-match Elo, SEP = + Elo + home_has_ball; *_ag = same model with elo_diff set to 0 (team-agnostic).
library(data.table); library(xgboost)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
L <- readRDS(file.path(out, "wpa_ledger_liv_d0.rds")); w <- copy(L$w)
models <- readRDS(file.path(out, "wp_strength_arms.rds"))
md <- as.data.table(readRDS("C:/dev/pannaverse/panna/data-raw/cache-predictions-opta/04_match_dataset.rds"))
elo <- unique(md[, .(match_id, elo_diff)], by = "match_id"); rm(md)
w <- elo[w, on = "match_id"]
miss <- w[is.na(elo_diff), unique(match_id)]
message(sprintf("Elo missing for %d of %d ledger matches (%s): dropped from this walk-through", length(miss),
                uniqueN(w$match_id), paste(miss, collapse = ",")))
w <- w[!is.na(elo_diff)]
w[, `:=`(m_home = score_diff, red_home = red_card_diff, home_has_ball = as.integer(is_home == 1))]
base <- c("time_remaining", "m_home", "red_home", "is_second_half", "is_extra_time")
arms <- list(S0 = base, SE = c(base, "elo_diff"), SEP = c(base, "elo_diff", "home_has_ball"))
setorder(w, match_id, period_id, time_seconds, action_id)
wp_of <- function(nm, elo0 = FALSE) {
  f <- arms[[nm]]; m <- models[[nm]]; e <- w$epv
  P <- function(add) { x <- copy(w); x[, m_home := m_home + add * fifelse(is_home == 1, 1, -1)]
    if (elo0) x[, elo_diff := 0]
    h <- predict(m, as.matrix(x[, ..f])); fifelse(w$is_home == 1, h, 1 - h) }
  pm <- P(0); pu <- P(1); pd <- P(-1); ifelse(e >= 0, (1 - e) * pm + e * pu, (1 + e) * pm - e * pd)
}
wpa_of <- function(p) {
  d <- data.table(match_id = w$match_id, team_id = w$team_id, p = p, lab = w$wp_label)
  d[, `:=`(pn = shift(p, type = "lead"), tn = shift(team_id, type = "lead")), by = match_id]
  d[is.na(pn), `:=`(pn = lab, tn = team_id)]
  d[, fifelse(tn == team_id, pn - p, (1 - pn) - p)]
}
for (a in list(c("S0", FALSE), c("SE", FALSE), c("SEP", FALSE), c("SEP", TRUE))) {
  nm <- paste0(a[1], if (a[2] == "TRUE") "_ag" else "")
  p <- wp_of(a[1], a[2] == "TRUE"); w[, (paste0("wp_", nm)) := p]; w[, (paste0("wpa_", nm)) := wpa_of(p)]
}
liv <- "c8h9bw1l82s06h77xxrelzhur"; m <- "4mr2fpdoxrh2o9m6dj4lpopw4"
x <- w[match_id == m]; x[, i := .I]; x[, side := fifelse(team_id == liv, "LIV", "OPP")]
for (nm in c("S0", "SE", "SEP", "SEP_ag")) x[, (paste0("liv_", nm)) := fifelse(side == "LIV", get(paste0("wp_", nm)), 1 - get(paste0("wp_", nm)))]
cat(sprintf("elo_diff (home - away) for this match: %.0f; Liverpool kickoff win chance (draw = 0.5): S0 %.3f, SE %.3f, SEP %.3f, SEP_ag %.3f\n",
            x$elo_diff[1], x$liv_S0[1], x$liv_SE[1], x$liv_SEP[1], x$liv_SEP_ag[1]))
g <- x[is_goal == 1 | (shift(score_diff, type = "lead") != score_diff), i][1]
show <- function(rows, title) { cat("\n==", title, "==\n")
  print(x[rows, .(min = round(time_seconds / 60 + 45 * (period_id - 1), 1), side, player = substr(player_name, 1, 14),
                  action_type, result, score = score_diff, pub = round(wpa, 4), S0 = round(wpa_S0, 4),
                  SE = round(wpa_SE, 4), SEP = round(wpa_SEP, 4), SEP_ag = round(wpa_SEP_ag, 4))]) }
show((g - 7):(g + 2), "build-up to Liverpool's first goal")
z <- x[score_diff == 0 & time_seconds < 900 & side != shift(side, type = "lead"), i]
show((z[3] - 3):(z[3] + 2), "early 0-0 stretch with a turnover")
print(x[, .(pub = mean(abs(wpa)), S0 = mean(abs(wpa_S0)), SE = mean(abs(wpa_SE)), SEP = mean(abs(wpa_SEP)), SEP_ag = mean(abs(wpa_SEP_ag))),
        by = .(turnover = side != shift(side, type = "lead"))])
cat("\nLiverpool player totals this match (sum of WPA; team sums to result minus kickoff chance):\n")
print(x[side == "LIV", lapply(.SD, function(v) round(sum(v), 3)), .SDcols = c("wpa", "wpa_S0", "wpa_SE", "wpa_SEP", "wpa_SEP_ag")])
