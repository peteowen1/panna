## 99d_continental_league_check.R ------------------------------------------
## DIAGNOSTIC, not a pipeline step (panna#219). Nothing consumes its output and
## run_skills_pipeline.R does not source it.
##
##   PANNA_CONT_DIR=/path/to/data Rscript data-raw/estimated-skills/99d_continental_league_check.R
##
## PANNA_CONT_DIR must hold the public pannadata release assets (download them
## OUTSIDE the repo): game_logs_20*.parquet (blog-latest), opta_fixtures.parquet
## and opta_psr_weekly.parquet (opta-latest).
##
## WHAT IT TESTS. Cross-league UCL/UEL/UECL matches are the one selection-free
## way to price one league against another: two teams from different domestic
## leagues meet, and neither chose the other for reasons tied to league
## mis-pricing. If the league offsets baked into PSR were right, the as-at PSR
## of the fielded players alone would predict goal difference, and the residual
## would carry no information about which two leagues met.
##
##   gd (home - away) ~ psr_diff                       baseline
##   gd ~ psr_diff + off_diff                          do existing offsets need scaling?
##   gd ~ psr_diff + sum_L g_L (1[home=L] - 1[away=L]) per-league residual
##
## psr_diff is the minutes-weighted SUM of fielded players' PSR (goals/match) from
## the last weekly snapshot strictly BEFORE the match -- no leakage. PSR already
## contains the offsets, so g_L is the extra discount (negative = league still
## over-rated, goals per team-match). Dividing by 11 players gives the per-player
## PSR offset change that would close it; the table prints current + that.
##
## "Current" offsets are read back out of the game logs (psv_league_offset per
## 90), i.e. the offsets that build actually applied -- NOT the
## psr_league_offsets.parquet release asset, which can be months stale
## (2026-07-05 when this was written, before the #221 reconnect).
##
## Held-out validation (leave-one-season-out and forward-chained) scores whether
## ridge-shrunk league effects improve goal-difference MSE out of sample.
## Saudi/MLS/Brazil/Argentina play no UEFA fixtures, so this CANNOT test them.

suppressMessages({ library(arrow); library(data.table) })

D <- Sys.getenv("PANNA_CONT_DIR", unset = NA)
if (is.na(D) || !dir.exists(D)) stop("Set PANNA_CONT_DIR to the directory with the release assets.")
dom_codes <- c("ENG","ESP","GER","ITA","FRA","NED","POR","BEL","TUR","SCO")
uefa      <- c("UCL","UEL","UECL")
N_PLAYERS <- 11          # converts team-sum goals <-> per-player PSR offset
RIDGE     <- 200         # league-effect shrinkage, in matches-equivalent (chosen by CV below)
MIN_COV   <- 0.85        # share of fielded minutes that must have an as-at PSR

# 1. Data ----
cols <- c("player_id","match_id","match_date","league","season","team_id","total_minutes","psv_league_offset")
gl <- rbindlist(lapply(list.files(D, "^game_logs_20.*\\.parquet$", full.names = TRUE),
                       function(f) as.data.table(read_parquet(f, col_select = any_of(cols)))), fill = TRUE)
gl[, match_date := as.Date(sub("Z$", "", as.character(match_date)))]
# Offsets per season file. The per-game column is stamped by whichever build wrote
# that file, so different season files can carry different vintages (seen
# 2026-11-02: files through 2025-26 built 2026-10-19 vs 2026-27 built 11-02 off the
# 07-05 offsets asset). We take the NEWEST file's, which matches what the weekly
# PSR workflow loads from psr_league_offsets.parquet, and warn if vintages differ.
cur_by_season <- gl[total_minutes >= 45 & !is.na(psv_league_offset) & league %in% dom_codes,
                    .(offset = median(psv_league_offset / (total_minutes / 90))), by = .(season, league)]
vint <- cur_by_season[, .(sig = paste(round(offset, 3)[order(league)], collapse = ",")), by = season]
if (uniqueN(vint$sig) > 1L) {
  cat("WARNING: game-log files carry", uniqueN(vint$sig), "different offset vintages:\n")
  print(vint[, .(seasons = paste(season, collapse = " ")), by = sig][, sig := NULL][])
}
cur <- cur_by_season[season == max(season)]
ov <- setNames(cur$offset, cur$league)
cat("Offsets taken from the newest game-log season file (per 90):\n"); print(round(ov, 4))
gl <- unique(gl[, .(player_id, match_id, match_date, league, season, team_id, total_minutes)])

dom <- gl[league %in% dom_codes, .(m = sum(total_minutes, na.rm = TRUE)), by = .(team_id, season, league)]
setorder(dom, team_id, season, -m)
dom <- dom[, .SD[1], by = .(team_id, season)][, .(team_id, season, dom = league)]
ue <- merge(gl[league %in% uefa], dom, by = c("team_id", "season"))   # teams with a modelled domestic league
fx <- as.data.table(read_parquet(file.path(D, "opta_fixtures.parquet")))[
  , .(match_id, home_team_id, away_team_id, home_score = as.numeric(home_score), away_score = as.numeric(away_score))]
ue <- merge(ue, fx, by = "match_id")
ue[, side := fifelse(team_id == home_team_id, "h", fifelse(team_id == away_team_id, "a", NA_character_))]
ue <- ue[!is.na(side) & !is.na(home_score)]

ps <- as.data.table(open_dataset(file.path(D, "opta_psr_weekly.parquet")) |>
  dplyr::filter(player_id %in% unique(ue$player_id)) |>
  dplyr::select(snapshot_date, player_id, psr) |> dplyr::collect())
setorder(ps, player_id, snapshot_date)
ue[, qd := match_date - 1L]; ps[, snap := snapshot_date]
ue <- ps[, .(player_id, snap, psr)][ue, on = .(player_id, snap <= qd), mult = "last"]
ue[, w := total_minutes / 90]
tm <- ue[, .(psr_sum = sum(psr * w, na.rm = TRUE), cov = sum(w[!is.na(psr)]) / sum(w), mins = sum(w),
             dom = dom[1], hs = home_score[1], as = away_score[1], match_date = match_date[1]),
         by = .(match_id, side)]
m <- merge(tm[side == "h"], tm[side == "a"], by = "match_id", suffixes = c("_h", "_a"))
m <- m[cov_h >= MIN_COV & cov_a >= MIN_COV & mins_h > 8 & mins_a > 8 & dom_h != dom_a]
m[, `:=`(gd = hs_h - as_h, psr_diff = psr_sum_h - psr_sum_a,
         off_diff = N_PLAYERS * (ov[dom_h] - ov[dom_a]),
         sy = as.integer(format(match_date_h, "%Y")) + (as.integer(format(match_date_h, "%m")) >= 7L))]
cat(sprintf("\nCross-league matches: %d (%d team-matches), %d league pairs\n", nrow(m), 2 * nrow(m),
            uniqueN(paste(pmin(m$dom_h, m$dom_a), pmax(m$dom_h, m$dom_a)))))

# 2. Pooled test: do the existing offsets need scaling? ----
cat("\n-- gd ~ psr_diff --\n");            print(summary(lm(gd ~ psr_diff, m))$coef)
cat("\n-- gd ~ psr_diff + off_diff (positive off_diff = offsets too small) --\n")
print(summary(lm(gd ~ psr_diff + off_diff, m))$coef)

# 3. Per-league residual and joint ridge effects ----
fit0 <- lm(gd ~ psr_diff, m); m[, e := resid(fit0)]
res <- rbind(m[, .(lg = dom_h, e = e)], m[, .(lg = dom_a, e = -e)])[
  , .(n = .N, resid = mean(e), se = sd(e) / sqrt(.N)), by = lg][order(resid)]
res[, `:=`(t = round(resid / se, 2), cur_offset = ov[lg])]
cat("\n-- Mean goals a league's teams beat their PSR by (negative = over-rated) --\n"); print(res)

lv <- sort(unique(c(m$dom_h, m$dom_a)))
X  <- sapply(lv, function(L) as.numeric(m$dom_h == L) - as.numeric(m$dom_a == L)); colnames(X) <- lv
ridge_fit <- function(tr, lambda) {   # unpenalised intercept + psr slope; ridge on league effects
  Z <- cbind(1, m$psr_diff[tr], X[tr, ])
  drop(solve(crossprod(Z) + diag(c(0, 0, rep(lambda, length(lv)))), crossprod(Z, m$gd[tr])))
}
ridge_pred <- function(b, te) drop(cbind(1, m$psr_diff[te], X[te, ]) %*% b)
big5 <- c("ENG","ESP","GER","ITA","FRA")
eff <- function(lambda, idx = seq_len(nrow(m))) { g <- ridge_fit(idx, lambda)[-(1:2)]; g - mean(g[big5]) }
suggest <- data.table(league = lv, free_goals = round(eff(1e-3)[lv], 3), ridge_goals = round(eff(RIDGE)[lv], 3),
                      cur_offset = round(ov[lv], 4))
suggest[, `:=`(per_player_free = round(free_goals / N_PLAYERS, 4), per_player_ridge = round(ridge_goals / N_PLAYERS, 4))]
suggest[, suggested_offset := round(cur_offset + per_player_ridge, 4)]
cat(sprintf("\n-- League effects (goals/team-match, Big-5 mean = 0; ridge = %g) and implied offsets --\n", RIDGE))
print(suggest[order(ridge_goals)])
cat("\nF test, free league dummies vs none:\n")
d <- data.frame(gd = m$gd, psr = m$psr_diff, X[, -1]); print(anova(lm(gd ~ psr, d), lm(gd ~ ., d)))

# 4. Held-out validation ----
lams <- c(1e-3, 50, RIDGE, 800, 1e6)       # 1e6 == no league term (baseline)
run <- function(scheme) {
  out <- lapply(sort(unique(m$sy)), function(s) {
    te <- which(m$sy == s); tr <- if (scheme == "loso") which(m$sy != s) else which(m$sy < s)
    if (length(tr) < 400) return(NULL)
    c(n = length(te), setNames(sapply(lams, function(l) sum((m$gd[te] - ridge_pred(ridge_fit(tr, l), te))^2)),
                               paste0("ridge", lams)),
      offdiff = { b <- coef(lm(gd ~ psr_diff + off_diff, m[tr])); sum((m$gd[te] - (b[1] + b[2] * m$psr_diff[te] + b[3] * m$off_diff[te]))^2) })
  })
  tot <- colSums(do.call(rbind, out)); round(tot[-1] / tot["n"], 4)
}
cat("\nLeave-one-season-out MSE of goal difference (ridge1e+06 = baseline; offdiff = scaled existing offsets):\n"); print(run("loso"))
cat("Forward-chained MSE (train on earlier seasons only):\n"); print(run("fwd"))
