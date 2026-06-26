# Generate per-(league, role) live-PSV centering constants K[league,role] for the
# blog's live in-match scorer (inthegame-blog/football/stat-value.js).
#
# THEORY (see session notes): exact game-logs PSV (per-90, pre-minutes) is
#   exactPSV = raw_full(x) - B_role[role] - C_pop[league, pop]
# so the single constant live must subtract is
#   K[league,role] = raw_full(x) - exactPSV    ( = B_role + C_pop )
# and x cancels => K is CONSTANT within every (league, role). We compute it
# empirically (drift-proof: consistent with whatever 10b ships) and self-check
# that the within-group SD ~ 0. Missing-feature terms (xMetrics/duel WOE) live
# inside both raw_full and exactPSV so they cancel — K is clean regardless.
#
# Cold-start: current-season K is shrunk toward a prior:
#   prev-season same-league K  ->  cross-league __default__  (n/(n+k) weight)
# so the first game of a season / a brand-new league regresses to the prior.

suppressMessages({ library(data.table) })
devtools::load_all(".", quiet = TRUE)

SHRINK_K <- 80L   # player-games of prior weight (n/(n+k) shrinkage)
pm <- load_position_role_means()
ms <- as.data.table(readRDS("data-raw/cache-skills/01_match_stats.rds"))

# Keep the two most recent end-years (current + prior-as-prior), blog-ish leagues
sey_map <- vapply(unique(ms$season), extract_season_end_year, numeric(1))
ms[, sey := sey_map[season]]
recent <- head(sort(unique(ms$sey), decreasing = TRUE), 2)  # head() not [1:2]: no NA pad if only 1 season
if (length(recent) < 1L) stop("no parseable season end-years in 01_match_stats.rds")
ms <- ms[sey %in% recent]
cat(sprintf("rows: %d  seasons(end_year): %s  leagues: %d\n",
            nrow(ms), paste(recent, collapse=","), uniqueN(ms$league)))

# broad role per player-game (same classifier position-norm uses)
ms[, role := .player_role(ms)]

score_one <- function(d, center, position_means) {
  compute_player_psv(d, min_adjust = FALSE, center = center,
                     scale_to_minutes = FALSE, exclude_efficiency = FALSE,
                     target = "blend", position_means = position_means)
}

# Compute K per (league, season): raw_full (no norm, no center) minus exactPSV
# (norm + per-league/per-population center). Score each league-season block on
# its OWN population so centering matches 10b exactly.
res <- list()
for (s in recent) {
  for (lg in sort(unique(ms[sey == s, league]))) {
    blk <- ms[sey == s & league == lg]
    if (nrow(blk) < 20L) next
    # fail_if_missing_frac = 0.6 (matches 02/07/07b): STOP if a league is xG-blind
    # rather than silently building K without the finishing/duel over-perf terms.
    blk <- enrich_match_stats_with_xmetrics(blk, fail_if_missing_frac = 0.6, verbose = FALSE)
    ex <- as.data.table(score_one(blk, center = TRUE,  position_means = pm))
    rw <- as.data.table(score_one(blk, center = FALSE, position_means = NULL))
    key <- c("match_id", "player_id")
    m <- merge(ex[, c(key, "psv","osv","dsv"), with=FALSE],
               rw[, c(key, "psv","osv","dsv"), with=FALSE],
               by = key, suffixes = c("_ex","_rw"))
    role_lookup <- unique(blk[, c(key, "role"), with=FALSE], by = key)
    m <- merge(m, role_lookup, by = key)
    m[, `:=`(K_psr = psv_rw - psv_ex, K_osr = osv_rw - osv_ex, K_dsr = dsv_rw - dsv_ex)]
    g <- m[, .(psr = mean(K_psr), osr = mean(K_osr), dsr = mean(K_dsr),
               sd_psr = sd(K_psr), sd_osr = sd(K_osr), sd_dsr = sd(K_dsr), n = .N), by = role]
    g[, `:=`(league = lg, sey = s)]
    res[[paste(s, lg)]] <- g
  }
}
K <- rbindlist(res)

# ---- THEORY CHECK (HARD): K must be constant within (league,role) ----
# This invariant is the whole justification for shipping ONE constant per group.
# Abort rather than ship a wrong constant if it ever breaks — e.g. a future
# round/matchweek column splitting calculate_psv's centering into subgroups, or
# GK/role-taxonomy drift contaminating a bucket. Checks all three components
# (K_osr/K_dsr can drift while K_psr stays flat).
SD_TOL <- 1e-6
max_sd <- max(c(K$sd_psr, K$sd_osr, K$sd_dsr), na.rm = TRUE)
cat(sprintf("\n[check] max within-group SD of K (psr/osr/dsr): %.3e (tol %.0e)\n", max_sd, SD_TOL))
print(head(K[order(-sd_psr), .(league, sey, role, n, sd_psr, sd_osr, sd_dsr)], 5))
if (is.finite(max_sd) && max_sd > SD_TOL)
  stop(sprintf(paste("K is NOT constant within (league,role): max SD %.3e > %.0e.",
                     "The centering-constant assumption is broken (round-split centering?",
                     "GK/role taxonomy drift?). Aborting rather than shipping a wrong constant."),
               max_sd, SD_TOL))

# ---- How much does C_pop vary across leagues? (does cold-start matter?) ----
cur <- max(recent)
cat(sprintf("\n[spread] cross-league K_psr by role, current season (end %d):\n", cur))
print(K[sey == cur, .(mean = round(mean(psr),4), sd = round(sd(psr),4),
                      min = round(min(psr),4), max = round(max(psr),4), nlg = .N), by = role])

# ---- Shrink toward a prior, KEEPING prior-only leagues -------------------------
# Prior hierarchy: current-season league K (weight n/(n+SHRINK_K)) blended with
# prev-season same-league K, else the cross-league __default__. A league with NO
# current-season block (n=0: season not started yet, or relegated/one-off comp)
# is KEPT with n=0 -> 100% its OWN prev-season prior, rather than dropped to the
# generic default (that drop was the cold-start regression this is designed to do
# right). __default__ is the n-WEIGHTED cross-league mean so a thin 20-game league
# can't skew the offset every unseen league inherits.
cur_dt <- K[sey == cur,         .(league, role, c_psr = psr, c_osr = osr, c_dsr = dsr, n)]
prev   <- K[sey == min(recent), .(league, role, p_psr = psr, p_osr = osr, p_dsr = dsr)]
defaults <- K[sey == cur, .(psr = weighted.mean(psr, n), osr = weighted.mean(osr, n),
                            dsr = weighted.mean(dsr, n)), by = role]  # __default__

# universe = every (league, role) seen in EITHER season (keeps prior-only leagues)
universe <- unique(rbind(cur_dt[, .(league, role)], prev[, .(league, role)]))
g <- merge(universe, cur_dt, by = c("league","role"), all.x = TRUE)
g <- merge(g, prev,     by = c("league","role"), all.x = TRUE)
g <- merge(g, defaults[, .(role, d_psr = psr, d_osr = osr, d_dsr = dsr)], by = "role", all.x = TRUE)
g[is.na(n), n := 0L]
for (cc in c("c_psr","c_osr","c_dsr")) g[is.na(get(cc)), (cc) := 0]   # n=0 -> weight 0, value unused
# prior = prev-season league K if present, else cross-league default
g[, `:=`(pr_psr = fifelse(is.na(p_psr), d_psr, p_psr),
         pr_osr = fifelse(is.na(p_osr), d_osr, p_osr),
         pr_dsr = fifelse(is.na(p_dsr), d_dsr, p_dsr))]
w <- function(n) n / (n + SHRINK_K)
g[, `:=`(psr = w(n)*c_psr + (1-w(n))*pr_psr,
         osr = w(n)*c_osr + (1-w(n))*pr_osr,
         dsr = w(n)*c_dsr + (1-w(n))*pr_dsr)]

out <- rbind(
  g[, .(league, role, psr, osr, dsr, n)],
  defaults[, .(league = "__default__", role, psr, osr, dsr, n = 0L)]
)
setorder(out, league, role)
fwrite(out, "inst/extdata/psv_live_constants.csv")
cat(sprintf("\nwrote inst/extdata/psv_live_constants.csv: %d rows (%d leagues + default)\n",
            nrow(out), uniqueN(out$league) - 1L))
print(out[league %in% c("ENG","__default__")])
