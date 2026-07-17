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

# Tournament leagues fetched inline from box scores when the skills cache lacks
# their current-season rows (the skills pipeline lags the tournament — the same
# gap 10b's pannadata#74 fallback closes for game-logs). Same transform as 10b:
# load_opta_stats -> compute_match_level_opta_stats -> enrich.
INLINE_LEAGUES <- c("WC")

# panna#123: leagues whose K is built on the LIVE-OBSERVABLE feature subset.
# The blog's live scorer builds its vector from the Opta matchstats block +
# event-derived counts (stat-value.js eventFeaturesFromRows); for the WC that
# block is thin (~27 stat types vs ~90 club), so the live raw score is missing
# the unobservable features' contribution. A full-vector K would over-subtract
# by the population mean of exactly that contribution (every player negative —
# the Kane −0.13 bug). Building K as mean(raw_OBSERVABLE − exactPSV) absorbs it.
# NB for these leagues K is NOT constant within (league, role) — the within-
# group SD is the irreducible live-estimate noise, reported not enforced.
LIVE_SUBSET_LEAGUES <- c("WC")

# The live-observable panna features, transcribed from inthegame-blog
# football/stat-value.js (PER90 map ∩ cols eventFeaturesFromRows derives, plus
# shots_on_target_p90 + goals_conceded_p90 which even the thin block carries —
# ontargetScoringAtt is deliberately NOT event-derived, see stat-value.js).
# LOCKSTEP: if the blog's event-derived builder gains/loses a feature, update
# this list and regenerate (inthegame-blog#378 — "constants must match whatever
# vector this ships"). Everything else in the coefficient files — through
# balls, blocks (incl. blocked_passes: "No reliable event type derives
# blockedPass"), errors, niche GK counts, and ALL xMetrics/WOE model outputs —
# scores as 0 live.
# panna#<blog-xmetrics-live>: 5 of the 12 xMetrics coefficient features became
# live-observable when the blog's live scorer started deriving them itself
# from the worker's live-scored per-shot xG (inthegame-blog
# football/stat-value.js). The other 7 xMetrics features (+ xg_per90/
# npxg_per90, which the blog does NOT derive this way) remain unobservable —
# see the enrichment block below, which keeps these 5 real and drops the rest.
LIVE_XMETRICS_FEATURES <- c(
  "npg_minus_npxg_per90", "ibox_g_minus_xg_per90", "obox_g_minus_xg_per90",
  "xa_per90_xmetrics", "gsaa_per90"
)
LIVE_OBSERVABLE_FEATURES <- c(
  # shooting
  "goals_p90", "shots_p90", "shots_on_target_p90", "shots_ibox_p90",
  "shots_obox_p90", "big_chance_scored_p90", "big_chance_missed_p90",
  "att_openplay_p90", "att_setpiece_p90", "att_fastbreak_p90",
  "offtarget_att_assist_p90",
  # creation / passing
  "assists_p90", "big_chance_created_p90", "total_att_assist_p90",
  "key_passes_p90", "passes_p90", "passes_accurate_p90",
  "final_third_passes_p90", "long_balls_p90", "crosses_p90",
  "crosses_open_play_p90", "corners_taken_p90", "forward_pass_p90",
  "fwd_zone_pass_p90", "open_play_pass_p90", "pen_area_entries_p90",
  "final_third_entries_p90",
  # defending / duels
  "tackles_p90", "tackles_won_p90", "interceptions_p90",
  "interceptions_won_p90", "clearances_p90", "clearances_effective_p90",
  "duel_won_p90", "duel_lost_p90",
  "aerial_won_p90", "aerial_lost_p90",
  "poss_won_def3rd_p90", "poss_won_mid3rd_p90", "poss_won_att3rd_p90",
  "ball_recovery_p90",
  # possession / discipline
  "touches_p90", "touches_opp_box_p90", "dispossessed_p90",
  "unsuccessful_touch_p90", "fouls_p90", "was_fouled_p90",
  # GK (thin block + event-derived: saves / punches / high claims)
  "saves_p90", "punches_p90", "high_claim_p90", "good_high_claim_p90",
  "goals_conceded_p90",
  # xMetrics (client-side aggregated from live-scored per-shot xG — see above)
  LIVE_XMETRICS_FEATURES
)

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

# Inline current-season box stats for tournament leagues the skills cache
# lacks (10b's pannadata#74 pattern). Best-effort per league: a tournament
# outside its window (resolve_league_season -> NULL) or a fetch failure skips
# that league without killing the club constants.
cur_year <- max(recent)
cur_domestic <- sprintf("%d-%d", cur_year - 1L, cur_year)
inline_failures <- character(0)   # feeds the upload gate at the bottom
for (lg in INLINE_LEAGUES) {
  tryCatch({
    lg_season <- resolve_league_season(lg, cur_domestic, tournament_leagues = lg)
    if (is.null(lg_season)) {
      cat(sprintf("[inline] %s: no tournament for end-year %d — skipped\n", lg, cur_year))
    } else {
      box <- load_opta_stats(lg, season = lg_season)
      if (!is.null(box) && nrow(box) > 0) {
        box_dt <- as.data.table(box)
        # 10b's per-match coverage gate + dedupe (10b:474-489): fetch ONLY the
        # matches the skills cache doesn't already cover. A fixed row-count gate
        # has two failure modes this avoids: a stale partial cache (>=N early
        # rows suppress the fetch and K is built on matchday-1 data), and
        # duplicate (match_id, player_id) keys when cached matches are appended
        # again (the downstream K merge then double-counts or errors).
        cached_ids <- unique(ms[league == lg & sey == cur_year, match_id])
        box_dt <- box_dt[!match_id %in% cached_ids]
        if (nrow(box_dt) > 0L) {
          box_dt[, league := lg]
          box_dt[, season := lg_season]
          inline_ms <- as.data.table(compute_match_level_opta_stats(box_dt, min_minutes = 10))
          inline_ms[, sey := cur_year]
          ms <- rbindlist(list(ms, inline_ms), fill = TRUE, use.names = TRUE)
          cat(sprintf("[inline] %s %s: +%d player-games from %d matches beyond the cache\n",
                      lg, lg_season, nrow(inline_ms), uniqueN(inline_ms$match_id)))
        } else {
          cat(sprintf("[inline] %s %s: skills cache already covers all matches\n",
                      lg, lg_season))
        }
      } else {
        cat(sprintf("[inline] %s %s: no box scores available — skipped\n", lg, lg_season))
      }
    }
  }, error = function(e) {
    cat(sprintf("[inline] %s FAILED: %s — skipped\n", lg, conditionMessage(e)))
    inline_failures <<- c(inline_failures, lg)
  })
}

# panna#<blog-xmetrics-live>: enrich with REAL per-match xMetrics ONCE up front
# (not per league-season, for efficiency) so both the `ex` and `rw` sides of
# the K computation below see real values for LIVE_XMETRICS_FEATURES (the 5
# the blog can now derive live) -- covers the inline-fetched rows above too.
#
# Every OTHER column enrich_match_stats_with_xmetrics() adds (xg_per90,
# npxg_per90, and the 7 xMetrics features still NOT live: placement_added_per90,
# xpass_overperformance_per90_xmetrics, aerial_woe_per90, aerial_poss_woe_per90,
# takeon_woe_per90, tackle_poss_woe_per90, containment_woe_per90) is DROPPED
# entirely rather than zeroed. This matters: position_role_means.csv (pm) DOES
# carry per-role means for most of these (07b enriches fully before averaging),
# so leaving them PRESENT-but-zero would make .position_normalize_skills
# subtract that role mean from the literal 0 on the `ex` (exact, position_means
# = pm) side ONLY -- injecting a role-specific, non-cancelling offset that `rw`
# (position_means = NULL, never normalized) does not share. Dropping the column
# instead excludes it from both the coefficient dot-product AND position
# normalization on BOTH sides, reproducing the original "fully unenriched"
# convention validated by debug/k_enrich_bias_check.R, now scoped to just the
# still-not-live subset instead of all 12.
.ms_cols_before <- names(ms)
ms <- enrich_match_stats_with_xmetrics(ms, verbose = TRUE, source = "remote")
.xm_new_cols  <- setdiff(names(ms), .ms_cols_before)
.xm_drop_cols <- setdiff(.xm_new_cols, LIVE_XMETRICS_FEATURES)
if (length(.xm_drop_cols) > 0L) ms[, (.xm_drop_cols) := NULL]
cat(sprintf(
  "[xmetrics] enriched ms with %d col(s): %s\n  keeping live-observable: %s\n  dropping (still not live): %s\n",
  length(.xm_new_cols), paste(.xm_new_cols, collapse = ", "),
  paste(intersect(.xm_new_cols, LIVE_XMETRICS_FEATURES), collapse = ", "),
  paste(.xm_drop_cols, collapse = ", ")))

# broad role per player-game (same classifier position-norm uses)
ms[, role := .player_role(ms)]

score_one <- function(d, center, position_means) {
  compute_player_psv(d, min_adjust = FALSE, center = center,
                     scale_to_minutes = FALSE, exclude_efficiency = FALSE,
                     target = "blend", position_means = position_means)
}

# Compute K per (league, season): raw (no norm, no center) minus exactPSV
# (norm + per-league/per-population center). Score each league-season block on
# its OWN population so centering matches 10b exactly. For LIVE_SUBSET_LEAGUES
# the raw side zeroes every coefficient feature the live scorer can't build, so
# K = mean(raw_observable − exact) absorbs the unobservable features' mean
# contribution (see the panna#123 note above).
# HARD-FAIL on any coefficient load: a swallowed error here would leave
# all_coef_stats empty/partial, make .zero_unobservable a silent no-op, and ship
# a full-vector K labeled live_subset=TRUE — the exact Kane over-subtraction bug
# this script exists to fix, with a green run. (Scoring needs these same CSVs,
# so aborting early loses nothing.)
all_coef_stats <- unique(unlist(lapply(
  list(c("margin","blend","outfield"), c("offense","blend","outfield"),
       c("defense","blend","outfield"), c("margin","goals","gk"),
       c("offense","goals","gk"), c("defense","goals","gk")),
  function(a) load_psr_coefficients(a[1], target = a[2], model = a[3])$stat_name
)))
if (length(all_coef_stats) < 50L)
  stop(sprintf(paste("coefficient stat union suspiciously small (%d stats, expected ~70+)",
                     "— refusing to build the live-subset K from a partial list"),
               length(all_coef_stats)))
.zero_unobservable <- function(blk) {
  out <- copy(blk)
  zero_cols <- setdiff(intersect(all_coef_stats, names(out)), LIVE_OBSERVABLE_FEATURES)
  for (cc in zero_cols) set(out, j = cc, value = 0)
  out
}

res <- list()
for (s in recent) {
  for (lg in sort(unique(ms[sey == s, league]))) {
    blk <- ms[sey == s & league == lg]
    if (nrow(blk) < 20L) next
    is_subset <- lg %in% LIVE_SUBSET_LEAGUES
    g <- tryCatch({
      # `blk` (from `ms`) is now PARTIALLY enriched with xMetrics — see the
      # enrichment block above `ms[, role := ...]`: LIVE_XMETRICS_FEATURES (5
      # of 12) carry REAL per-match values on BOTH `ex` and `rw` below (the
      # blog's live scorer now derives these itself — see the LIVE_XMETRICS_
      # FEATURES comment above), while every other xMetrics column (xg_per90/
      # npxg_per90 + the 7 still-not-live features) was DROPPED entirely, not
      # zeroed, so it's excluded from both the coefficient dot-product and
      # position normalization on both sides — the same "fully unenriched"
      # convention this comment used to describe for all 12, now scoped to
      # just the remaining not-live subset (measured bias from getting this
      # wrong: debug/k_enrich_bias_check.R; K built on data missing xm entirely
      # tracks the blog-truth K* = mean(raw_obs − enriched exact) within ±0.01,
      # because the position norm absorbs the xm role-means on the exact side).
      ex <- as.data.table(score_one(blk, center = TRUE,  position_means = pm))
      rw_input <- if (is_subset) .zero_unobservable(blk) else blk
      rw <- as.data.table(score_one(rw_input, center = FALSE, position_means = NULL))
      key <- c("match_id", "player_id")
      m <- merge(ex[, c(key, "psv","osv","dsv"), with=FALSE],
                 rw[, c(key, "psv","osv","dsv"), with=FALSE],
                 by = key, suffixes = c("_ex","_rw"))
      role_lookup <- unique(blk[, c(key, "role"), with=FALSE], by = key)
      m <- merge(m, role_lookup, by = key)
      m[, `:=`(K_psr = psv_rw - psv_ex, K_osr = osv_rw - osv_ex, K_dsr = dsv_rw - dsv_ex)]
      gg <- m[, .(psr = mean(K_psr), osr = mean(K_osr), dsr = mean(K_dsr),
                  sd_psr = sd(K_psr), sd_osr = sd(K_osr), sd_dsr = sd(K_dsr), n = .N), by = role]
      gg[, `:=`(league = lg, sey = s, live_subset = is_subset)]
      # Diagnostic for subset leagues: the full-vector K alongside, so the delta
      # (= population-mean contribution of the live-unobservable features) is
      # visible in the log every rebuild.
      if (is_subset) {
        rw_full <- as.data.table(score_one(blk, center = FALSE, position_means = NULL))
        mf <- merge(ex[, c(key, "psv"), with=FALSE], rw_full[, c(key, "psv"), with=FALSE],
                    by = key, suffixes = c("_ex","_rw"))
        mf <- merge(mf, role_lookup, by = key)
        kf <- mf[, .(K_full = mean(psv_rw - psv_ex)), by = role]
        diag <- merge(gg[, .(role, K_obs = psr)], kf, by = "role")
        diag[, unobservable_mean := K_full - K_obs]
        cat(sprintf("\n[subset] %s %s — K on live-observable subset vs full vector:\n", lg, s))
        print(diag[order(role)])
      }
      gg
    }, error = function(e) {
      if (is_subset || lg %in% INLINE_LEAGUES) {
        cat(sprintf("[subset/inline] %s %s FAILED: %s — league skipped\n",
                    lg, s, conditionMessage(e)))
        NULL
      } else stop(e)   # club leagues keep fail-fast
    })
    if (!is.null(g)) res[[paste(s, lg)]] <- g
  }
}
K <- rbindlist(res)

# ---- THEORY CHECK (HARD): K must be constant within (league,role) ----
# This invariant is the whole justification for shipping ONE constant per group.
# Abort rather than ship a wrong constant if it ever breaks — e.g. a future
# round/matchweek column splitting calculate_psv's centering into subgroups, or
# GK/role-taxonomy drift contaminating a bucket. Checks all three components
# (K_osr/K_dsr can drift while K_psr stays flat).
# LIVE_SUBSET_LEAGUES are exempt BY DESIGN: their raw side drops per-player
# amounts of unobservable-feature contribution, so within-group SD is the
# irreducible live-estimate noise — reported below, not enforced.
SD_TOL <- 1e-6
K_full <- K[live_subset == FALSE]
max_sd <- max(c(K_full$sd_psr, K_full$sd_osr, K_full$sd_dsr), na.rm = TRUE)
cat(sprintf("\n[check] max within-group SD of K (psr/osr/dsr, full-vector leagues): %.3e (tol %.0e)\n",
            max_sd, SD_TOL))
print(head(K_full[order(-sd_psr), .(league, sey, role, n, sd_psr, sd_osr, sd_dsr)], 5))
if (is.finite(max_sd) && max_sd > SD_TOL)
  stop(sprintf(paste("K is NOT constant within (league,role): max SD %.3e > %.0e.",
                     "The centering-constant assumption is broken (round-split centering?",
                     "GK/role taxonomy drift?). Aborting rather than shipping a wrong constant."),
               max_sd, SD_TOL))
if (any(K$live_subset)) {
  cat("\n[subset] within-group SD for live-subset leagues (= irreducible live noise, per-90):\n")
  print(K[live_subset == TRUE, .(league, sey, role, n,
                                 sd_psr = round(sd_psr, 4), sd_osr = round(sd_osr, 4),
                                 sd_dsr = round(sd_dsr, 4))])
  # SANITY BOUND: subset SD is expected nonzero (live noise, observed 0.06-0.31
  # per-90 at WC 2026) but a centering regression or role-taxonomy drift that
  # would trip the hard invariant above for club leagues must not ship silently
  # for subset leagues either. 0.6 ~ 2x the worst legitimate noise observed.
  SUBSET_SD_MAX <- 0.6
  bad <- K[live_subset == TRUE &
             (sd_psr > SUBSET_SD_MAX | sd_osr > SUBSET_SD_MAX | sd_dsr > SUBSET_SD_MAX)]
  if (nrow(bad) > 0) {
    print(bad[, .(league, sey, role, n, sd_psr, sd_osr, sd_dsr)])
    stop(sprintf(paste("live-subset within-group SD exceeds %.1f per-90 — far above the",
                       "irreducible live noise. Likely a centering split or role-taxonomy",
                       "drift; aborting rather than shipping a wrong subset constant."),
                 SUBSET_SD_MAX))
  }
}

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
# __default__ from full-vector leagues ONLY: a live-subset K is a different
# convention (observable-subset raw) and would contaminate the prior every
# unseen club league inherits.
defaults <- K[sey == cur & live_subset == FALSE,
              .(psr = weighted.mean(psr, n), osr = weighted.mean(osr, n),
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

# Live-subset leagues bypass the shrinkage blend entirely: their prior would be
# a full-vector K (prev season or __default__), i.e. the wrong convention —
# blending 10–15% of it back in re-introduces exactly the over-subtraction the
# subset K exists to remove. Own current-season value, or drop if none.
g[, live_subset := league %in% LIVE_SUBSET_LEAGUES]
g[live_subset == TRUE & n > 0, `:=`(psr = c_psr, osr = c_osr, dsr = c_dsr)]
g <- g[!(live_subset == TRUE & n == 0)]

out <- rbind(
  g[, .(league, role, psr, osr, dsr, n, live_subset)],
  defaults[, .(league = "__default__", role, psr, osr, dsr, n = 0L, live_subset = FALSE)]
)
setorder(out, league, role)

# ---- UPLOAD GATE: expected live-subset leagues must be present ---------------
# Every WC failure path above (inline fetch, enrich xG-blind abort, scoring) is
# a logged skip so club diagnostics still print — but a CSV silently missing an
# expected subset league must NOT ship: the weekly workflow would clobber
# blog-latest with it, the blog falls back to the full-vector __default__ K for
# WC live scoring, and every live WC PSV drops by ~the unobservable-mean (the
# Kane bug, reintroduced by an upload). Fail the run instead — a stale-but-
# correct published CSV beats a fresh degraded one. "Expected" = has CURRENT-
# season rows (or a failed inline fetch), so a tournament aging out of the
# 2-season window is a legitimate absence, not a red run.
subset_expected <- intersect(LIVE_SUBSET_LEAGUES,
                             union(unique(ms[sey == cur_year, league]), inline_failures))
subset_missing <- setdiff(subset_expected, unique(out$league))
if (length(subset_missing) > 0)
  stop(sprintf(paste("live-subset league(s) %s expected this season but ABSENT from the",
                     "output — an upstream fetch/enrich/scoring failure was skipped (see",
                     "log above). Refusing to write a degraded constants CSV."),
               paste(subset_missing, collapse = ", ")))

fwrite(out, "inst/extdata/psv_live_constants.csv")
cat(sprintf("\nwrote inst/extdata/psv_live_constants.csv: %d rows (%d leagues + default)\n",
            nrow(out), uniqueN(out$league) - 1L))
print(out[league %in% c("ENG", intersect(LIVE_SUBSET_LEAGUES, out$league), "__default__")])
