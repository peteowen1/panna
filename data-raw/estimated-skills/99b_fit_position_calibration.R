## 99b_fit_position_calibration.R ----------------------------------------
## DIAGNOSTIC / FITTER, not a pipeline step. Numbered 99 so it is obviously
## terminal: run_skills_pipeline.R does not source it, and nothing downstream
## reads its output yet.
##
##   cd panna && Rscript data-raw/estimated-skills/99b_fit_position_calibration.R
##
## WHAT IT ANSWERS. `psv_calibration.csv` / `psr_calibration.csv` key on four
## position levels (GK/DEF/MID/FWD). Pete's question, 2026-09-12: a centre-back
## and a full-back both sit inside DEF's single factor -- should they? This
## fits the same calibration at a finer 8-bucket grain and reports whether the
## buckets inside one broad group actually differ.
##
## It is a MEASUREMENT, not a shipping path. It deliberately does NOT write a
## calibration CSV: turning slopes into shipped factors needs a normalisation
## decision (see the closing note) that has not been made.
##
## METHOD -- deliberately identical to 99_sense_check_ratings.R's calibrate(),
## because that harness is already validated and docs/reference/
## RATING_CALIBRATION.md documents three confidently-wrong attempts that each
## skipped one of its requirements:
##   - point-in-time ratings, rolled to match_date - 1 (a season rating leaks,
##     and once flipped the sign of panna#202 -- keepers play every minute, so
##     their season rating is the most contaminated of any position);
##   - STARTERS ONLY (skipping this made keepers look worth 3x);
##   - minutes-weighted SUM per team, never a mean (the sum makes the target
##     slope exactly 1.0 and readable; a mean divides by ~10.9 nineties);
##   - own-minus-opponent differenced against goal difference;
##   - report the top-20 position mix next to the factors, on named players.
##
## The one thing it adds is the bucket key. See the taxonomy note below.
##
## VINTAGE WARNING -- READ BEFORE QUOTING ANY NUMBER FROM THIS FILE.
## `opta_psr_weekly.parquet` is written INCREMENTALLY by 08b, so a calibration
## merge only rewrites the newest snapshots. As of 2026-09-14 the file holds 245
## snapshots back to 2014-08-25 and only the last few carry the 2026-09-12
## 4-bucket calibration; the rest are pre-calibration. Measured consequence, on
## one identical >=900-minute cohort: 12 keepers in the top 20 valued as-at,
## 2 valued at the latest snapshot (T. Courtois 0.454 as-at 2026-07-06 vs 0.237
## at 2026-09-14 -- that gap is the calibration landing, not form).
##
## What that does and does not invalidate:
##   - the SLOPES are fine as a calibration fit. Every bucket is fitted on the
##     same vintage, and a calibration is supposed to be fitted on its
##     UNcalibrated input. This is the same input the shipped 4-bucket table
##     was fitted on.
##   - the BEFORE/AFTER top-20 anchor OVERSTATES the improvement, because part
##     of what the 8-bucket factors appear to fix is really the 4-bucket
##     calibration that has not reached the old snapshots yet. Read its
##     DIRECTION, never its magnitude.
##   - once 08b is rebuilt with PSR_FORCE_FULL_REBUILD=1, this file must be
##     re-run and the slopes WILL move toward 1.0, because the input will then
##     already be calibrated. At that point the fit becomes a RESIDUAL fit on
##     top of the 4-bucket factors, which is a different (and also valid)
##     shipping shape -- see the closing note. Do not mix the two.

devtools::load_all("."); suppressPackageStartupMessages({library(data.table); library(arrow)})

PSR_WEEKLY <- "../pannadata/data/opta/opta_psr_weekly.parquet"
OUT_CSV    <- "data-raw/cache-skills/99b_position_calibration_8bucket.csv"

## ---- the 8-bucket taxonomy (Pete's spec, 2026-09-14) --------------------
## A layer ABOVE GK/DEF/MID/FWD, built by collapsing classify_role()'s 16 roles.
## The collapses, and why each one:
##   LB/RB + LWB/RWB -> FB    the wing-back distinction is a formation label,
##                            not a different job; splitting them halves a
##                            bucket for no measured gain.
##   LM/RM + LW/RW   -> W     wide midfielders and wingers do the same work.
##                            This is the collapse that mattered: in the
##                            4-bucket taxonomy LW/RW land in FWD while LM/RM
##                            land in MID, so "wingers" straddle two factors.
##   LF/RF + CF      -> ST    LF/RF is negligible volume and the players in it
##                            (Vinícius Júnior, Luis Díaz) are inside forwards.
## UNK is the literal string "Substitute" plus blank positions -- it is not a
## position, it is a missing one, which is why it is resolved rather than
## bucketed (below) and never gets a factor of its own.
BUCKETS <- c("GK","CB","FB","DM","CM","AM","W","ST")

role8_of <- function(role16) {
  fcase(role16 == "GK",  "GK",
        role16 == "CB",  "CB",
        role16 %in% c("LB","RB","LWB","RWB"), "FB",
        role16 == "DM",  "DM",
        role16 == "CM",  "CM",
        role16 == "CAM", "AM",
        role16 %in% c("LM","RM","LW","RW"),   "W",
        role16 %in% c("CF","LF","RF"),        "ST",
        default = NA_character_)
}

## ---- match scaffolding (same source as 99_) ----------------------------
ms <- as.data.table(readRDS("data-raw/cache-skills/01_match_stats.rds"))
ms[, match_date := as.Date(match_date)]
ms[, season_end_year := as.integer(extract_season_end_year(season))]
ms <- ms[!is.na(season_end_year) & !is.na(total_minutes) & total_minutes > 0]
ms_star <- ms[!is.na(position) & position != "Substitute" & position != ""]
mo <- unique(ms[, .(match_id, team_id, home_score, away_score, is_home)])[
        , .SD[1L], by = .(match_id, team_id)]

## ---- resolve a bucket per (player, season) -----------------------------
## Per-ROW labels cannot be used directly: a row's `position` is that match's
## role, so a CB who filled in at RB for one game would move buckets mid-season
## and be calibrated against two different factors. Resolve to the
## minutes-weighted modal bucket per (player, season), with a career fallback
## for a season whose every row is UNK.
##
## season_end_year comes from the season LABEL via extract_season_end_year(),
## never from match_date -- three label formats share one end year, and a
## month>=7 heuristic mis-seasons 100% of Leagues Cup and 50% of MLS.
ms[, role16 := classify_role(position, position_side)]
ms[, role8 := role8_of(role16)]
mins <- as.numeric(ms$total_minutes); mins[is.na(mins)] <- 0
ms[, .mins := mins]

st <- ms[!is.na(role8), .(m = sum(.mins)), by = .(player_id, season_end_year, role8)]
setorder(st, player_id, season_end_year, -m)
smod <- st[, .SD[1L], by = .(player_id, season_end_year)][
           , .(player_id, season_end_year, r_s = role8)]
ct <- ms[!is.na(role8), .(m = sum(.mins)), by = .(player_id, role8)]
setorder(ct, player_id, -m)
cmod <- ct[, .SD[1L], by = player_id][, .(player_id, r_c = role8)]
pos_ref8 <- merge(smod, cmod, by = "player_id", all.x = TRUE)
pos_ref8[, role8 := fcoalesce(r_s, r_c)]
pos_ref8 <- pos_ref8[!is.na(role8), .(player_id, season_end_year, role8)]

## Coverage, before any headline number (99_'s rule, and stats-discipline §7).
res_join <- merge(ms[, .(player_id, season_end_year, .mins)], pos_ref8,
                  by = c("player_id","season_end_year"), all.x = TRUE)
cat(sprintf("pos_ref8: %s player-seasons\n", format(nrow(pos_ref8), big.mark = ",")))
cat(sprintf("raw row labels unresolved: %.2f%% of minutes\n",
            100 * sum(ms[is.na(role8)]$.mins) / sum(ms$.mins)))
cat(sprintf("after (player,season) resolve + career fallback: %.2f%% of minutes\n",
            100 * sum(res_join[is.na(role8)]$.mins) / sum(res_join$.mins)))
cat("\nminutes per bucket, all seasons (k 90s):\n")
print(res_join[, .(k90s = round(sum(.mins)/90/1000, 1),
                   pct  = round(100*sum(.mins)/sum(res_join$.mins), 1))
               , by = .(bucket = fcoalesce(role8, "UNRESOLVED"))][order(-k90s)])

## ---- as-at PSR: last weekly snapshot strictly before the match ---------
psr <- as.data.table(read_parquet(PSR_WEEKLY))
psr[, snapshot_date := as.Date(snapshot_date)]
## State the vintage before any number, so a mixed-vintage file is never read
## as a finding (see the VINTAGE WARNING at the top).
cat(sprintf("\nPSR source: %s snapshots, %s to %s (file mtime %s)\n",
            uniqueN(psr$snapshot_date), format(min(psr$snapshot_date)),
            format(max(psr$snapshot_date)),
            format(file.mtime(PSR_WEEKLY), "%Y-%m-%d %H:%M")))
psr <- psr[, .(player_id, snapshot_date, val = psr)][!is.na(val)]
psr[, snap_actual := snapshot_date]
base <- ms_star[, .(player_id, match_id, team_id, season_end_year, total_minutes,
                    match_date)]
base[, join_date := match_date - 1L]
setkey(psr, player_id, snapshot_date); setkey(base, player_id, join_date)
asat <- psr[base, roll = TRUE]
## The roll join OVERWRITES the join key with the requested date, so the lag
## assert is only non-vacuous because snap_actual was copied first.
lag <- as.integer(asat$match_date - asat$snap_actual)
stopifnot(min(lag, na.rm = TRUE) >= 1)
asat <- asat[!is.na(val)]

## ---- fit --------------------------------------------------------------
x <- merge(asat, pos_ref8, by = c("player_id","season_end_year"), all.x = TRUE)
x <- x[!is.na(role8)]
## Keep only team-matches where EVERY starter has an as-at value, so a team's
## minutes-weighted sum is never a partial sum compared against a full one.
x[, has := !is.na(val)]
full <- x[, .(n = .N, ok = sum(has)), by = .(match_id, team_id)][ok == n,
          .(match_id, team_id)]
x <- merge(x, full, by = c("match_id","team_id"))
x[, mins90 := as.numeric(total_minutes)/90]

tm <- dcast(x[, .(s = sum(val*mins90)), by = .(match_id, team_id, role8)],
            match_id + team_id ~ role8, value.var = "s", fill = 0)
pg <- intersect(BUCKETS, setdiff(names(tm), c("match_id","team_id")))
tm <- merge(tm, mo, by = c("match_id","team_id"))
n2 <- tm[, .N, by = match_id][N == 2, match_id]; t2 <- tm[match_id %in% n2]
op <- copy(t2)[, c("match_id","team_id",pg), with = FALSE]
setnames(op, setdiff(names(op),"match_id"), paste0("o_", setdiff(names(op),"match_id")))
g <- merge(t2, op, by = "match_id", allow.cartesian = TRUE)[team_id != o_team_id]
g[, gd := fifelse(as.logical(is_home), home_score - away_score, away_score - home_score)]
for (p in pg) g[, paste0("d_", p) := get(p) - get(paste0("o_", p))]
dc <- paste0("d_", pg)

m1 <- lm(reformulate(dc, "gd"), data = g)
co <- summary(m1)$coefficients
res <- data.table(bucket = sub("^d_", "", rownames(co)[-1]),
                  slope  = round(co[-1, 1], 4),
                  se     = round(co[-1, 2], 4))
setorder(res, slope)
cat(sprintf("\n=== 8-bucket slopes, PSR as-at === n team-matches = %s | R2 %.4f\n",
            format(nrow(g), big.mark = ","), summary(m1)$r.squared))
print(res)

## ---- THE ACTUAL QUESTION: do buckets inside one broad group differ? ------
## A slope table alone does not answer it -- two cells can look far apart and
## be one standard error away. These coefficients come from ONE regression and
## are correlated, so the contrast se must come from the full vcov, not from
## sqrt(se1^2 + se2^2) (which ignores the covariance and is the wrong number).
V <- vcov(m1)
contrast <- function(a, b) {
  ka <- paste0("d_", a); kb <- paste0("d_", b)
  est <- unname(coef(m1)[ka] - coef(m1)[kb])
  se  <- sqrt(V[ka,ka] + V[kb,kb] - 2*V[ka,kb])
  data.table(contrast = sprintf("%s - %s", a, b), broad_group = NA_character_,
             diff = round(est, 4), se = round(se, 4), t = round(est/se, 2))
}
ct <- rbindlist(list(
  contrast("FB","CB")[, broad_group := "DEF"],
  contrast("CM","DM")[, broad_group := "MID"],
  contrast("AM","DM")[, broad_group := "MID"],
  contrast("CM","AM")[, broad_group := "MID"],
  contrast("ST","W") [, broad_group := "FWD"],
  contrast("DM","GK")[, broad_group := "across (DM sits on the GK slope)"]))
cat("\nwithin-broad-group contrasts (|t| > 2 means one factor cannot serve both):\n")
print(ct[, .(broad_group, contrast, diff, se, t)])

## RATING_CALIBRATION.md requires precision-shrinking thin cells. Reported
## rather than applied: every bucket's se is ~0.04-0.08 against slopes of
## 1.3-2.2, so no cell is thin enough for shrinkage to move it materially.
## If a future finer taxonomy produces an se comparable to its own slope,
## shrink that cell toward the broad-group slope before shipping it.
cat(sprintf("\nthinnest cell: %s (se %.4f = %.1f%% of its slope)\n",
            res[which.max(se/abs(slope))]$bucket, max(res$se),
            100 * max(res$se / abs(res$slope))))
fwrite(res, OUT_CSV)
cat(sprintf("wrote %s\n", OUT_CSV))

## ---- ANCHOR: does applying these factors move the top 20 the right way? --
## Pre-registered before looking (Pete, 2026-09-14): the top 20 should be
## "mostly AMs or wingers and ST and CM", with no more than 3-4 FBs and 4-5 CBs.
## Ranking by val*slope is equivalent to ranking by val*factor -- the
## normalisation from slopes to shipped factors is one common constant, so it
## cannot change the cross-position ordering.
##
## League matters as much as position: a top 20 pooled over all 31 competitions
## reads a league-offset defect AS a position defect (RATING_CALIBRATION.md's
## "R. Williams, CAF CL, 4th in the world"). Both are reported.
sl <- setNames(res$slope, res$bucket)
BIG5 <- c("EPL","La_Liga","Serie_A","Bundesliga","Ligue_1","UCL")

## max(season_end_year) is usually a barely-started season where nobody clears
## the 900-minute gate, which silently yields an EMPTY top 20.
cohort <- x[, .(mins = sum(as.numeric(total_minutes))), by = .(player_id, season_end_year)][
            mins >= 900, .N, by = season_end_year][N >= 50]
sey <- max(cohort$season_end_year)
cat(sprintf("\nanchor season: %d (latest with a >=50-player 900-minute cohort)\n", sey))

lgm <- unique(ms_star[season_end_year == sey, .(player_id, competition)])[
         , .SD[1L], by = player_id]
nm  <- unique(ms_star[, .(player_id, player_name)])[, .SD[1L], by = player_id]
pl <- x[season_end_year == sey][
        , .(val = val[.N], mins = sum(as.numeric(total_minutes)), role8 = role8[.N]),
        by = player_id][mins >= 900]
pl <- merge(merge(pl, lgm, by = "player_id", all.x = TRUE), nm, by = "player_id",
            all.x = TRUE)
pl[, val_cal := val * sl[role8]]
stopifnot(nrow(pl) >= 50)

mix <- function(d, col, lab) {
  setorderv(d, col, -1)
  mx <- sort(table(head(d, 20)$role8), decreasing = TRUE)
  cat(sprintf("%-38s: %s\n", lab,
              paste(sprintf("%s=%d", names(mx), as.integer(mx)), collapse = "  ")))
}
cat("\n########## top-20 position mix, before vs after ##########\n")
mix(pl,                        "val",     "BEFORE (current 4-bucket) all comps")
mix(pl[competition %in% BIG5], "val",     "BEFORE  Big-5/UCL")
mix(pl,                        "val_cal", "AFTER (8-bucket fitted) all comps")
mix(pl[competition %in% BIG5], "val_cal", "AFTER   Big-5/UCL")

b5 <- pl[competition %in% BIG5]
setorder(b5, -val_cal)
cat("\nAFTER, Big-5/UCL top 15 (named players, not summary stats):\n")
print(head(b5[, .(player_name, role8, competition, val = round(val, 3),
                  val_cal = round(val_cal, 3))], 15))

cat("\n---- NOT DECIDED, and why nothing ships from this file ----\n")
cat("Slopes are not factors. Shipping needs a normalisation choice: scale so\n")
cat("the minutes-weighted mean factor is 1 (preserves the overall PSR scale,\n")
cat("what PSV's GK factor did) or so the target slope is 1.0 (makes PSR\n")
cat("goal-denominated, which it currently is not -- PSR has no goals-unit\n")
cat("scale constant, unlike PSV's PSV_RELIABILITY_GD_SCALE).\n")
