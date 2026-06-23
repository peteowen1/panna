## Build a weekly EPR cache via the regression method (with league FE +
## opponent strength + cross-league EPV calibration + data-chosen decay),
## mirroring opta_psr_weekly.parquet's structure.
##
## RUN FROM THE PACKAGE ROOT (cwd = panna/) — all input/output paths below are
## cwd-relative, not script-relative. Moved here from debug/keep/ 2026-06-23 so
## it's version-controlled and can run in CI (epr-weekly-snapshot.yml); the old
## location was gitignored, which is why EPR had no automated rebuild.
## INCREMENTAL: reuses snapshots older than a 28-day buffer from the existing
## opta_epr_weekly.parquet and recomputes only the recent window (~3 min vs
## ~25 min full). Set EPR_FORCE_FULL_REBUILD=1 to recompute every snapshot.
##
## Method: calculate_epr_regression() — for each snapshot date, fit a weighted
## ridge regression
##     epv_p90 ~ β_player + α_league_season + γ × opp_def_rating
## with exponential time-decay weights. β_player is the EPR.
##
## Cross-league calibration: each row's y_off / y_def is shifted by a per-league
## additive offset so β_player is on a single Big-5-equivalent per-90 scale —
## replacing the coarse player × tier interaction. The offset is estimated by the
## co-occurrence network (see section 3b + the note below), NOT applied at the end
## like PSR's — it shifts the regression inputs, so changing it requires a FULL
## snapshot rebuild (EPR_FORCE_FULL_REBUILD), not a cheap additive re-apply.
##
## Inputs: per-season game_logs.parquet files + opta_lineups (for opponents)
##         + cache-opta/team_season_strength.parquet (for opp_def_rating)
## Output: opta_epr_weekly.parquet — one row per (player_id × snapshot_date)
##
## League offsets are computed IN-RUN (no cached input) via build_league_network()
## — the same same-season co-occurrence estimator PSR uses — run on offensive and
## defensive EPV (see section 3b). Migrated 2026-06-23 off the legacy
## compute_league_offsets() (which over-swung on thin-bridge leagues like MLS).

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(Matrix); library(glmnet)
  devtools::load_all()
})

t_log <- function(msg) cat(sprintf("[%s] %s\n",
                                     format(Sys.time(), "%H:%M:%S"), msg))
t_log("===== build_epr_weekly (regression method) =====")

cache_dir <- "data-raw/cache-predictions-opta"

## --- 1. Load and stack ALL available game_logs ---
## Loads every game_logs_*.parquet in the cache (2015-2016 -> current, plus
## Brazilian Serie A) so the EPR weekly cache spans the full history — the
## match-prediction pipeline (step 02) needs historical EPR to attach to
## training matches, not just the recent window.
t0 <- Sys.time()
files <- list.files(cache_dir, pattern = "^game_logs_.*\\.parquet$",
                    full.names = TRUE)
gl <- rbindlist(lapply(files, read_parquet),
                use.names = TRUE, fill = TRUE)
gl[, match_date := as.Date(sub("Z$","", match_date))]
gl <- gl[!is.na(epv_offensive_adj) & !is.na(epv_defensive_adj),
         .(player_id, player_name, match_id, match_date, league, season,
            team_id, minutes_played = total_minutes,
            epv_offensive = epv_offensive_adj,
            epv_defensive = epv_defensive_adj)]
gl[, season_end_year := fifelse(month(match_date) >= 7L,
                                 year(match_date) + 1L,
                                 year(match_date))]
t_log(sprintf("loaded game_logs in %.1fs: %s rows, %s players, %s matches",
              as.numeric(Sys.time()-t0, units="secs"),
              format(nrow(gl), big.mark=","),
              format(uniqueN(gl$player_id), big.mark=","),
              format(uniqueN(gl$match_id), big.mark=",")))

## --- 2. Build match → opponent lookup (once for all snapshots) ---
t0 <- Sys.time()
lu <- as.data.table(read_parquet("../pannadata/data/opta/opta_lineups.parquet"))
match_teams <- unique(lu[, .(match_id, team_id, team_name)])
match_pairs <- match_teams[, {
  if (.N == 2L) list(team_id = team_id, opp_team_id = rev(team_id),
                       opp_team_name = rev(team_name))
  else list(team_id = team_id, opp_team_id = NA_character_,
             opp_team_name = NA_character_)
}, by = match_id]
gl <- match_pairs[gl, on = c("match_id","team_id")]
t_log(sprintf("opponent lookup + join in %.1fs (NAs: %d/%d)",
              as.numeric(Sys.time()-t0, units="secs"),
              sum(is.na(gl$opp_team_id)), nrow(gl)))

## --- 3. Join opp_def_rating from team_season_strength ---
t0 <- Sys.time()
ts <- as.data.table(read_parquet("data-raw/cache-opta/team_season_strength.parquet"))
team_name_map <- unique(lu[, .(team_id, team_name, season)])
team_name_map[, season_end_year := {
  if (all(grepl("^\\d{4}-\\d{4}$", season))) as.integer(sub(".*-", "", season))
  else if (all(grepl("^\\d{4} ", season))) as.integer(sub(" .*", "", season))
  else rep(NA_integer_, .N)
}, by = season]
team_name_map <- unique(team_name_map[, .(team_id, season_end_year, team_name)])
ts_id <- merge(ts, team_name_map, by = c("team_name","season_end_year"))
gl <- merge(gl, ts_id[, .(team_id, season_end_year,
                            opp_def_rating = def_rating)],
             by.x = c("opp_team_id","season_end_year"),
             by.y = c("team_id","season_end_year"), all.x = TRUE)
n_na <- sum(is.na(gl$opp_def_rating))
gl[is.na(opp_def_rating), opp_def_rating := 0]
t_log(sprintf("opp_def_rating join in %.1fs: %d rows fallback to 0 (%.1f%%)",
              as.numeric(Sys.time()-t0, units="secs"),
              n_na, 100*n_na/nrow(gl)))

## --- 3b. Per-league EPV calibration offsets (co-occurrence NETWORK) ---
## Offsets shift each row's per-90 EPV to a Big-5-equivalent scale so β_player
## is comparable across leagues. Estimated with build_league_network() — the
## SAME same-season co-occurrence estimator PSR uses (PSR <- PSV network) — run
## separately on offensive and defensive EPV to produce offset_off / offset_def.
##
## This replaces the legacy UCL-anchored compute_league_offsets(), which
## over-swung on thin-bridge leagues (e.g. MLS -0.346 vs network -0.102 — it was
## burying the entire league, suppressing Messi/Evander/etc.; CAFCL -0.169 from a
## 7-player chain). The network pools every same-season pairing a player straddles
## (domestic + continental + international), so it is robust on thin bridges and
## anchors Big-5 = 0. Validated 2026-06-23: cor(old,new)=0.86, mean|ΔEPR|=0.0018,
## changes concentrated on MLS (+~0.06, the correction); Big-5/elite unchanged.
## build_league_network needs `total_minutes`; gl renamed it to minutes_played.
gl[, total_minutes := minutes_played]
off_net <- build_league_network(gl, value_col = "epv_offensive", verbose = FALSE)
def_net <- build_league_network(gl, value_col = "epv_defensive", verbose = FALSE)
league_offsets <- merge(off_net[, .(league, offset_off = offset)],
                        def_net[, .(league, offset_def = offset)],
                        by = "league", all = TRUE)
league_offsets[is.na(offset_off), offset_off := 0]
league_offsets[is.na(offset_def), offset_def := 0]
league_offsets[, offset_tot := offset_off + offset_def]
t_log(sprintf("EPV-network league offsets: %d leagues (Big-5-anchored)", nrow(league_offsets)))
print(league_offsets[order(offset_tot), .(league,
                       offset_off = round(offset_off, 3),
                       offset_def = round(offset_def, 3),
                       offset_tot = round(offset_tot, 3))])

## --- 4. Define snapshot dates (full history, weekly) ---
## Weekly snapshots across the whole game_logs history. Step 02 of the match-
## prediction pipeline takes the latest snapshot within each season_end_year,
## so coverage must span every training season — mirroring opta_psr_weekly.
end_date   <- max(gl$match_date)
start_date <- min(gl$match_date) + 90L   # skip first ~3 months (thin history)
snapshots  <- seq(start_date, end_date, by = "week")
t_log(sprintf("Building %d weekly snapshots: %s -> %s",
              length(snapshots), start_date, end_date))

out_path <- "../pannadata/data/opta/opta_epr_weekly.parquet"

## --- 4b. Incremental: reuse snapshots older than a recompute buffer ----
## Each snapshot is an independent calculate_epr_regression() fit filtered to
## `match_date < ref_date` (verified leak-free), so a snapshot older than the
## buffer is fully determined by matches that already existed last run — it is
## immutable and can be reused verbatim. We recompute only the recent window
## (covers new matches + late-arriving results) and any brand-new snapshot
## dates the advancing end_date introduced. ~25 min full rebuild -> ~3 min.
## Set EPR_FORCE_FULL_REBUILD=1 to force all 1346 snapshots (e.g. after a
## method/offset change that shifts historical values).
RECOMPUTE_BUFFER_DAYS <- 28L
force_full_epr <- nzchar(Sys.getenv("EPR_FORCE_FULL_REBUILD"))
keep_existing <- NULL
if (force_full_epr) {
  t_log("EPR_FORCE_FULL_REBUILD set — recomputing every snapshot")
} else if (file.exists(out_path)) {
  existing_epr <- tryCatch(as.data.table(read_parquet(out_path)),
                           error = function(e) { t_log(sprintf(
                             "could not read existing parquet (%s) — full rebuild",
                             conditionMessage(e))); NULL })
  if (!is.null(existing_epr) && "snapshot_date" %in% names(existing_epr) &&
      nrow(existing_epr) > 0) {
    existing_epr[, snapshot_date := as.Date(snapshot_date)]
    cutoff <- end_date - RECOMPUTE_BUFFER_DAYS
    keep_existing <- existing_epr[snapshot_date < cutoff]
    snapshots <- snapshots[snapshots >= cutoff]
    t_log(sprintf("Incremental: reuse %s rows (<%s), recompute %d snapshots (>=%s)",
                  format(nrow(keep_existing), big.mark = ","), cutoff,
                  length(snapshots), cutoff))
  } else {
    t_log("existing parquet unusable (no snapshot_date / empty) — full rebuild")
  }
} else {
  t_log("no existing parquet — full rebuild")
}

## --- 5. Run calculate_epr_regression() at each snapshot ---
## Decay 900 was chosen by held-out MSE search in build_epr_regression.R.
## All decays produced near-identical MSE so this is robust.
DECAY <- 900L
PRIOR_STRENGTH <- 5
ALPHA <- 0   # pure ridge — keeps all players, no zeroing

t0 <- Sys.time()
all_snaps <- vector("list", length(snapshots))
for (i in seq_along(snapshots)) {
  d <- snapshots[i]
  res <- calculate_epr_regression(
    gl, ref_date = d, decay = DECAY,
    prior_strength = PRIOR_STRENGTH, alpha = ALPHA,
    league_offsets = league_offsets,
    verbose = FALSE
  )
  if (nrow(res) > 0) {
    res[, snapshot_date := d]
    all_snaps[[i]] <- res
  }
  if (i %% 10 == 0) {
    el <- as.numeric(Sys.time() - t0, units = "secs")
    rate <- el / i
    eta <- rate * (length(snapshots) - i)
    cat(sprintf("  snapshot %d/%d (%s) — %.0fs elapsed, ~%.0fs remaining\n",
                i, length(snapshots), d, el, eta))
  }
}
new_epr <- rbindlist(all_snaps, use.names = TRUE, fill = TRUE)
n_new_snaps <- if (nrow(new_epr) > 0) uniqueN(new_epr$snapshot_date) else 0L
if (n_new_snaps == 0L) {
  stop("All recomputed snapshots failed (0 produced) — refusing to publish. ",
       "Reused rows alone would stagnate the EPR cache.", call. = FALSE)
}

## Merge reused (immutable) rows with freshly recomputed ones. Dedup on
## (snapshot_date, player_id) keeping the new rows on any overlap.
if (!is.null(keep_existing) && nrow(keep_existing) > 0) {
  common <- intersect(names(keep_existing), names(new_epr))
  epr_weekly <- rbindlist(list(keep_existing[, ..common], new_epr[, ..common]),
                          use.names = TRUE, fill = TRUE)
  epr_weekly <- unique(epr_weekly, by = c("snapshot_date", "player_id"),
                       fromLast = TRUE)
  t_log(sprintf("Merged: %s reused + %s recomputed -> %s rows",
                format(nrow(keep_existing), big.mark = ","),
                format(nrow(new_epr), big.mark = ","),
                format(nrow(epr_weekly), big.mark = ",")))
} else {
  epr_weekly <- new_epr
}
setorder(epr_weekly, snapshot_date, player_id)
t_log(sprintf("Done fitting %d snapshots in %.1f min",
              length(snapshots),
              as.numeric(Sys.time()-t0, units="mins")))
t_log(sprintf("Output: %d rows across %d snapshots, %d unique players",
              nrow(epr_weekly), uniqueN(epr_weekly$snapshot_date),
              uniqueN(epr_weekly$player_id)))

## --- 6. Save ---
write_parquet(epr_weekly, out_path)
t_log(sprintf("Saved -> %s (%.1f MB)",
              out_path, file.size(out_path) / 1e6))

## --- 6. Latest snapshot sanity check ---
latest <- max(epr_weekly$snapshot_date)
cat(sprintf("\nLatest snapshot: %s\n", latest))
top10 <- epr_weekly[snapshot_date == latest][order(-epr)][1:10]
cat("Top 10 by EPR:\n")
print(top10[, .(player_name,
                 epr = round(epr, 3),
                 epr_off = round(epr_offensive, 3),
                 epr_def = round(epr_defensive, 3),
                 wt_games = round(wt_games, 1),
                 n_games)])
