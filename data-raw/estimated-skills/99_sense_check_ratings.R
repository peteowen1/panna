## 99_sense_check_ratings.R -----------------------------------------------
## DIAGNOSTIC, not a pipeline step. Numbered 99 so it is obviously terminal:
## nothing consumes its output and run_skills_pipeline.R does not source it.
##
##   cd panna && Rscript data-raw/estimated-skills/99_sense_check_ratings.R
##
## Run it after any retrain, before believing a rating. It exists because the
## defects this repo has shipped were never visible in aggregate metrics:
##   - the opponent control sat inert in 8 competitions for 3.5 months while
##     coverage read "healthy" (panna#224) -- only WITHIN-league variance saw it;
##   - a GK position factor was argued over for days on PSR as a whole when the
##     over-spread lives entirely in OSR (panna#227);
##   - DSR's overall calibration slope reads 1.025, apparently perfect, while its
##     goalkeeper cell is -2.08 and the positions cancel (panna#227).
## Every one is caught by splitting a number by position or league and looking at
## a named player. That is all this file does.
##
## EVERY RATING HERE IS EVALUATED AS-AT. panna's ratings are continuous and
## weekly-snapshotted -- PSR/OSR/DSR in opta_psr_weekly.parquet, EPR/offensive/
## defensive in opta_epr_weekly.parquet -- so each is rolled to the last snapshot
## strictly BEFORE the match. The seasonal_* tables are a per-season VIEW of a
## continuous quantity, not the rating; using them here would be the leak
## RATING_CALIBRATION.md lists first among its four non-negotiable design points
## (it once flipped the sign of #202, because keepers play every minute and so
## have the most contaminated season rating of any position).
##
## Per-game metrics (psv/epv/wpa/panna/piero) have no snapshot file, so their
## as-at value is an EXPANDING prior-form mean: cumulative metric and minutes over
## a player's earlier matches only, shifted so the current match never contributes.
##
## Battery per metric:
##   A. coverage        -- % of starters with no as-at value
##   B. position spread -- sd by position, GK/DEF ratio
##   C. calibration     -- minutes-weighted SUM, own-minus-opponent, vs goal
##                         difference. Target slope 1.00 for goal-denominated
##                         metrics; for others read the SHAPE (position ordering,
##                         GK sign) not the level.
##   D. GK sign check   -- a negative keeper cell is the #209/#210/#226/#227
##                         inversion family and must abort belief in the metric.

devtools::load_all("."); suppressPackageStartupMessages({library(data.table); library(arrow)})

PSR_WEEKLY <- "../pannadata/data/opta/opta_psr_weekly.parquet"
EPR_WEEKLY <- "../pannadata/data/opta/opta_epr_weekly.parquet"
GL_DIR     <- "data-raw/cache-predictions-opta"

## ---- shared match scaffolding -------------------------------------------
ms <- as.data.table(readRDS("data-raw/cache-skills/01_match_stats.rds"))
ms[, match_date := as.Date(match_date)]
ms[, season_end_year := as.integer(extract_season_end_year(season))]
ms <- ms[!is.na(season_end_year) & !is.na(total_minutes) & total_minutes > 0]
ms_star <- ms[!is.na(position) & position != "Substitute" & position != ""]
mo <- unique(ms[, .(match_id, team_id, home_score, away_score, is_home)])[
        , .SD[1L], by = .(match_id, team_id)]
pos_ref <- unique(as.data.table(
  readRDS("data-raw/cache-skills/06_seasonal_ratings.rds")$seasonal_psr)[
    , .(player_id, season_end_year, primary_position)])

#' Team-level calibration of an as-at player value against goal difference.
#' Minutes-weighted SUM (never a mean: the sum makes the target slope exactly
#' 1.0 and readable), own-minus-opponent differenced, starters only.
calibrate <- function(x, label) {
  x <- merge(x, pos_ref, by = c("player_id","season_end_year"), all.x = TRUE)
  x[, pos_grp := fcase(primary_position=="GK","GK", primary_position=="DEF","DEF",
                       primary_position=="MID","MID", primary_position=="FWD","FWD",
                       default = NA_character_)]
  x <- x[!is.na(pos_grp)]
  x[, has := !is.na(val)]
  pct_na <- 100 * mean(!x$has)
  ## zero NAs: keep only team-matches where EVERY starter has a value
  full <- x[, .(n=.N, ok=sum(has)), by=.(match_id, team_id)][ok==n, .(match_id, team_id)]
  x <- merge(x, full, by = c("match_id","team_id"))
  if (nrow(x) < 5000) return(list(skip = TRUE, pct_na = pct_na))
  x[, mins90 := as.numeric(total_minutes)/90]
  sds <- x[, .(sd = sd(val)), by = pos_grp]
  tm <- dcast(x[, .(s = sum(val*mins90)), by=.(match_id, team_id, pos_grp)],
              match_id + team_id ~ pos_grp, value.var = "s", fill = 0)
  pg <- setdiff(names(tm), c("match_id","team_id"))
  tm <- merge(tm, mo, by = c("match_id","team_id"))
  n2 <- tm[, .N, by = match_id][N == 2, match_id]; t2 <- tm[match_id %in% n2]
  if (nrow(t2) < 2000) return(list(skip = TRUE, pct_na = pct_na))
  op <- copy(t2)[, c("match_id","team_id",pg), with = FALSE]
  setnames(op, setdiff(names(op),"match_id"), paste0("o_", setdiff(names(op),"match_id")))
  g <- merge(t2, op, by = "match_id", allow.cartesian = TRUE)[team_id != o_team_id]
  g[, gd := fifelse(as.logical(is_home), home_score-away_score, away_score-home_score)]
  for (p in pg) g[, paste0("d_",p) := get(p) - get(paste0("o_",p))]
  dc <- paste0("d_", intersect(c("GK","DEF","MID","FWD"), pg))
  g[, vdiff := rowSums(.SD), .SDcols = dc]
  m0 <- lm(gd ~ vdiff, data = g); m1 <- lm(reformulate(dc, "gd"), data = g)
  co <- summary(m1)$coefficients
  gk <- sds[pos_grp=="GK"]$sd; df <- sds[pos_grp=="DEF"]$sd
  list(skip = FALSE, label = label, n = nrow(g), pct_na = pct_na,
       global = unname(coef(m0)["vdiff"]), r2 = summary(m0)$r.squared,
       pos = setNames(round(co[dc,"Estimate"], 3), sub("^d_","",dc)),
       gk_def = if (length(gk) && length(df) && df > 0) gk/df else NA_real_)
}

#' As-at value from a WEEKLY SNAPSHOT file: last snapshot strictly before the
#' match. The snapshot_date column is copied first because data.table's roll
#' join OVERWRITES the join key with the requested date -- without the copy the
#' lag assert below is vacuous (it would compare 1 >= 1).
asat_from_weekly <- function(path, vcol) {
  w <- as.data.table(read_parquet(path))
  if (!vcol %in% names(w)) return(NULL)
  w <- w[, .(player_id, snapshot_date = as.Date(snapshot_date), val = get(vcol))]
  w <- w[!is.na(val)]
  w[, snap_actual := snapshot_date]
  m <- ms_star[, .(player_id, match_id, team_id, match_date, season_end_year, total_minutes)]
  m <- m[match_date > min(w$snapshot_date) & match_date <= max(w$snapshot_date) + 30L]
  m[, join_date := match_date - 1L]
  setkey(w, player_id, snapshot_date); setkey(m, player_id, join_date)
  j <- w[m, roll = TRUE]
  lag <- as.integer(j$match_date - j$snap_actual)
  stopifnot(min(lag, na.rm = TRUE) >= 1)
  j[, .(player_id, match_id, team_id, season_end_year, total_minutes, val)]
}

#' As-at value for a PER-GAME metric: expanding prior-form per-90 over the
#' player's earlier matches only. cumsum minus the current row is what makes it
#' strictly prior -- the current match can never contribute to its own predictor.
asat_from_gamelogs <- function(gl, vcol, min_prior_mins = 450) {
  if (!vcol %in% names(gl)) return(NULL)
  d <- gl[!is.na(get(vcol)), .(player_id, match_id, team_id, match_date,
                               season_end_year, total_minutes, v = get(vcol))]
  setorder(d, player_id, match_date)
  d[, `:=`(cv = cumsum(v) - v, cm = cumsum(as.numeric(total_minutes)) - as.numeric(total_minutes)),
    by = player_id]
  d[, val := fifelse(cm >= min_prior_mins, cv / (cm/90), NA_real_)]
  d[, .(player_id, match_id, team_id, season_end_year, total_minutes, val)]
}

report <- function(r) {
  if (isTRUE(r$skip)) { cat(sprintf("%-16s -- insufficient coverage (%.1f%% NA)\n",
                                    r$label, r$pct_na)); return(NULL) }
  flag <- if (any(r$pos < 0)) "  <<< INVERTED CELL" else ""
  cat(sprintf("%-16s global %7.3f | R2 %.4f | n %7s | %%NA %4.1f | GK/DEF sd %5.2f%s\n",
              r$label, r$global, r$r2, format(r$n, big.mark=","), r$pct_na, r$gk_def, flag))
  cat(sprintf("%-16s   %s\n", "",
              paste(sprintf("%s %7.2f", names(r$pos), r$pos), collapse = "  ")))
  data.table(metric = r$label, n = r$n, pct_na = round(r$pct_na,1),
             global = round(r$global,3), r2 = round(r$r2,4),
             gk_def_sd = round(r$gk_def,2),
             GK = r$pos[["GK"]], DEF = r$pos[["DEF"]],
             MID = r$pos[["MID"]], FWD = r$pos[["FWD"]])
}

out <- data.table()
cat("=== A. WEEKLY-SNAPSHOTTED RATINGS (as-at, roll to match_date - 1) ===\n")
for (spec in list(list(PSR_WEEKLY, "psr"), list(PSR_WEEKLY, "osr"), list(PSR_WEEKLY, "dsr"),
                  list(EPR_WEEKLY, "epr"), list(EPR_WEEKLY, "epr_offensive"),
                  list(EPR_WEEKLY, "epr_defensive"))) {
  if (!file.exists(spec[[1]])) { cat(sprintf("%-16s -- %s missing\n", spec[[2]],
                                             basename(spec[[1]]))); next }
  x <- asat_from_weekly(spec[[1]], spec[[2]])
  if (is.null(x)) next
  out <- rbind(out, report(calibrate(x, spec[[2]])), fill = TRUE)
}

cat("\n=== B. PER-GAME METRICS (as-at, expanding prior-form per 90) ===\n")
f <- list.files(GL_DIR, pattern = "^game_logs_20.*[.]parquet$", full.names = TRUE)
if (length(f) == 0) {
  cat("no game_logs_*.parquet -- run the predictions pipeline first\n")
} else {
  MCOLS <- c("psv","osv","dsv","epv_total","epv_total_adj","wpa_total",
             "panna","piero_value_p90","spm_overall")
  gl <- rbindlist(lapply(f, function(p) {
    d <- as.data.table(read_parquet(p))
    d[, intersect(c("player_id","match_id","team_id","match_date","season", MCOLS),
                  names(d)), with = FALSE]
  }), use.names = TRUE, fill = TRUE)
  gl[, match_date := as.Date(match_date)]
  gl[, season_end_year := as.integer(extract_season_end_year(season))]
  gl <- merge(gl, unique(ms_star[, .(match_id, player_id, total_minutes)]),
              by = c("match_id","player_id"))
  gl <- gl[!is.na(season_end_year) & !is.na(total_minutes) & total_minutes > 0]
  cat(sprintf("game-log starter rows: %s | vintage %s\n",
              format(nrow(gl), big.mark=","), format(max(file.mtime(f)), "%Y-%m-%d")))
  for (m in MCOLS) {
    x <- asat_from_gamelogs(gl, m)
    if (is.null(x)) next
    out <- rbind(out, report(calibrate(x, m)), fill = TRUE)
  }
}

cat("\n\n================= SUMMARY =================\n")
print(out)
inv <- out[GK < 0 | DEF < 0 | MID < 0 | FWD < 0]
if (nrow(inv) > 0) {
  cat("\n!!! INVERTED POSITION CELLS -- do not calibrate these until fixed !!!\n")
  cat("(RATING_CALIBRATION.md: a factor on a negative slope encodes the defect\n")
  cat(" as a legitimate-looking coefficient. See #209 / #210 / #226 / #227.)\n")
  print(inv[, .(metric, GK, DEF, MID, FWD)])
} else {
  cat("\nNo inverted position cells.\n")
}
