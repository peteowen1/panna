## 99_sense_check_ratings.R -----------------------------------------------
## DIAGNOSTIC, not a pipeline step. Numbered 99 so it is obviously terminal:
## nothing consumes its output, and `run_skills_pipeline.R` does not source it.
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
## Every one of those is caught by splitting a number by position or by league
## and looking at a named player. That is all this file does.
##
## Run the SAME battery over every season-level rating, so metrics that already
## have league/opponent adjustment get audited too rather than being assumed fine.
##
## For each metric:
##   A. coverage           -- n rated, % of match participants with no value
##   B. position spread    -- sd by position, GK/DEF ratio (the anchor that caught PSR)
##   C. league spread      -- mean by league, range, and Spearman vs the PSR offsets
##   D. CALIBRATION        -- leak-free S-1 -> S, starters only, minutes-weighted
##                            SUM, own-minus-opponent. Target slope = 1.00, both
##                            overall and per position.
##   E. top-20 anchor      -- position mix and league mix
devtools::load_all("."); suppressPackageStartupMessages(library(data.table))

sk <- readRDS("data-raw/cache-skills/06_seasonal_ratings.rds")
ms <- as.data.table(readRDS("data-raw/cache-skills/01_match_stats.rds"))
ms[, season_end_year := as.integer(extract_season_end_year(season))]
ms <- ms[!is.na(season_end_year) & !is.na(total_minutes) & total_minutes > 0]
ms_star <- ms[!is.na(position) & position != "Substitute" & position != ""]

lgmap <- ms[, .(mins = sum(as.numeric(total_minutes))),
            by = .(player_id, season_end_year, competition)][order(-mins)][
              , .SD[1L], by = .(player_id, season_end_year)]
setnames(lgmap, "competition", "league")
pos_ref <- unique(as.data.table(sk$seasonal_psr)[, .(player_id, season_end_year, primary_position)])
psr_off <- fread("data-raw/cache-skills/psr_league_offsets.csv")[, .(league, psr_offset = offset)]
BIG5 <- c("EPL","La_Liga","Serie_A","Bundesliga","Ligue_1","UCL")

METRICS <- list(
  list(tag="spm",    tbl=as.data.table(sk$seasonal_spm),   col="spm"),
  list(tag="rapm",   tbl=as.data.table(sk$seasonal_rapm),  col="rapm"),
  list(tag="xrapm",  tbl=as.data.table(sk$seasonal_xrapm), col="xrapm"),
  list(tag="psr",    tbl=as.data.table(sk$seasonal_psr),   col="psr"),
  list(tag="osr",    tbl=as.data.table(sk$seasonal_psr),   col="osr"),
  list(tag="dsr",    tbl=as.data.table(sk$seasonal_psr),   col="dsr")
)

## D. leak-free calibration: season S-1 rating predicting season S goal difference
calibrate <- function(d, vcol) {
  look <- d[!is.na(get(vcol)), .(player_id, lookup_season = season_end_year + 1L,
                                 val = get(vcol))]
  x <- merge(ms_star, look, by.x = c("player_id","season_end_year"),
             by.y = c("player_id","lookup_season"), all.x = TRUE)
  x <- merge(x, pos_ref, by = c("player_id","season_end_year"), all.x = TRUE)
  x[, pos_grp := fcase(primary_position=="GK","GK", primary_position=="DEF","DEF",
                       primary_position=="MID","MID", primary_position=="FWD","FWD",
                       default = NA_character_)]
  x <- x[!is.na(pos_grp)]
  x[, has := !is.na(val)]
  full <- x[, .(n=.N, ok=sum(has)), by=.(match_id, team_id)][ok==n, .(match_id, team_id)]
  x <- merge(x, full, by = c("match_id","team_id"))
  if (nrow(x) < 5000) return(NULL)
  x[, mins90 := as.numeric(total_minutes)/90]
  tm <- dcast(x[, .(v = sum(val*mins90)), by=.(match_id, team_id, pos_grp)],
              match_id + team_id ~ pos_grp, value.var="v", fill=0)
  pg <- setdiff(names(tm), c("match_id","team_id"))
  mo <- unique(x[, .(match_id, team_id, home_score, away_score, is_home)])[
          , .SD[1L], by=.(match_id, team_id)]
  tm <- merge(tm, mo, by=c("match_id","team_id"))
  n2 <- tm[, .N, by=match_id][N==2, match_id]; t2 <- tm[match_id %in% n2]
  if (nrow(t2) < 2000) return(NULL)
  op <- copy(t2)[, c("match_id","team_id",pg), with=FALSE]
  setnames(op, setdiff(names(op),"match_id"), paste0("o_", setdiff(names(op),"match_id")))
  g <- merge(t2, op, by="match_id", allow.cartesian=TRUE)[team_id != o_team_id]
  g[, gd := fifelse(as.logical(is_home), home_score-away_score, away_score-home_score)]
  for (p in pg) g[, paste0("d_",p) := get(p) - get(paste0("o_",p))]
  dc <- paste0("d_", intersect(c("GK","DEF","MID","FWD"), pg))
  g[, vdiff := rowSums(.SD), .SDcols = dc]
  m0 <- lm(gd ~ vdiff, data = g)
  m1 <- lm(reformulate(dc, "gd"), data = g)
  co <- summary(m1)$coefficients
  list(n = nrow(g), global = unname(coef(m0)["vdiff"]),
       ci = confint(m0)["vdiff",],
       pos = setNames(round(co[dc,"Estimate"], 3), sub("^d_","",dc)))
}

out <- data.table()
for (M in METRICS) {
  d <- M$tbl; v <- M$col
  if (!v %in% names(d)) next
  d <- merge(d, lgmap[, .(player_id, season_end_year, league)],
             by = c("player_id","season_end_year"), all.x = TRUE)
  if (!"primary_position" %in% names(d))
    d <- merge(d, pos_ref, by = c("player_id","season_end_year"), all.x = TRUE)
  cat(sprintf("\n################ %s ################\n", toupper(M$tag)))

  ## A. coverage
  have <- unique(d[!is.na(get(v)), .(player_id, season_end_year)])[, hv := TRUE]
  part <- unique(ms[, .(player_id, season_end_year)])
  part <- merge(part, have, by = c("player_id","season_end_year"), all.x = TRUE)
  pct_na <- 100*mean(is.na(part$hv))
  cat(sprintf("A. coverage: %s rated | %.1f%% of match participants have NO value\n",
              format(sum(!is.na(d[[v]])), big.mark=","), pct_na))

  ## B. position spread
  dp <- d[!is.na(primary_position) & total_minutes >= 900 & !is.na(get(v))]
  ps <- dp[, .(n=.N, sd=sd(get(v))), by=primary_position][order(-sd)]
  gkdef <- if (all(c("GK","DEF") %in% ps$primary_position))
    ps[primary_position=="GK"]$sd / ps[primary_position=="DEF"]$sd else NA_real_
  cat(sprintf("B. position sd: %s | GK/DEF ratio = %.2f %s\n",
              paste(sprintf("%s %.4f", ps$primary_position, ps$sd), collapse="  "),
              gkdef, if (!is.na(gkdef) && gkdef > 1.4) "<- GK OVER-SPREAD" else ""))

  ## C. league spread
  lgm <- d[!is.na(league) & !is.na(get(v)), .(n=.N, mean=mean(get(v))), by=league][n>=200]
  lgm <- merge(lgm, psr_off, by="league", all.x=TRUE)
  rho <- suppressWarnings(cor(lgm[!is.na(psr_offset)]$mean,
                              lgm[!is.na(psr_offset)]$psr_offset, method="spearman"))
  cat(sprintf("C. league mean range = %.4f (%s .. %s) | rho vs PSR offsets = %.3f\n",
              max(lgm$mean)-min(lgm$mean), lgm[which.min(mean)]$league,
              lgm[which.max(mean)]$league, rho))

  ## D. calibration
  cal <- calibrate(d, v)
  if (is.null(cal)) {
    cat("D. calibration: insufficient coverage to fit\n")
  } else {
    cat(sprintf("D. calibration slope (target 1.00): %.3f [%.3f, %.3f]  n=%s\n",
                cal$global, cal$ci[1], cal$ci[2], format(cal$n, big.mark=",")))
    cat(sprintf("   by position: %s\n",
                paste(sprintf("%s %.2f", names(cal$pos), cal$pos), collapse="  ")))
  }

  ## E. top-20 anchor
  top <- d[total_minutes >= 900 & !is.na(get(v))][order(-get(v))][1:20]
  gk <- sum(top$primary_position == "GK", na.rm=TRUE)
  b5 <- sum(top$league %in% BIG5, na.rm=TRUE)
  cat(sprintf("E. top-20: GK=%d %s | big5/UCL=%d %s\n", gk,
              if (gk <= 2) "PASS" else "FAIL", b5, if (b5 >= 15) "PASS" else "FAIL"))
  cat(sprintf("   top5: %s\n", paste(sprintf("%s(%s)", top$player_name[1:5],
                                             top$primary_position[1:5]), collapse=", ")))

  out <- rbind(out, data.table(metric = M$tag, pct_na = round(pct_na,1),
                               gk_def_sd = round(gkdef,2),
                               league_range = round(max(lgm$mean)-min(lgm$mean),4),
                               rho_vs_offsets = round(rho,3),
                               calib = if (is.null(cal)) NA_real_ else round(cal$global,3),
                               top20_gk = gk, top20_big5 = b5), fill = TRUE)
}
cat("\n\n================= SUMMARY =================\n")
print(out)
