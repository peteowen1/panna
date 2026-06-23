# ONE-TIME migration: convert the live opta_psr_weekly from PRIMARY-LEAGUE offsets
# to the DECAY-WEIGHTED BLEND offsets (matching 08b's new method). A full 08b
# rebuild is ~4h (skill re-estimation), but the offset is END-ADD/additive, so we
# can migrate the existing parquet: strip the primary-league offset that was
# applied, then add the blend offset. Both are recomputed from the SAME match_stats
# + psr_offsets 08b used, so the strip is exact.
#
# Self-check: after stripping, a weak-league name (J. Randall / A_League) must pop
# back near the top (confirms the live parquet really had primary offsets); after
# adding the blend it must drop again. Aborts if the strip doesn't recover the
# inflated state (=> live parquet isn't primary-offset as assumed).
#
# Set TEST_ONLY <- TRUE to verify without writing.
suppressMessages({devtools::load_all(".", quiet = TRUE); library(arrow); library(data.table)})

TEST_ONLY <- if (exists("TEST_ONLY")) TEST_ONLY else TRUE
cache_dir <- file.path("data-raw", "cache-skills")
opta_dir  <- opta_data_dir()
weekly_path <- file.path(opta_dir, "opta_psr_weekly.parquet")

cat("=== Loading ===\n")
weekly <- as.data.table(read_parquet(weekly_path))
weekly[, snapshot_date := as.Date(snapshot_date)]
out_cols <- names(weekly)
offsets <- as.data.table(read_parquet(file.path(cache_dir, "psr_league_offsets.parquet")))

ms_path <- { p <- file.path(cache_dir, "01_match_stats_slim.rds")
             if (file.exists(p)) p else file.path(cache_dir, "01_match_stats.rds") }
ms <- as.data.table(readRDS(ms_path))
if (!inherits(ms$match_date, "Date")) ms[, match_date := as.Date(ms$match_date)]
lgcol <- if ("competition" %in% names(ms)) "competition" else "league"

# --- primary-league (what was applied) ---
pl_src <- ms[!is.na(get(lgcol)), .(player_id, .lg = get(lgcol), match_date, total_minutes)]
WIN <- 365L
.primary_asof <- function(d) {
  hist <- pl_src[match_date < d]; if (!nrow(hist)) return(NULL)
  pk <- function(x){ if(!nrow(x)) return(NULL); a<-x[,.(m=sum(total_minutes,na.rm=TRUE)),by=.(player_id,.lg)]; setorder(a,player_id,-m); a[,.(league=.lg[1L]),by=player_id] }
  r<-pk(hist[match_date>=d-WIN]); a<-pk(hist)
  if(is.null(a))return(r); if(is.null(r))return(a)
  o<-merge(a,r,by="player_id",all.x=TRUE,suffixes=c("_all","_recent")); o[,league:=fifelse(is.na(league_recent),league_all,league_recent)]; o[,.(player_id,league)]
}
off_lk <- offsets[, .(league, .off = offset)]

# --- decay-weighted blend (new) — match PSR skill recency ---
dp <- if (file.exists(file.path(cache_dir, "02b_decay_params.rds"))) readRDS(file.path(cache_dir,"02b_decay_params.rds")) else get_default_decay_params()
LAMBDA <- dp$rate
blend_src <- merge(pl_src, offsets[, .(.lg = league, .o = offset)], by = ".lg", all.x = TRUE)
blend_src[is.na(.o), .o := 0]
mm <- as.numeric(max(blend_src$match_date))
blend_src[, gfac := exp(-LAMBDA * (mm - as.numeric(match_date))) * total_minutes]
blend_src[, wo := gfac * .o]; setkey(blend_src, match_date)
.blend_asof <- function(d){ h<-blend_src[match_date<d]; if(!nrow(h)) return(NULL); h[,.(boff=sum(wo)/sum(gfac)),by=player_id] }

cat(sprintf("  weekly: %s rows, %d dates | lambda=%.4f (~%dd half-life)\n",
            format(nrow(weekly),big.mark=","), uniqueN(weekly$snapshot_date), LAMBDA, round(log(2)/LAMBDA)))

apply_one <- function(pdt, d) {
  prim <- .primary_asof(d)
  primoff <- if (!is.null(prim)) merge(prim, off_lk, by="league", all.x=TRUE)[is.na(.off), .off:=0][, .(player_id, primoff=.off)] else NULL
  bl <- .blend_asof(d)
  pdt <- merge(pdt, if(!is.null(primoff)) primoff else data.table(player_id=character(),primoff=numeric()), by="player_id", all.x=TRUE)
  pdt <- merge(pdt, if(!is.null(bl)) bl else data.table(player_id=character(),boff=numeric()), by="player_id", all.x=TRUE)
  pdt[is.na(primoff), primoff:=0][is.na(boff), boff:=0]
  pdt[, delta := boff - primoff]                       # strip primary, add blend
  pdt[, psr := psr + delta][, osr := osr + delta/2][, dsr := dsr + delta/2]
  pdt[, c("primoff","boff","delta") := NULL]
  pdt
}

# Self-check on latest date: strip-only should re-inflate Randall; full should drop him
dlate <- max(weekly$snapshot_date)
late <- copy(weekly[snapshot_date==dlate])
prim <- .primary_asof(dlate); primoff <- merge(prim, off_lk, by="league", all.x=TRUE)[is.na(.off),.off:=0][,.(player_id,primoff=.off)]
stripped <- merge(copy(late), primoff, by="player_id", all.x=TRUE)[is.na(primoff),primoff:=0]
stripped[, psr_free := psr - primoff]
rj_stripped_rank <- { setorder(stripped, -psr_free); which(grepl("Randall", stripped$player_name))[1] }
cat(sprintf("\nSELF-CHECK @ %s: J. Randall rank after STRIP (offset-free) = %s of %d\n",
            as.character(dlate), rj_stripped_rank, nrow(stripped)))
if (is.na(rj_stripped_rank) || rj_stripped_rank > 200) {
  stop("Strip did NOT re-inflate Randall (rank ", rj_stripped_rank, ") — live parquet is not primary-offset as assumed. ABORT.")
}
cat("  -> strip recovers the inflated offset-free state. Safe to migrate.\n")

cat("\n=== Applying blend across all dates ===\n")
dates <- sort(unique(weekly$snapshot_date))
res <- vector("list", length(dates))
for (i in seq_along(dates)) {
  res[[i]] <- apply_one(weekly[snapshot_date==dates[i]], dates[i])[, ..out_cols]
}
out <- rbindlist(res, use.names=TRUE); setorder(out, snapshot_date, player_id)

latest <- out[snapshot_date==max(snapshot_date)][order(-psr)]
cat(sprintf("\n=== Top 12 @ %s (blend offsets) ===\n", as.character(max(out$snapshot_date))))
print(latest[1:12, .(player_name, primary_position, psr=round(psr,3), weighted_90s=round(weighted_90s,1))])
rj <- latest[grepl("Randall", player_name)]
if (nrow(rj)) cat(sprintf("  J. Randall: psr=%.3f rank %d of %d\n", rj$psr[1], which(latest$player_id==rj$player_id[1])[1], nrow(latest)))

if (TEST_ONLY) { cat("\n[TEST_ONLY] not written.\n") } else {
  write_parquet(as.data.frame(out), weekly_path)
  cat(sprintf("\n=== WROTE %s (%s rows, %d dates) ===\n", weekly_path, format(nrow(out),big.mark=","), uniqueN(out$snapshot_date)))
}
