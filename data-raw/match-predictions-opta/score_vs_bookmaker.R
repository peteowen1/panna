# score_vs_bookmaker.R — the match model against bookmaker closing prices (EPL)
#
# Why: the honest record (score_record.R) has only ever been compared with the
# league base rate. This puts the bookmaker's closing price beside it on the
# SAME matches, so we know how far the model is from the market before changing
# it. Bookmaker prices are a BENCHMARK here and must never become a model input.
#
# Hand-run, reads files only, publishes nothing.
#
# Usage (from panna/, via powershell Rscript because of arrow):
#   Rscript data-raw/match-predictions-opta/score_vs_bookmaker.R <in_dir> <odds_dir> <out_dir>
# <in_dir>   as for score_record.R: match_predictions.parquet, match_features.parquet,
#            predictions-locked.parquet
# <odds_dir> football-data.co.uk season folders, each holding E0_*.csv
#
# Bookmaker probability = 1/price for home, draw, away, divided by their sum
# (the simplest way to take out the bookmaker's margin). Two prices are scored:
# Pinnacle closing (PSCH/PSCD/PSCA) and the market-average closing (AvgCH/..).

suppressPackageStartupMessages({ library(data.table); library(arrow) })

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) stop("usage: score_vs_bookmaker.R <in_dir> <odds_dir> <out_dir>")
in_dir <- args[1]; odds_dir <- args[2]; out_dir <- args[3]
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 1. Load ----

mp <- as.data.table(read_parquet(file.path(in_dir, "match_predictions.parquet")))
mf <- as.data.table(read_parquet(file.path(in_dir, "match_features.parquet"),
                                 col_select = c("match_id", "split", "season_end_year",
                                                "home_goals", "away_goals")))
lk <- as.data.table(read_parquet(file.path(in_dir, "predictions-locked.parquet")))
mf <- unique(mf[!is.na(home_goals) & !is.na(away_goals)], by = "match_id")

epl <- merge(mp[league == "ENG" & status == "played",
                .(match_id, match_date = as.Date(substr(match_date, 1, 10)), home_team, away_team,
                  pH = prob_H, pD = prob_D, pA = prob_A)],
             mf[, .(match_id, split, season_end_year, hg = home_goals, ag = away_goals)],
             by = "match_id")
epl[, result := fifelse(hg > ag, "H", fifelse(hg < ag, "A", "D"))]
cat(sprintf("EPL played rows with a result: %d, dates %s to %s\n",
            nrow(epl), min(epl$match_date), max(epl$match_date)))
print(epl[, .N, by = .(season_end_year, split)][order(season_end_year, split)])

files <- list.files(odds_dir, pattern = "^E0.*[.]csv$", recursive = TRUE, full.names = TRUE)
odds <- rbindlist(lapply(files, function(f) {
  d <- fread(f, encoding = "UTF-8")
  d[, src := basename(dirname(f))]
  d
}), fill = TRUE)
odds <- odds[!is.na(FTHG)]
odds[, match_date := as.Date(Date, format = "%d/%m/%Y")]
cat(sprintf("Odds rows: %d from %d files, dates %s to %s\n", nrow(odds), length(files),
            min(odds$match_date), max(odds$match_date)))
print(odds[, .(n = .N, pinnacle_close = sum(!is.na(PSCH)), avg_close = sum(!is.na(AvgCH))), by = src])

# 2. Join on date + teams ----
# Team names differ ("Man United" v Opta's name). Learn the mapping from the
# data: pair rows on date + score, take each odds name's most common partner,
# then join on date + both mapped names and check the result agrees.

cand <- merge(odds[, .(match_date, fd_home = HomeTeam, fd_away = AwayTeam, hg = FTHG, ag = FTAG)],
              epl[, .(match_date, home_team, away_team, hg, ag)], by = c("match_date", "hg", "ag"),
              allow.cartesian = TRUE)
pairs <- rbind(cand[, .(fd = fd_home, opta = home_team)], cand[, .(fd = fd_away, opta = away_team)])
name_map <- pairs[, .N, by = .(fd, opta)][order(fd, -N)][, .SD[1], by = fd]
cat("Team name map (odds file -> Opta), with the number of matches that support it:\n")
print(name_map)
if (anyDuplicated(name_map$opta)) stop("two odds-file teams map to one Opta team")

odds <- merge(odds, name_map[, .(HomeTeam = fd, home_team = opta)], by = "HomeTeam", all.x = TRUE)
odds <- merge(odds, name_map[, .(AwayTeam = fd, away_team = opta)], by = "AwayTeam", all.x = TRUE)
devig <- function(h, d, a) { s <- 1 / h + 1 / d + 1 / a; list(1 / h / s, 1 / d / s, 1 / a / s) }
odds[, c("kH", "kD", "kA") := devig(PSCH, PSCD, PSCA)]
odds[, c("mH", "mD", "mA") := devig(AvgCH, AvgCD, AvgCA)]

j <- merge(epl, odds[, .(match_date, home_team, away_team, FTR, kH, kD, kA, mH, mD, mA,
                         margin_pin = 1 / PSCH + 1 / PSCD + 1 / PSCA - 1)],
           by = c("match_date", "home_team", "away_team"))
in_range <- epl[match_date >= min(odds$match_date) & match_date <= max(odds$match_date)]
cat(sprintf("Joined %d of %d EPL model rows inside the odds date range (%d odds rows)\n",
            nrow(j), nrow(in_range), nrow(odds)))
if (anyDuplicated(j$match_id)) stop("join duplicated a match")
if (any(j$FTR != j$result)) stop(sum(j$FTR != j$result), " joined rows disagree on the result")
cat(sprintf("Result agrees on all %d joined rows. Pinnacle margin: mean %.3f\n",
            nrow(j), mean(j$margin_pin, na.rm = TRUE)))

# 3. Score ----

base <- epl[split == "train", .(bH = mean(result == "H"), bD = mean(result == "D"), bA = mean(result == "A"))]
ll <- function(r, h, d, a) -log(pmax(fifelse(r == "H", h, fifelse(r == "A", a, d)), 1e-6))

score <- function(d, tier, book = c("k", "m")) {
  book <- match.arg(book)
  bh <- d[[paste0(book, "H")]]; bd <- d[[paste0(book, "D")]]; ba <- d[[paste0(book, "A")]]
  ok <- !is.na(bh)
  d <- d[ok]; bh <- bh[ok]; bd <- bd[ok]; ba <- ba[ok]
  if (nrow(d) == 0) return(NULL)
  m <- ll(d$result, d$pH, d$pD, d$pA); b <- ll(d$result, bh, bd, ba)
  data.table(tier = tier, n = nrow(d),
             model = round(mean(m), 4), bookmaker = round(mean(b), 4),
             base_rate = round(mean(ll(d$result, base$bH, base$bD, base$bA)), 4),
             model_minus_bookmaker = round(mean(m - b), 4),
             se = round(sd(m - b) / sqrt(nrow(d)), 4))
}

locked <- merge(j, lk[, .(match_id, locked_at, lock_source, lH = prob_H_pre, lD = prob_D_pre, lA = prob_A_pre)],
                by = "match_id")
locked_as_model <- copy(locked)[, `:=`(pH = lH, pD = lD, pA = lA)]
lead <- as.numeric(locked$match_date - as.Date(substr(locked$locked_at, 1, 10)))
cat(sprintf("Locked rows: %d. Days from lock to match day: median %g, range %g to %g
",
            nrow(locked), median(lead), min(lead), max(lead)))
print(locked[, .N, by = .(season_end_year, lock_source)])
# The lock file holds two kinds of row: forecasts frozen before match day
# (lock_source daily / history-seed) and walk-forward retrodictions, made
# afterwards by a model refitted only on earlier matches. They are scored apart.
locked_as_model[, lead := lead]
pre <- locked_as_model[lead > 0]
wf <- locked_as_model[lead <= 0]
if (nrow(wf) > 0 && any(wf$lock_source != "retrodiction")) stop("a non-retrodiction row was locked on or after match day")
# The published file carries no probabilities for train rows, so there is no train tier.
tiers <- function(book) rbindlist(list(
  score(pre, "locked before match day", book),
  score(locked[match_id %in% pre$match_id], "same matches, published row (recomputed afterwards)", book),
  score(wf, "walk-forward (refit on earlier matches only, made afterwards)", book),
  score(locked[match_id %in% wf$match_id], "same matches, published row (recomputed afterwards)", book)
))
t_pin <- tiers("k"); t_avg <- tiers("m")


cat("\nLog loss (how surprised the forecast was by what happened; lower is better).\n")
cat("model_minus_bookmaker above 0 means the bookmaker was better; se is its standard error.\n")
cat("\nAgainst Pinnacle closing prices:\n"); print(t_pin)
cat("\nAgainst the market-average closing price:\n"); print(t_avg)

fwrite(t_pin, file.path(out_dir, "vs_pinnacle_close.csv"))
fwrite(t_avg, file.path(out_dir, "vs_market_average_close.csv"))
fwrite(name_map, file.path(out_dir, "team_name_map.csv"))
