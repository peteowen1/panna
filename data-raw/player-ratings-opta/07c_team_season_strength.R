## 07c_team_season_strength.R ---------------------------------------------
## Team-season offensive/defensive strength: the minute-weighted aggregate of
## player xRAPM for everyone who played for a team in a season.
##
## This is the OPPONENT-QUALITY control consumed by:
##   - data-raw/estimated-skills/07_train_psr_model.R   (home/away_def_rating)
##   - data-raw/match-predictions-opta/build_epr_weekly.R (opp_def_rating)
##
## Replaces debug/keep/sketch_team_strength.R, which belonged to no pipeline and
## therefore went stale (last run 2026-05-19) and carried three defects that
## together killed the control outright in eight non-European competitions
## (panna#224). Each is fixed here and each has an assertion below:
##
##   1. It keyed on `team_name`. Club names differ between the lineups feed and
##      the match feed, so MLS/Liga MX/Saudi/Argentina matched at ~0%. We key on
##      `team_id` (verified: 100% of match_stats team_ids appear in lineups).
##   2. It derived season_end_year from the match DATE (`month >= 7 -> year+1`).
##      That is the European convention and is wrong for calendar-year leagues:
##      an MLS match on 2025-07-27 became season 2026. It mis-seasoned 100% of
##      Leagues Cup, 69.6% of Brazil, 50.1% of MLS and 36.7% of Argentina. We
##      take the season from the `season` LABEL via extract_season_end_year().
##   3. It zero-filled unrated players (`mins_pts[is.na(get(c)), (c) := 0]`),
##      so a team whose players were mostly unrated aggregated to ~0 — which is
##      why 80.4% of the old file was exactly zero. Unrated players now get the
##      season's REPLACEMENT-LEVEL rating, which is a real prior rather than
##      "league average".
##
## Sign convention (inherited from xRAPM, unchanged): offense positive = good,
## defense NEGATIVE = good. Consumers use def_rating as-is.
##
## Output: cache-opta/team_season_strength.parquet
##   team_id, team_name, season_end_year, off_rating, def_rating,
##   total_mins, n_players, n_rated, mins_rated, coverage
##
## `coverage` is the share of a team-season's minutes played by RATED players.
## Consumers must gate on it -- a team-season built entirely from the prior
## carries no opponent information, and if that is true of every team in a
## league then the control is constant there and the league-season fixed effect
## absorbs it, silently. That is the exact failure mode of panna#224.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

if (!exists("MIN_TEAM_SEASON_MINS")) MIN_TEAM_SEASON_MINS <- 900   # >= 10 full games
if (!exists("LOW_COVERAGE_WARN"))    LOW_COVERAGE_WARN    <- 0.30

cache_opta <- file.path("data-raw", "cache-opta")
ratings_path <- file.path(cache_opta, "07_seasonal_ratings.rds")
lineups_path <- file.path("..", "pannadata", "data", "opta", "opta_lineups.parquet")

stopifnot(file.exists(ratings_path), file.exists(lineups_path))
cat(sprintf("ratings: %s (%s)\n", ratings_path,
            format(file.mtime(ratings_path), "%Y-%m-%d")))

## --- 1. Player-season ratings -------------------------------------------
ratings <- data.table::as.data.table(readRDS(ratings_path)$seasonal_xrapm)
stopifnot(all(c("player_id", "season_end_year", "offense", "defense") %in% names(ratings)))

## The synthetic "replacement" row is the prior for unrated players, not a
## player. Pull it out BEFORE the join so it can never be aggregated as one.
replacement <- ratings[player_id == "replacement",
                       .(season_end_year, rep_off = offense, rep_def = defense)]
ratings <- ratings[player_id != "replacement"]
cat(sprintf("player-seasons: %s | players: %s | seasons %d..%d\n",
            format(nrow(ratings), big.mark = ","),
            format(uniqueN(ratings$player_id), big.mark = ","),
            min(ratings$season_end_year), max(ratings$season_end_year)))
cat(sprintf("replacement-level prior available for %d seasons\n", nrow(replacement)))
stopifnot(nrow(replacement) > 0)

## Global fallback for a season with no replacement row of its own (e.g. a
## season the RAPM has not reached). Median, not mean -- one bad season should
## not move it.
rep_off_global <- stats::median(replacement$rep_off, na.rm = TRUE)
rep_def_global <- stats::median(replacement$rep_def, na.rm = TRUE)

## --- 2. Minutes by (player, team_id, season) ----------------------------
lu <- data.table::as.data.table(arrow::read_parquet(lineups_path))
stopifnot(all(c("player_id", "team_id", "team_name", "season", "minutes_played")
              %in% names(lu)))

## DEFECT 2 FIX: season from the LABEL, never from the match date.
lu[, season_end_year := as.integer(extract_season_end_year(season))]
n_bad_season <- sum(is.na(lu$season_end_year))
if (n_bad_season > 0) {
  cli::cli_warn(paste("extract_season_end_year() returned NA for {n_bad_season}",
                      "lineup rows; they are dropped from team strength."))
  lu <- lu[!is.na(season_end_year)]
}

pts <- lu[, .(mins = sum(as.numeric(minutes_played), na.rm = TRUE)),
           by = .(player_id, team_id, team_name, season_end_year)]
pts <- pts[mins > 0]
cat(sprintf("player-team-seasons with minutes: %s\n", format(nrow(pts), big.mark = ",")))

## --- 3. Attach ratings; unrated players fall back to the season prior ----
pts <- merge(pts, ratings[, .(player_id, season_end_year, offense, defense)],
              by = c("player_id", "season_end_year"), all.x = TRUE)
pts[, is_rated := !is.na(offense) & !is.na(defense)]

pts <- merge(pts, replacement, by = "season_end_year", all.x = TRUE)
## DEFECT 3 FIX: replacement-level prior, NOT zero. Zero is "league average",
## which is a materially better player than an unrated one actually is.
pts[is_rated == FALSE, offense := data.table::fcoalesce(rep_off, rep_off_global)]
pts[is_rated == FALSE, defense := data.table::fcoalesce(rep_def, rep_def_global)]
pts[, c("rep_off", "rep_def") := NULL]

stopifnot(!anyNA(pts$offense), !anyNA(pts$defense))
cat(sprintf("player-team-seasons rated: %.1f%% | minutes rated: %.1f%%\n",
            100 * mean(pts$is_rated),
            100 * sum(pts[is_rated == TRUE]$mins) / sum(pts$mins)))

## --- 4. Team-season aggregate, keyed on team_id -------------------------
## DEFECT 1 FIX: group by team_id. team_name is carried for readability only,
## taken as the name attached to the most minutes in that team-season.
team_strength <- pts[, .(
  off_rating = sum(offense * mins) / sum(mins),
  def_rating = sum(defense * mins) / sum(mins),
  total_mins = sum(mins),
  n_players  = .N,
  n_rated    = sum(is_rated),
  mins_rated = sum(fifelse(is_rated, mins, 0)),
  team_name  = team_name[which.max(mins)]
), by = .(team_id, season_end_year)]

team_strength[, coverage := mins_rated / total_mins]
team_strength <- team_strength[total_mins >= MIN_TEAM_SEASON_MINS]
data.table::setcolorder(team_strength,
  c("team_id", "team_name", "season_end_year", "off_rating", "def_rating",
    "total_mins", "n_players", "n_rated", "mins_rated", "coverage"))

cat(sprintf("\nteam-seasons: %s | median coverage %.3f | >=70%% covered: %.1f%%\n",
            format(nrow(team_strength), big.mark = ","),
            stats::median(team_strength$coverage),
            100 * mean(team_strength$coverage >= 0.7)))

## --- 5. Assertions: the three defects must not be able to come back -----
## No NAs anywhere. Unrated players get a prior, so a rating is always defined.
stopifnot(!anyNA(team_strength$off_rating), !anyNA(team_strength$def_rating))

## The key must be unique, or every consumer's merge silently row-multiplies.
stopifnot(!anyDuplicated(team_strength, by = c("team_id", "season_end_year")))

## The old file was 80.4% exactly-zero. Anything close to that means the
## rating join has collapsed again.
pct_zero <- mean(team_strength$off_rating == 0 & team_strength$def_rating == 0)
cat(sprintf("rows exactly zero on both ratings: %.2f%% (old file: 80.40%%)\n",
            100 * pct_zero))
stopifnot(pct_zero < 0.05)

## --- 6. Coverage by competition — the check that would have caught #224 --
## Within-league variance is what the model can actually use: league-season
## fixed effects absorb any constant, so a competition whose control has no
## within-league spread gets NO opponent adjustment while its peers do.
comp <- unique(lu[, .(team_id, season_end_year, competition)])
comp <- merge(team_strength, comp, by = c("team_id", "season_end_year"))
by_comp <- comp[, .(team_seasons = .N,
                    median_coverage = round(stats::median(coverage), 3),
                    sd_def = round(stats::sd(def_rating), 5)),
                by = competition][order(sd_def)]
cat("\n=== within-competition spread of def_rating (0 => control is dead) ===\n")
print(utils::head(by_comp, 20))

dead <- by_comp[sd_def < 1e-3 & team_seasons >= 20]
if (nrow(dead) > 0) {
  cli::cli_warn(paste(
    "{nrow(dead)} competition(s) have near-zero within-league spread in",
    "def_rating: {paste(dead$competition, collapse = ', ')}. The opponent",
    "control is INERT there (league-season FE absorbs a constant). See panna#224."))
} else {
  cat("\nOK: no competition has a degenerate opponent control.\n")
}

## --- 7. Write ------------------------------------------------------------
out_path <- file.path(cache_opta, "team_season_strength.parquet")
arrow::write_parquet(team_strength, out_path)
cat(sprintf("\nSaved: %s (%s rows)\n", out_path,
            format(nrow(team_strength), big.mark = ",")))
