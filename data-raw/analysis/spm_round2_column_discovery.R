# spm_round2_column_discovery.R
# Discover unmapped Opta columns for SPM Round 2 feature expansion
#
# Compares all columns in raw Opta data against those already mapped
# in aggregate_opta_stats(), ranking unmapped columns by fill rate.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")

# 2. Load Raw Data ----

cat("\n=== Loading Processed Opta Data ===\n")
processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))
opta_stats <- processed_data$opta_stats

cat("Rows:", nrow(opta_stats), "\n")
cat("Columns:", ncol(opta_stats), "\n")
cat("Seasons:", paste(sort(unique(opta_stats$season)), collapse = ", "), "\n")

# 3. Get Currently Mapped Columns ----

mapped_opta_cols <- c(
  "minsPlayed", "goals", "goalsOpenplay", "totalScoringAtt",
  "attemptsIbox", "attemptsObox", "ontargetScoringAtt", "shotOffTarget",
  "blockedScoringAtt", "bigChanceCreated", "bigChanceScored", "bigChanceMissed",
  "goalAssist", "goalAssistOpenplay", "goalAssistSetplay", "secondGoalAssist",
  "totalAttAssist", "ontargetAttAssist", "totalPass", "accuratePass",
  "totalFinalThirdPasses", "successfulFinalThirdPasses", "totalLongBalls",
  "accurateLongBalls", "totalThroughBall", "accurateThroughBall",
  "totalCross", "accurateCross", "putThrough", "successfulPutThrough",
  "backwardPass", "fwdPass", "totalLayoffs", "accurateLayoffs",
  "totalTackle", "wonTackle", "interception", "interceptionWon",
  "interceptionsInBox", "totalClearance", "effectiveClearance",
  "headClearance", "effectiveHeadClearance", "outfielderBlock",
  "blockedPass", "blockedCross", "duelWon", "duelLost",
  "aerialWon", "aerialLost", "wonContest", "totalContest",
  "challengeLost", "touches", "touchesInOppBox", "possWonDef3rd",
  "possWonMid3rd", "possWonAtt3rd", "ballRecovery", "dispossessed",
  "turnover", "possLostAll", "timesTackled", "fouls", "wasFouled",
  "fouledFinalThird", "yellowCard", "redCard", "secondYellow",
  "cornerTaken", "wonCorners", "lostCorners", "totalCornersIntobox",
  "accurateCornersIntobox", "freekickCross", "saves", "savedIbox",
  "savedObox", "goalsConceded", "goalsConcededIbox", "totalHighClaim",
  "goodHighClaim", "punches", "keeperThrows", "accurateKeeperThrows",
  "totalFwdZonePass", "accurateFwdZonePass", "openPlayPass",
  "successfulOpenPlayPass", "errorLeadToShot", "errorLeadToGoal",
  "attFastbreak", "shotFastbreak", "attOpenplay", "attSetpiece",
  "attHdTotal", "attHdGoal", "attOneOnOne", "totalCrossNocorner",
  "accurateCrossNocorner", "penaltyWon", "penaltyConceded",
  "offtargetAttAssist", "lastManTackle", "sixYardBlock",
  "clearanceOffLine", "totalKeeperSweeper", "accurateKeeperSweeper",
  "attemptsConcededIbox", "attemptsConcededObox", "gkSmother",
  "unsuccessfulTouch", "overrun", "totalFlickOn", "accurateFlickOn",
  "totalOffside", "offsideProvoked", "penAreaEntries",
  "finalThirdEntries", "totalPullBack", "accuratePullBack"
)

# Non-stat columns (metadata)
meta_cols <- c("match_id", "player_name", "team_name", "position",
               "season", "league", "match_date", "home_team", "away_team",
               "team_position", "sub_on_minute", "sub_off_minute",
               "appearances", "is_starter", "is_home")

# 4. Find Unmapped Columns ----

all_cols <- names(opta_stats)
unmapped <- setdiff(all_cols, c(mapped_opta_cols, meta_cols))

cat("\n=== Column Summary ===\n")
cat("Total columns:", length(all_cols), "\n")
cat("Mapped to features:", length(intersect(mapped_opta_cols, all_cols)), "\n")
cat("Metadata columns:", length(intersect(meta_cols, all_cols)), "\n")
cat("UNMAPPED columns:", length(unmapped), "\n")

# 5. Analyze Unmapped Columns ----

cat("\n=== Unmapped Column Analysis ===\n")

results <- lapply(unmapped, function(col) {
  vals <- opta_stats[[col]]
  is_num <- is.numeric(vals)

  if (is_num) {
    non_zero <- sum(vals != 0 & !is.na(vals))
    non_na <- sum(!is.na(vals))
    fill_rate <- non_zero / nrow(opta_stats)
    mean_nz <- if (non_zero > 0) mean(vals[vals != 0 & !is.na(vals)], na.rm = TRUE) else 0
    max_val <- if (non_zero > 0) max(vals[vals != 0 & !is.na(vals)], na.rm = TRUE) else 0
  } else {
    non_zero <- sum(!is.na(vals) & vals != "")
    non_na <- non_zero
    fill_rate <- non_zero / nrow(opta_stats)
    mean_nz <- NA
    max_val <- NA
  }

  data.frame(
    column = col,
    is_numeric = is_num,
    non_zero = non_zero,
    non_na = non_na,
    fill_rate = round(fill_rate, 4),
    mean_nonzero = round(mean_nz, 3),
    max_value = max_val,
    stringsAsFactors = FALSE
  )
})

results_df <- do.call(rbind, results)
results_df <- results_df[order(-results_df$fill_rate), ]

cat("\nAll unmapped columns ranked by fill rate:\n\n")
print(results_df, row.names = FALSE, right = FALSE)

# 6. Check Fill Rate by Season for Key Candidates ----

cat("\n\n=== Fill Rate by Season (Tier 1 + 2 candidates) ===\n")

tier_candidates <- c(
  "totalDribble", "totalSuccessfulDribble", "dribbleLastMan",
  "totalAttProgRun", "attPenGoal", "attPenMiss",
  "attIboxGoal", "attOboxGoal", "hitWoodwork", "touchesInOwnBox",
  "keyPassLong", "keyPassShort", "keyPassCross",
  "keyPassThroughball", "keyPassCorner",
  "totalChippedPass", "accurateChippedPass",
  "attIboxTarget", "attOboxTarget",
  "attRfTotal", "attLfTotal",
  "keeperPickup",
  "totalBackZonePass", "accurateBackZonePass",
  "penGoalsConceded"
)

tier_found <- intersect(tier_candidates, all_cols)
tier_missing <- setdiff(tier_candidates, all_cols)

if (length(tier_missing) > 0) {
  cat("\nCandidate columns NOT in data:", paste(tier_missing, collapse = ", "), "\n")
}

cat("\nCandidate columns found:", length(tier_found), "/", length(tier_candidates), "\n\n")

for (col in tier_found) {
  vals <- opta_stats[[col]]
  if (!is.numeric(vals)) {
    cat(sprintf("  %-30s  NOT NUMERIC\n", col))
    next
  }

  seasons <- sort(unique(opta_stats$season))
  season_rates <- sapply(seasons, function(s) {
    idx <- opta_stats$season == s
    sum(vals[idx] != 0 & !is.na(vals[idx])) / sum(idx)
  })

  overall_rate <- sum(vals != 0 & !is.na(vals)) / length(vals)
  cat(sprintf("  %-30s  overall=%.3f  ", col, overall_rate))

  # Show first/last season with data
  has_data <- which(season_rates > 0.01)
  if (length(has_data) > 0) {
    cat(sprintf("first=%s  last=%s", seasons[min(has_data)], seasons[max(has_data)]))
    # Flag if not available early
    if (min(has_data) > 3) cat("  **LATE START**")
  } else {
    cat("NO DATA")
  }
  cat("\n")
}

cat("\n=== Discovery Complete ===\n")
