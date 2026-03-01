# 03_team_rolling_features.R
# Compute rolling form features, Elo ratings, and rest days per team
#
# All rolling features are strictly lagged (no future data leakage).
# Uses data.table frollmean + shift for efficient rolling calculations.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
output_path <- file.path(cache_dir, "03_rolling_features.rds")

ELO_K <- 20
ELO_HOME_ADV <- 65
ELO_INITIAL <- 1500
ROLLING_WINDOWS <- c(5L, 10L, 20L)

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 03_rolling_features.rds")
  rolling_features <- readRDS(output_path)
  message(sprintf("  %d matches with rolling features", nrow(rolling_features)))
  return(invisible(NULL))
}

# 4. Load Data ----

message("\n=== Computing Rolling Features ===\n")

fixture_results <- readRDS(file.path(cache_dir, "01_fixture_results.rds"))
played <- fixture_results[fixture_results$match_status == "Played", ]
played <- played[order(played$match_date), ]

message(sprintf("  %d played matches to process", nrow(played)))

# 5. Compute Elo Ratings ----

message("\n  Computing Elo ratings...")

elo_features <- compute_match_elos(
  played,
  k = ELO_K,
  home_advantage = ELO_HOME_ADV,
  initial_elo = ELO_INITIAL
)

message(sprintf("  Elo range: %.0f to %.0f", min(elo_features$home_elo), max(elo_features$home_elo)))

# 6. Compute Rolling Features ----

message("  Computing rolling team features...")

# Fill missing xG with per-league average xG (less noisy than actual goals)
for (lg in unique(played$league)) {
  lg_idx <- played$league == lg
  lg_home_mean <- mean(played$home_xg[lg_idx], na.rm = TRUE)
  lg_away_mean <- mean(played$away_xg[lg_idx], na.rm = TRUE)
  # If entire league has no xG, fall back to global mean
  if (is.na(lg_home_mean)) lg_home_mean <- mean(played$home_xg, na.rm = TRUE)
  if (is.na(lg_away_mean)) lg_away_mean <- mean(played$away_xg, na.rm = TRUE)
  na_home <- lg_idx & is.na(played$home_xg)
  na_away <- lg_idx & is.na(played$away_xg)
  played$home_xg[na_home] <- lg_home_mean
  played$away_xg[na_away] <- lg_away_mean
}

rolling_team <- compute_team_rolling_features(played, windows = ROLLING_WINDOWS)

message(sprintf("  Rolling features: %d matches, %d columns",
                nrow(rolling_team), ncol(rolling_team) - 1))

# 7. Combine ----

rolling_features <- merge(elo_features, rolling_team, by = "match_id", all.x = TRUE)

# 8. Add Elo for Upcoming Fixtures ----

upcoming <- fixture_results[fixture_results$match_status != "Played", ]
if (nrow(upcoming) > 0) {
  message(sprintf("\n  Computing Elo for %d upcoming fixtures...", nrow(upcoming)))

  # Get final Elo state after all played matches
  all_teams <- unique(c(played$home_team, played$away_team))
  elos <- init_team_elos(all_teams, ELO_INITIAL)

  # Replay all played matches to get current Elo
  for (i in seq_len(nrow(played))) {
    ht <- played$home_team[i]
    at <- played$away_team[i]
    if (ht %in% names(elos) && at %in% names(elos)) {
      updated <- update_elo(elos[ht], elos[at],
                            played$home_goals[i], played$away_goals[i],
                            k = ELO_K, home_advantage = ELO_HOME_ADV)
      elos[ht] <- updated$new_home_elo
      elos[at] <- updated$new_away_elo
    }
  }

  # Create Elo features for fixtures
  fixture_elos <- data.frame(
    match_id = upcoming$match_id,
    home_elo = vapply(upcoming$home_team, function(t) {
      if (t %in% names(elos)) elos[t] else ELO_INITIAL
    }, numeric(1)),
    away_elo = vapply(upcoming$away_team, function(t) {
      if (t %in% names(elos)) elos[t] else ELO_INITIAL
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
  fixture_elos$elo_diff <- fixture_elos$home_elo - fixture_elos$away_elo

  # Carry forward each team's last known rolling features to fixtures
  dt_roll <- data.table::as.data.table(rolling_features)
  home_roll_cols <- grep("^home_roll_", names(dt_roll), value = TRUE)
  away_roll_cols <- grep("^away_roll_", names(dt_roll), value = TRUE)

  if (length(home_roll_cols) > 0) {
    dt_played <- data.table::as.data.table(played)
    fixture_dt <- data.table::as.data.table(fixture_elos)
    fixture_dt[, home_team := upcoming$home_team]
    fixture_dt[, away_team := upcoming$away_team]

    # Last home-side rolling features per team (from their most recent home match)
    home_lookup <- merge(dt_played[, .(match_id, home_team, match_date)],
                         dt_roll[, c("match_id", home_roll_cols), with = FALSE],
                         by = "match_id")
    data.table::setorder(home_lookup, match_date)
    home_lookup <- home_lookup[, .SD[.N], by = home_team]

    # Last away-side rolling features per team (from their most recent away match)
    away_lookup <- merge(dt_played[, .(match_id, away_team, match_date)],
                         dt_roll[, c("match_id", away_roll_cols), with = FALSE],
                         by = "match_id")
    data.table::setorder(away_lookup, match_date)
    away_lookup <- away_lookup[, .SD[.N], by = away_team]

    # Join onto fixtures by team name
    fixture_dt <- merge(fixture_dt,
                        home_lookup[, c("home_team", home_roll_cols), with = FALSE],
                        by = "home_team", all.x = TRUE)
    fixture_dt <- merge(fixture_dt,
                        away_lookup[, c("away_team", away_roll_cols), with = FALSE],
                        by = "away_team", all.x = TRUE)
    fixture_dt[, c("home_team", "away_team") := NULL]
    fixture_elos <- fixture_dt
  }

  # Append fixture rows to rolling_features
  rolling_features <- data.table::rbindlist(
    list(rolling_features, fixture_elos),
    use.names = TRUE, fill = TRUE
  )
}

# 9. Save ----

data.table::setDF(rolling_features)
saveRDS(rolling_features, output_path)

# 10. Summary ----

message("\n========================================")
message("Rolling features complete!")
message("========================================")
message(sprintf("Total matches: %d", nrow(rolling_features)))
message(sprintf("Features: %d columns", ncol(rolling_features)))
message(sprintf("Saved to: %s", output_path))
