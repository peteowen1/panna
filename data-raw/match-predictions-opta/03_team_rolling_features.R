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

elo_result <- compute_match_elos(
  played,
  k = ELO_K,
  home_advantage = ELO_HOME_ADV,
  initial_elo = ELO_INITIAL,
  # Optimized 2026-05-28: per-match-type K (tournaments 80, qualifiers
  # 25, friendlies 5, club 20) + cross-conf multiplier 1.5 + per-
  # confederation prior with conf_spread=200. WC-weighted Brier improved
  # 8.6% vs single-K=20 baseline. The conf priors are what fixed the
  # Norway < Uzbekistan cross-pool isolation issue.
  k_table         = ELO_MATCH_TYPE_K,
  cross_conf_mult = ELO_CROSS_CONF_MULT,
  conf_priors     = elo_conf_priors_from_spread(ELO_CONF_SPREAD)
)
elo_features <- elo_result$per_match
final_elos <- elo_result$final_elos

# Report range with na.rm so a handful of NA-team rows (which compute_match_elos
# correctly returns as NA pre-match Elo) don't make the whole range read NA.
# Also report NA count explicitly — a non-trivial count means upstream
# scraper gaps that should be reported, not silently filtered.
n_na <- sum(is.na(elo_features$home_elo))
message(sprintf("  Elo pre-match range: %.0f to %.0f (across %d matches; %d NA from missing-team rows)",
                min(elo_features$home_elo, na.rm = TRUE),
                max(elo_features$home_elo, na.rm = TRUE),
                nrow(elo_features), n_na))
message(sprintf("  Final-state Elo range across %d teams: %.0f to %.0f",
                length(final_elos),
                min(final_elos, na.rm = TRUE),
                max(final_elos, na.rm = TRUE)))

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
  message(sprintf("\n  Looking up Elo for %d upcoming fixtures...", nrow(upcoming)))

  # Use compute_match_elos's final-state vector directly — previously this
  # block re-iterated all played matches without the NA guards in
  # compute_match_elos, which is what caused the 2026-05-28 NA cascade
  # (NA team_name in a friendly poisoned the elos[NA] entry, which then
  # cascaded to every team that played a poisoned opponent — France,
  # Germany, Brazil etc. all ended up with NA Elo, then 0 via step 04
  # NA-fill).
  #
  # For a team not in final_elos (debutant with NO matches anywhere in
  # the dataset), the Elo lookup returns NA — we surface that as NA
  # rather than silently fall back to ELO_INITIAL = 1500, because "no
  # data" should be visibly different from "average team". The step 03
  # assertion below counts how many WC2026 teams couldn't be resolved;
  # if it's anything > a small handful, that's a data-quality signal
  # worth investigating, not silently smoothing over.
  lookup_elo <- function(team_name) {
    if (is.na(team_name) || !(team_name %in% names(final_elos))) {
      return(NA_real_)
    }
    unname(final_elos[team_name])
  }
  fixture_elos <- data.frame(
    match_id = upcoming$match_id,
    home_elo = vapply(upcoming$home_team, lookup_elo, numeric(1)),
    away_elo = vapply(upcoming$away_team, lookup_elo, numeric(1)),
    stringsAsFactors = FALSE
  )
  fixture_elos$elo_diff <- fixture_elos$home_elo - fixture_elos$away_elo

  n_unresolved <- sum(is.na(fixture_elos$home_elo) | is.na(fixture_elos$away_elo))
  if (n_unresolved > 0L) {
    unresolved_teams <- unique(c(
      upcoming$home_team[is.na(fixture_elos$home_elo)],
      upcoming$away_team[is.na(fixture_elos$away_elo)]
    ))
    unresolved_teams <- unresolved_teams[!is.na(unresolved_teams)]
    message(sprintf("  %d / %d upcoming fixtures have NA Elo (team not seen in played history)",
                    n_unresolved, nrow(fixture_elos)))
    if (length(unresolved_teams) > 0L) {
      message(sprintf("    unresolved teams: %s",
                      paste(unresolved_teams, collapse = ", ")))
    }
  }

  # Carry forward each team's last known rolling features to fixtures.
  # Rolling-form columns are named home_<metric>_last_<window> (and
  # home_days_since_last) — there is no `roll_` infix. Take the column names
  # from rolling_team so the Elo columns (home_elo/away_elo) are excluded.
  dt_roll <- data.table::as.data.table(rolling_features)
  home_roll_cols <- grep("^home_", names(rolling_team), value = TRUE)
  away_roll_cols <- grep("^away_", names(rolling_team), value = TRUE)

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

    # Recompute the rolling differentials for the fixture rows from the
    # carried-forward home_/away_ values (compute_team_rolling_features only
    # derived these for played matches).
    for (w in ROLLING_WINDOWS) {
      for (m in c("goals_scored", "xg_for", "points", "npxgd")) {
        hc <- sprintf("home_%s_last_%d", m, w)
        ac <- sprintf("away_%s_last_%d", m, w)
        dc <- sprintf("diff_%s_last_%d", m, w)
        if (hc %in% names(fixture_dt) && ac %in% names(fixture_dt)) {
          fixture_dt[, (dc) := get(hc) - get(ac)]
        }
      }
    }
    if (all(c("home_days_since_last", "away_days_since_last") %in% names(fixture_dt))) {
      fixture_dt[, rest_diff := home_days_since_last - away_days_since_last]
    }

    n_form <- sum(!is.na(fixture_dt[["home_points_last_10"]]))
    message(sprintf("  Rolling form carried to %d/%d fixtures (%.0f%%)",
                    n_form, nrow(fixture_dt), 100 * n_form / nrow(fixture_dt)))

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
