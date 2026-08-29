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

ELO_K <- 20                    # fallback for any league NOT in ELO_MATCH_TYPE_K
ELO_HOME_ADV <- 88             # v6 optimized (was 65 pre-2026-05-29; package constant in R/elo_calibration.R)
ELO_INITIAL <- 1500            # fallback when a team has no confederation in conf_priors
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
  # v6 OPTIMIZED 2026-05-29 (DEoptim, 3-fold CV, 3-way logloss + Davidson
  # draw + venue factor) on the expanded intl corpus (WC 2022 Qatar +
  # AFCON 2023 + many historical WCQ cycles for AFC/CAF/CONMEBOL).
  # Best CV-mean logloss = 0.9782, -3.49% vs v4 seed. Decay halflife
  # converged to ~7000d (effectively off; the expanded recent-data
  # set carries enough signal without explicit weighting).
  #
  # Major changes vs v3/v4:
  #   - K_wc dropped from 94 to 44 (better prior reduces per-match swing)
  #   - K_continental dropped from 110 to 50
  #   - K_qualifier raised from 25 to 59 (more qualifier data = more signal)
  #   - K_friendly raised from 5 to 15
  #   - cross_conf_mult raised from 1.5 to 2.49 (rare but high-info matches)
  #   - home_advantage raised from 65 to 88
  #   - conf priors switched from parametric spread to per-conf deltas
  #   - use_venue_factor=TRUE (was FALSE) — neutral tournament matches now
  #     correctly get 0 home advantage instead of arbitrary +HA to whichever
  #     team Opta listed as "home"
  k_table          = ELO_MATCH_TYPE_K,
  cross_conf_mult  = ELO_CROSS_CONF_MULT,
  conf_priors      = ELO_CONFEDERATION_PRIORS,
  use_venue_factor = TRUE
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
  # id_rename_map (panna#204-sibling fix): non-empty only when a real
  # team-name collision was found (e.g. Arsenal EPL vs Arsenal Argentina) --
  # final_elos then carries the disambiguated name for every identity but
  # the most-established one, so a plain-name lookup here must resolve
  # through the same map before searching final_elos.
  id_rename_map <- elo_result$id_rename_map
  lookup_elo <- function(team_name, team_id) {
    if (length(id_rename_map) > 0 && !is.na(team_id) && nzchar(team_id) &&
        team_id %in% names(id_rename_map)) {
      team_name <- id_rename_map[[team_id]]
    }
    if (is.na(team_name) || !(team_name %in% names(final_elos))) {
      return(NA_real_)
    }
    unname(final_elos[team_name])
  }
  fixture_elos <- data.frame(
    match_id = upcoming$match_id,
    home_elo = mapply(lookup_elo, upcoming$home_team, upcoming$home_team_id),
    away_elo = mapply(lookup_elo, upcoming$away_team, upcoming$away_team_id),
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
